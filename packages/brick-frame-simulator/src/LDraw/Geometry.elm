module LDraw.Geometry exposing (ConditionalEdge, DetailOptions, FlatGeometry, Options, StudDetail(..), defaultDetailOptions, flatten, flattenWith, flattenWithDetail, localOptions, toDetailOptions, worldOptions)

{-| Flatten an LDraw part tree into WebGL-ready vertex buffers.

Takes a parsed list of `LDrawLine` values plus a populated `PartCache` and
recursively resolves sub-file references, accumulating the 4×4 transform
matrix and the current color at each level.


## Coordinate conversion

LDraw uses a right-handed coordinate system where **-Y is up**. With
`worldOptions` this module negates Y and Z on all output positions so that the
geometry is Y-up, matching WebGL convention.

With `localOptions` the conversion is skipped and geometry stays in LDraw
space. That is what the instanced renderer wants: a part is baked **once** in
its own local space, and the Y-up conversion is folded into each placement's
model matrix instead (see `Render.Instanced`). `[x, -y, -z]` has determinant
+1 — it is a rotation, not a reflection — so skipping it does not change
triangle winding.


## Winding and BFC

Sub-file transforms with a negative determinant invert triangle winding, as
does a `0 BFC INVERTNEXT` directive. Both are tracked while walking the tree
and cancel each other out when combined. Triangles are emitted with their
vertex order corrected, so all output geometry has consistent counter-clockwise
outward winding and back-face culling is safe on BFC-certified parts.


## Output

`flatten` returns a `FlatGeometry` record containing:

  - `triangles` — a flat list of `(Vertex, Vertex, Vertex)` tuples ready for
    `WebGL.triangles`.
  - `lines` — pairs of positions for edge rendering via `WebGL.lines`.
  - `bfcCertified` — whether the top-level file declared BFC CCW winding.
    When true, back-face culling can safely be enabled.

@docs ConditionalEdge, DetailOptions, FlatGeometry, Options, StudDetail, defaultDetailOptions, flatten, flattenWith, flattenWithDetail, localOptions, toDetailOptions, worldOptions

-}

import Dict
import LDraw.Colors as Colors
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4
import Render.Mesh exposing (Vertex)
import String



-- ── Types ─────────────────────────────────────────────────────────────────────


{-| Result of flattening a LDraw part tree.
-}
type alias FlatGeometry =
    { triangles : List ( Vertex, Vertex, Vertex )
    , lines : List ( Vec3, Vec3 )
    , conditionalLines : List ConditionalEdge
    , bfcCertified : Bool
    }


type alias ConditionalEdge =
    { p1 : Vec3
    , p2 : Vec3
    , c1 : Vec3
    , c2 : Vec3
    }


{-| How positions are mapped on the way out of the flattener.

`flipYZ` negates Y and Z to convert LDraw space to the Y-up convention WebGL
uses. Use `worldOptions` when the result is drawn directly, `localOptions` when
the result is a reusable per-part mesh and the conversion belongs in the
placement matrix instead.

`trackInherit` records, per vertex, whether its colour came all the way down an
unbroken chain of code-16 references, so a single baked mesh can be re-coloured
per placement (see `Render.Mesh.Vertex`). Directly rendered geometry leaves it
off and gets fully resolved colours.

-}
type alias Options =
    { flipYZ : Bool
    , trackInherit : Bool
    }


{-| Level of geometric detail to emit for LDraw top stud primitives (`stud.dat`,
`stud2.dat`, `stud2a.dat`, `stud10.dat`, `stud15.dat`, etc.):

  - `FullStuds` — expand the catalog stud subfile at full detail.
  - `BoxStuds` — replace each top stud with a 10-triangle box approximation
    (open underside omitted), preserving bounding box and per-instance colour.
  - `NoStuds` — omit top studs completely.

-}
type StudDetail
    = FullStuds
    | BoxStuds
    | NoStuds


{-| Flattening options with explicit geometry detail controls.

  - `flipYZ` — negate output Y and Z to convert LDraw (`-Y` up) to WebGL (`+Y` up).
  - `trackInherit` — mark colour-16 vertices with `inherit = 1.0`.
  - `studs` — detail level for top stud primitives (`FullStuds`, `BoxStuds`, `NoStuds`).
  - `includeHollows` — when `False`, skips `stud3*` and `stud4*` underside hollows/tubes.
  - `includeLines` — when `False`, skips type-2 edge lines.
  - `includeConditionals` — when `False`, skips type-5 conditional lines.

-}
type alias DetailOptions =
    { flipYZ : Bool
    , trackInherit : Bool
    , studs : StudDetail
    , includeHollows : Bool
    , includeLines : Bool
    , includeConditionals : Bool
    }


{-| Convert to Y-up on output, with colours fully resolved. The default for
directly rendered geometry.
-}
worldOptions : Options
worldOptions =
    { flipYZ = True, trackInherit = False }


{-| Keep LDraw space on output and flag inherited colours, for per-part meshes
reused across placements and colours.
-}
localOptions : Options
localOptions =
    { flipYZ = False, trackInherit = True }


{-| Full-detail Y-up world options (all studs, hollows, edge lines, and
conditional lines included).
-}
defaultDetailOptions : DetailOptions
defaultDetailOptions =
    toDetailOptions worldOptions


{-| Upgrade simple `Options` into `DetailOptions` with full geometric detail.
-}
toDetailOptions : Options -> DetailOptions
toDetailOptions options =
    { flipYZ = options.flipYZ
    , trackInherit = options.trackInherit
    , studs = FullStuds
    , includeHollows = True
    , includeLines = True
    , includeConditionals = True
    }



-- ── Public API ────────────────────────────────────────────────────────────────


{-| Flatten a parsed LDraw file into renderable Y-up geometry.

    flatten lines cache parentColor worldTransform

  - `lines` — the parsed top-level file content.
  - `cache` — part cache (all sub-files must already be in `Loaded` state).
    Sub-files absent from the cache are silently skipped.
  - `parentColor` — the inherited color for resolving color code 16.
    Pass `15` (white) for the root call.
  - `worldTransform` — the initial transformation matrix.
    Pass `Mat4.identity` for the root call.

-}
flatten : List LDrawLine -> PartCache -> Int -> Mat4 -> FlatGeometry
flatten =
    flattenWith worldOptions


{-| `flatten` with control over the output coordinate convention.

    flattenWith localOptions partLines cache color Mat4.identity

-}
flattenWith : Options -> List LDrawLine -> PartCache -> Int -> Mat4 -> FlatGeometry
flattenWith options =
    flattenWithDetail (toDetailOptions options)


{-| Flatten a parsed LDraw file with explicit control over coordinate
conversion, colour inheritance tracking, stud detail, underside hollows, edge
lines, and conditional lines.
-}
flattenWithDetail : DetailOptions -> List LDrawLine -> PartCache -> Int -> Mat4 -> FlatGeometry
flattenWithDetail options lines cache parentColor worldTransform =
    let
        bfc =
            hasBfcCertify lines

        result =
            flattenLines options
                lines
                cache
                parentColor
                worldTransform
                { flipped = False, inheriting = True }
                { triangles = [], lines = [], conditionalLines = [], bfcCertified = False }
    in
    { result | bfcCertified = bfc }



-- ── Internal ──────────────────────────────────────────────────────────────────


{-| Whether the file declares BFC CCW certification.
-}
hasBfcCertify : List LDrawLine -> Bool
hasBfcCertify lines =
    List.any isBfcCertify lines


isBfcCertify : LDrawLine -> Bool
isBfcCertify line =
    case line of
        Comment text ->
            String.contains "BFC CERTIFY CCW" text

        _ ->
            False


{-| State carried down the tree, as opposed to accumulated across it.

  - `flipped` — winding inverted by enclosing sub-file transforms.
  - `inheriting` — no level so far has fixed a concrete colour, so geometry here
    still takes the caller's colour.

-}
type alias Inherited =
    { flipped : Bool
    , inheriting : Bool
    }


{-| Accumulate geometry by walking the line list.

The `Bool` threaded through the fold is `invertNext`: a pending
`0 BFC INVERTNEXT` that applies to the next sub-file reference only.

-}
flattenLines :
    DetailOptions
    -> List LDrawLine
    -> PartCache
    -> Int
    -> Mat4
    -> Inherited
    -> FlatGeometry
    -> FlatGeometry
flattenLines options lines cache parentColor transform inherited acc =
    List.foldl (flattenLine options cache parentColor transform inherited) ( acc, False ) lines
        |> Tuple.first


flattenLine :
    DetailOptions
    -> PartCache
    -> Int
    -> Mat4
    -> Inherited
    -> LDrawLine
    -> ( FlatGeometry, Bool )
    -> ( FlatGeometry, Bool )
flattenLine options cache parentColor transform inherited line ( acc, invertNext ) =
    let
        flipped =
            inherited.flipped
    in
    case line of
        SubFileRef ref ->
            let
                base =
                    baseFileName ref.file

                inheritsHere =
                    ref.color == 16 || ref.color == -1

                childColor =
                    if inheritsHere then
                        parentColor

                    else
                        ref.color

                -- A negative-determinant local transform flips winding, and so
                -- does INVERTNEXT; the two cancel when they occur together.
                childFlipped =
                    xor flipped (xor invertNext (determinant3 ref.transform < 0))

                childTransform =
                    Mat4.mul transform ref.transform

                childInherited =
                    { flipped = childFlipped
                    , inheriting = inherited.inheriting && inheritsHere
                    }
            in
            if not options.includeHollows && isHollowSubfile base then
                ( acc, False )

            else if isTopStudSubfile base && options.studs /= FullStuds then
                case options.studs of
                    NoStuds ->
                        ( acc, False )

                    BoxStuds ->
                        let
                            studColor =
                                surfaceColor options inherited parentColor ref.color
                        in
                        ( { acc
                            | triangles =
                                emitBoxStud options childTransform childFlipped studColor acc.triangles
                          }
                        , False
                        )

                    FullStuds ->
                        ( acc, False )

            else
                case Dict.get ref.file cache of
                    Just (Loaded subLines) ->
                        ( flattenLines options
                            subLines
                            cache
                            childColor
                            childTransform
                            childInherited
                            acc
                        , False
                        )

                    _ ->
                        -- Not loaded or failed — skip silently
                        ( acc, False )

        Triangle tri ->
            let
                p1 =
                    transformPoint options transform tri.p1

                p2 =
                    transformPoint options transform tri.p2

                p3 =
                    transformPoint options transform tri.p3
            in
            ( { acc
                | triangles =
                    windTriangle flipped (surfaceColor options inherited parentColor tri.color) p1 p2 p3
                        :: acc.triangles
              }
            , False
            )

        Quad quad ->
            -- Split quad into two triangles along the p1–p3 diagonal
            let
                color =
                    surfaceColor options inherited parentColor quad.color

                p1 =
                    transformPoint options transform quad.p1

                p2 =
                    transformPoint options transform quad.p2

                p3 =
                    transformPoint options transform quad.p3

                p4 =
                    transformPoint options transform quad.p4
            in
            ( { acc
                | triangles =
                    windTriangle flipped color p1 p2 p3
                        :: windTriangle flipped color p1 p3 p4
                        :: acc.triangles
              }
            , False
            )

        LineSegment seg ->
            if options.includeLines then
                ( { acc
                    | lines =
                        ( transformPoint options transform seg.p1
                        , transformPoint options transform seg.p2
                        )
                            :: acc.lines
                  }
                , False
                )

            else
                ( acc, False )

        Comment text ->
            -- Only BFC INVERTNEXT produces state; other comments leave a
            -- pending invert alone rather than clearing it.
            ( acc
            , if String.contains "BFC INVERTNEXT" text then
                True

              else
                invertNext
            )

        ConditionalLine cond ->
            if options.includeConditionals then
                ( { acc
                    | conditionalLines =
                        { p1 = transformPoint options transform cond.p1
                        , p2 = transformPoint options transform cond.p2
                        , c1 = transformPoint options transform cond.c1
                        , c2 = transformPoint options transform cond.c2
                        }
                            :: acc.conditionalLines
                  }
                , False
                )

            else
                ( acc, False )


baseFileName : String -> String
baseFileName raw =
    let
        norm =
            raw
                |> String.toLower
                |> String.replace "\\" "/"
    in
    case List.reverse (String.split "/" norm) of
        last :: _ ->
            last

        [] ->
            norm


isHollowSubfile : String -> Bool
isHollowSubfile base =
    String.startsWith "stud3" base || String.startsWith "stud4" base


isTopStudSubfile : String -> Bool
isTopStudSubfile base =
    String.startsWith "stud" base
        && not (isHollowSubfile base)
        && not (String.startsWith "studel" base)
        && not (String.startsWith "studtube" base)
        && not (String.startsWith "studholder" base)


{-| Emit a 10-triangle box approximation of a standard LDraw stud primitive
(top quad plus four side quads; bottom face at `y = 0` omitted).
-}
emitBoxStud :
    DetailOptions
    -> Mat4
    -> Bool
    -> ( Vec4.Vec4, Float )
    -> List ( Vertex, Vertex, Vertex )
    -> List ( Vertex, Vertex, Vertex )
emitBoxStud options transform flipped color accTriangles =
    let
        pt =
            transformPoint options transform

        tNW =
            pt (vec3 -6 -4 -6)

        tNE =
            pt (vec3 6 -4 -6)

        tSE =
            pt (vec3 6 -4 6)

        tSW =
            pt (vec3 -6 -4 6)

        bNW =
            pt (vec3 -6 0 -6)

        bNE =
            pt (vec3 6 0 -6)

        bSE =
            pt (vec3 6 0 6)

        bSW =
            pt (vec3 -6 0 6)

        addQuad a b c d tris =
            windTriangle flipped color a b c
                :: windTriangle flipped color a c d
                :: tris
    in
    accTriangles
        |> addQuad tSW tNW tNE tSE
        |> addQuad tNW bNW bNE tNE
        |> addQuad tSE bSE bSW tSW
        |> addQuad tNE bNE bSE tSE
        |> addQuad tSW bSW bNW tNW


{-| Resolve a surface's colour, and note whether it is still inheriting.

When `trackInherit` is on, a surface whose colour came down an unbroken code-16
chain is marked so the shader can substitute the placement's colour. The
resolved colour is still written to the vertex, so a mesh drawn without an
`instanceColor` override looks exactly as it did before.

-}
surfaceColor : DetailOptions -> Inherited -> Int -> Int -> ( Vec4.Vec4, Float )
surfaceColor options inherited parentColor lineColor =
    ( Colors.toVec4 (Colors.resolveColor parentColor lineColor)
    , if options.trackInherit && inherited.inheriting && (lineColor == 16 || lineColor == -1) then
        1.0

      else
        0.0
    )


{-| Emit a triangle with counter-clockwise outward winding, swapping the last
two vertices when the accumulated transform chain has inverted it.
-}
windTriangle : Bool -> ( Vec4.Vec4, Float ) -> Vec3 -> Vec3 -> Vec3 -> ( Vertex, Vertex, Vertex )
windTriangle flipped ( color, inherit ) p1 p2 p3 =
    let
        ( a, b, c ) =
            if flipped then
                ( p1, p3, p2 )

            else
                ( p1, p2, p3 )

        normal =
            faceNormal a b c

        mk position =
            { position = position, normal = normal, color = color, inherit = inherit }
    in
    ( mk a, mk b, mk c )


{-| Determinant of the upper-left 3×3 block, i.e. of the rotation/scale part.
Negative means the transform reflects and therefore inverts triangle winding.
-}
determinant3 : Mat4 -> Float
determinant3 mat =
    let
        m =
            Mat4.toRecord mat
    in
    (m.m11 * ((m.m22 * m.m33) - (m.m23 * m.m32)))
        - (m.m12 * ((m.m21 * m.m33) - (m.m23 * m.m31)))
        + (m.m13 * ((m.m21 * m.m32) - (m.m22 * m.m31)))


{-| Apply the accumulated transform to a point.

With `flipYZ` set, negates Y and Z to convert LDraw (X-right, Y-down, Z-away) →
WebGL right-handed (X-right, Y-up, Z-toward-viewer).

-}
transformPoint : DetailOptions -> Mat4 -> Vec3 -> Vec3
transformPoint options mat p =
    let
        tp =
            Mat4.transform mat p
    in
    if options.flipYZ then
        vec3 (Vec3.getX tp) -(Vec3.getY tp) -(Vec3.getZ tp)

    else
        tp


{-| Compute a face normal from three points (CCW winding assumed).
Returns the zero vector for degenerate triangles; shaders handle this gracefully.
-}
faceNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
faceNormal p1 p2 p3 =
    let
        edge1 =
            Vec3.sub p2 p1

        edge2 =
            Vec3.sub p3 p1

        cross =
            Vec3.cross edge1 edge2

        len =
            Vec3.length cross
    in
    if len < 1.0e-8 then
        vec3 0 1 0

    else
        Vec3.scale (1.0 / len) cross
