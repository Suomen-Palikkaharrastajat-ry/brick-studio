module Game.BrickProps exposing
    ( PropModel
    , View
    , arePartsBfcCertified
    , build
    , draw
    , fitBox
    , fitHeight
    , fitLength
    , fitWidth
    , progress
    , refresh
    , size
    , surfaceSettings
    )

{-| Brick-built props (trees, fences, buildings, buoys, ...) for the Brick
Frame games: LDraw models made of catalog parts (see `Game.BrickModels`),
each turned into geometry **once** and then drawn as many times as the scene
needs, each time with its own model matrix.

Loading works like any build: `progress` says which part files a model still
needs (fetch them with `Game.PartLoading.fetchParts`), and once all are in
the cache `build` flattens the model through `LDraw.Geometry` and uploads it
with `Render.Scene.buildScene`. Parts in colour 16 stay flagged, so `draw`
can paint them in a tint (a buoy's colour, a debris brick's colour) without
another copy of the geometry.

`draw` uses a plastic shader like the cars' and boats' with the game's
distance fog, so far props fade into the sky like the ground around them.

-}

import Dict exposing (Dict)
import Game.PartLoading as PartLoading exposing (PartProgress)
import LDraw.Document as Document
import LDraw.Geometry as Geometry
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4)
import Render.EdgeShader as EdgeShader
import Render.Mesh exposing (Vertex)
import Render.Scene as Scene exposing (Scene)
import Set exposing (Set)
import WebGL
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


{-| A prop ready to draw: its scene in Y-up LDraw units and its bounds
there (studs included).
-}
type alias PropModel =
    { scene : Scene
    , boundsMin : Vec3
    , boundsMax : Vec3
    , triangleCount : Int
    }


{-| Part files a model still needs, and how far the rest has loaded.
-}
progress : String -> PartCache -> PartProgress
progress ldraw cache =
    PartLoading.progress (Resolve.pendingParts (Parser.parseFile ldraw) Dict.empty) Document.empty cache


{-| Build every model of `sources` (name, LDraw text) that is not built yet
and whose parts have all arrived, and list the part files the others still
need. The returned cache has those marked as loading: fetch them with
`Game.PartLoading.fetchParts` and call this again after each one arrives.
-}
refresh : List ( String, String ) -> PartCache -> Dict String PropModel -> { props : Dict String PropModel, cache : PartCache, fetch : List String }
refresh sources cache props =
    let
        states =
            sources
                |> List.filter (\( name, _ ) -> not (Dict.member name props))
                |> List.map (\( name, ldraw ) -> ( name, ldraw, progress ldraw cache ))

        built =
            List.foldl
                (\( name, ldraw, p ) acc ->
                    if List.isEmpty p.needed && p.inFlight == 0 then
                        Dict.insert name (build ldraw cache) acc

                    else
                        acc
                )
                props
                states

        needed =
            states
                |> List.concatMap (\( _, _, p ) -> p.needed)
                |> PartLoading.uniqueNames
    in
    { props = built
    , cache = Resolve.markLoading needed cache
    , fetch = needed
    }


{-| Flatten a model whose parts are all in the cache. Missing parts are
skipped, as everywhere in the brick pipeline.
-}
build : String -> PartCache -> PropModel
build ldraw cache =
    let
        parsed =
            Parser.parseFile ldraw

        geom =
            Geometry.flattenWith { flipYZ = True, trackInherit = True } parsed cache 16 Mat4.identity

        bfcCertified =
            fileBfcCertified parsed
                && Tuple.first (linesSubfilesBfc cache Set.empty parsed Dict.empty)

        grow p acc =
            let
                r =
                    Vec3.toRecord p
            in
            { minX = min acc.minX r.x
            , minY = min acc.minY r.y
            , minZ = min acc.minZ r.z
            , maxX = max acc.maxX r.x
            , maxY = max acc.maxY r.y
            , maxZ = max acc.maxZ r.z
            }

        b =
            List.foldl (\( v1, v2, v3 ) acc -> acc |> grow v1.position |> grow v2.position |> grow v3.position)
                { minX = 1.0e9, minY = 1.0e9, minZ = 1.0e9, maxX = -1.0e9, maxY = -1.0e9, maxZ = -1.0e9 }
                geom.triangles

        ( boundsMin, boundsMax ) =
            if b.minX > b.maxX then
                ( vec3 0 0 0, vec3 0 0 0 )

            else
                ( vec3 b.minX b.minY b.minZ, vec3 b.maxX b.maxY b.maxZ )
    in
    { scene =
        Scene.buildScene
            { triangles = geom.triangles
            , lines = geom.lines
            , conditionalLines = []
            }
            bfcCertified
    , boundsMin = boundsMin
    , boundsMax = boundsMax
    , triangleCount = List.length geom.triangles
    }


{-| Whether every part in `partNames` and all of its transitive subfiles in
`cache` are loaded and BFC CCW certified without disabling or inverting
certification (`BFC CERTIFY CW`, `BFC NOCERTIFY`, `BFC NOCLIP`).
-}
arePartsBfcCertified : PartCache -> List String -> Bool
arePartsBfcCertified cache partNames =
    List.foldl
        (\name ( ok, memo ) ->
            if not ok then
                ( False, memo )

            else
                subfileBfc cache Set.empty name memo
        )
        ( True, Dict.empty )
        partNames
        |> Tuple.first


fileBfcCertified : List LDrawLine -> Bool
fileBfcCertified lines =
    let
        step line ( hasCcw, clean ) =
            case line of
                Comment text ->
                    if
                        String.contains "BFC CERTIFY CW" text
                            || String.contains "BFC NOCERTIFY" text
                            || String.contains "BFC NOCLIP" text
                    then
                        ( hasCcw, False )

                    else if String.contains "BFC CERTIFY CCW" text then
                        ( True, clean )

                    else
                        ( hasCcw, clean )

                _ ->
                    ( hasCcw, clean )

        ( ccw, ok ) =
            List.foldl step ( False, True ) lines
    in
    ccw && ok


linesSubfilesBfc : PartCache -> Set String -> List LDrawLine -> Dict String Bool -> ( Bool, Dict String Bool )
linesSubfilesBfc cache visiting lines memo =
    List.foldl
        (\line ( ok, accMemo ) ->
            if not ok then
                ( False, accMemo )

            else
                case line of
                    SubFileRef ref ->
                        subfileBfc cache visiting ref.file accMemo

                    _ ->
                        ( True, accMemo )
        )
        ( True, memo )
        lines


subfileBfc : PartCache -> Set String -> String -> Dict String Bool -> ( Bool, Dict String Bool )
subfileBfc cache visiting name memo =
    if Set.member name visiting then
        ( True, memo )

    else
        case Dict.get name memo of
            Just cached ->
                ( cached, memo )

            Nothing ->
                case Dict.get name cache of
                    Just (Loaded subLines) ->
                        if fileBfcCertified subLines then
                            let
                                ( childrenOk, nextMemo ) =
                                    linesSubfilesBfc cache (Set.insert name visiting) subLines memo
                            in
                            ( childrenOk, Dict.insert name childrenOk nextMemo )

                        else
                            ( False, Dict.insert name False memo )

                    _ ->
                        ( False, Dict.insert name False memo )


{-| WebGL surface settings for a triangle mesh: adds back-face culling (or
front-face culling when the model matrix has a negative determinant) whenever
the scene is `bfcCertified`.
-}
surfaceSettings : Bool -> Mat4 -> List WebGL.Settings.Setting
surfaceSettings bfcCertified modelMatrix =
    if not bfcCertified then
        [ DepthTest.default ]

    else if determinant3 modelMatrix < 0 then
        [ DepthTest.default, WebGL.Settings.cullFace WebGL.Settings.front ]

    else
        [ DepthTest.default, WebGL.Settings.cullFace WebGL.Settings.back ]


determinant3 : Mat4 -> Float
determinant3 mat =
    let
        m =
            Mat4.toRecord mat
    in
    (m.m11 * ((m.m22 * m.m33) - (m.m23 * m.m32)))
        - (m.m12 * ((m.m21 * m.m33) - (m.m23 * m.m31)))
        + (m.m13 * ((m.m21 * m.m32) - (m.m22 * m.m31)))


{-| Width (X), height (Y) and depth (Z) of a prop, in LDraw units.
-}
size : PropModel -> Vec3
size prop =
    Vec3.sub prop.boundsMax prop.boundsMin


{-| Moves the prop so the middle of its base is at the origin.
-}
base : PropModel -> Mat4
base prop =
    let
        lo =
            Vec3.toRecord prop.boundsMin

        hi =
            Vec3.toRecord prop.boundsMax
    in
    Mat4.makeTranslate3 (-(lo.x + hi.x) / 2) -lo.y (-(lo.z + hi.z) / 2)


uniformFit : Float -> Float -> PropModel -> Mat4
uniformFit target extent prop =
    let
        s =
            target / max 1.0e-6 extent
    in
    Mat4.mul (Mat4.makeScale3 s s s) (base prop)


{-| Base on the origin, scaled evenly to `metres` tall.
-}
fitHeight : Float -> PropModel -> Mat4
fitHeight metres prop =
    uniformFit metres (Vec3.getY (size prop)) prop


{-| Base on the origin, scaled evenly to `metres` across (X).
-}
fitWidth : Float -> PropModel -> Mat4
fitWidth metres prop =
    uniformFit metres (Vec3.getX (size prop)) prop


{-| Base on the origin, scaled evenly to `metres` long (Z).
-}
fitLength : Float -> PropModel -> Mat4
fitLength metres prop =
    uniformFit metres (Vec3.getZ (size prop)) prop


{-| Base on the origin, stretched to a box of `width` x `height` x `depth`
metres.
-}
fitBox : { width : Float, height : Float, depth : Float } -> PropModel -> Mat4
fitBox box prop =
    let
        s =
            Vec3.toRecord (size prop)
    in
    Mat4.mul (Mat4.makeScale3 (box.width / max 1.0e-6 s.x) (box.height / max 1.0e-6 s.y) (box.depth / max 1.0e-6 s.z)) (base prop)



-- DRAWING


{-| What every prop of one frame shares: the camera, the sun and the fog
(`fogMax` of the fog colour from `fogNear` to `fogFar` metres, smoothly),
and the canvas size for the edge lines.
-}
type alias View =
    { view : Mat4
    , projection : Mat4
    , eye : Vec3
    , sunDir : Vec3
    , fogColor : Vec3
    , fogNear : Float
    , fogFar : Float
    , fogMax : Float
    , width : Int
    , height : Int
    }


type alias Uniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    , viewPosition : Vec3
    , lightDirection : Vec3
    , tint : Vec4
    , fogColor : Vec3
    , fogNear : Float
    , fogFar : Float
    , fogMax : Float
    }


{-| Draw a prop with a model matrix. `tint` paints its colour-16 parts.
`edgeWidth` is the width of the brick edge lines in pixels: pass 0 for props
too far away for the lines to read.
-}
draw : View -> Mat4 -> Vec4 -> Float -> PropModel -> List WebGL.Entity
draw v modelMatrix tint edgeWidth prop =
    let
        surface =
            WebGL.entityWith (surfaceSettings prop.scene.bfcCertified modelMatrix)
                vertexShader
                fragmentShader
                prop.scene.triangleMesh
                { modelMatrix = modelMatrix
                , viewMatrix = v.view
                , projectionMatrix = v.projection
                , viewPosition = v.eye
                , lightDirection = v.sunDir
                , tint = tint
                , fogColor = v.fogColor
                , fogNear = v.fogNear
                , fogFar = v.fogFar
                , fogMax = v.fogMax
                }
    in
    if edgeWidth > 0.3 then
        [ surface
        , WebGL.entityWith
            [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
            , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
            ]
            EdgeShader.vertexShader
            EdgeShader.fragmentShader
            prop.scene.lineMesh
            { modelMatrix = modelMatrix
            , viewMatrix = v.view
            , projectionMatrix = v.projection
            , edgeColor = vec3 0.12 0.13 0.16
            , viewportWidth = toFloat v.width
            , viewportHeight = toFloat v.height
            , lineWidth = edgeWidth
            }
        ]

    else
        [ surface ]


type alias Varyings =
    { vColor : Vec4, vNormal : Vec3, vWorld : Vec3 }


vertexShader : WebGL.Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec4 color;
        attribute float inherit;

        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;
        uniform vec4 tint;

        varying vec4 vColor;
        varying vec3 vNormal;
        varying vec3 vWorld;

        void main() {
            vec4 world = modelMatrix * vec4(position, 1.0);
            gl_Position = projectionMatrix * viewMatrix * world;
            vColor = mix(color, tint, inherit);
            vNormal = mat3(modelMatrix) * normal;
            vWorld = world.xyz;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 viewPosition;
        uniform vec3 lightDirection;
        uniform vec3 fogColor;
        uniform float fogNear;
        uniform float fogFar;
        uniform float fogMax;

        varying vec4 vColor;
        varying vec3 vNormal;
        varying vec3 vWorld;

        void main() {
            vec3 n = normalize(vNormal);
            vec3 viewDir = normalize(viewPosition - vWorld);
            // Light whichever side faces the viewer, so stretched or mirrored
            // props never come out black.
            if (dot(n, viewDir) < 0.0) { n = -n; }
            vec3 l = normalize(lightDirection);

            float hemi = n.y * 0.5 + 0.5;
            vec3 amb = mix(vec3(0.66, 0.6, 0.55), vec3(0.95, 0.98, 1.05), hemi) * 0.62;
            float diff = max(0.0, (dot(n, l) + 0.12) / 1.12);
            vec3 halfDir = normalize(l + viewDir);
            float spec = pow(max(dot(n, halfDir), 0.0), 24.0) * 0.16;
            float rim = pow(1.0 - max(dot(n, viewDir), 0.0), 2.5) * 0.08;

            vec3 col = vColor.rgb * (amb + vec3(1.02, 0.99, 0.95) * 0.5 * diff) + vec3(spec + rim);
            float dist = length(vWorld - viewPosition);
            float fog = smoothstep(fogNear, fogFar, dist) * fogMax;
            gl_FragColor = vec4(mix(clamp(col, 0.0, 1.0), fogColor, fog), 1.0);
        }
    |]
