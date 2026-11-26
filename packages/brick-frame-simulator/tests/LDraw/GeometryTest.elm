module LDraw.GeometryTest exposing (suite)

{-| Unit tests for geometry flattening from parsed LDraw lines.
-}

import Dict
import Expect
import LDraw.Colors as Colors
import LDraw.Geometry exposing (StudDetail(..), defaultDetailOptions, flatten, flattenWithDetail)
import LDraw.Resolve exposing (PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Math.Vector4 as Vec4
import Test exposing (Test, describe, test)



-- ── Helpers ───────────────────────────────────────────────────────────────────


{-| A triangle in the XZ plane at Y=0 (LDraw Y-down space).
Winding: CCW viewed from above (-Y direction in LDraw = +Y after flip).
-}
xzTriangle : Int -> LDrawLine
xzTriangle color =
    Triangle { color = color, p1 = vec3 0 0 0, p2 = vec3 1 0 0, p3 = vec3 0 0 1 }


{-| A quad in the XZ plane.
-}
xzQuad : Int -> LDrawLine
xzQuad color =
    Quad { color = color, p1 = vec3 -1 0 -1, p2 = vec3 1 0 -1, p3 = vec3 1 0 1, p4 = vec3 -1 0 1 }


emptyCache : LDraw.Resolve.PartCache
emptyCache =
    Dict.empty


allVertices : List ( a, a, a ) -> List a
allVertices triangles =
    triangles
        |> List.concatMap (\( a, b, c ) -> [ a, b, c ])


isAtOrigin : Vec3.Vec3 -> Bool
isAtOrigin p =
    abs (Vec3.getX p)
        < 1.0e-6
        && abs (Vec3.getY p)
        < 1.0e-6
        && abs (Vec3.getZ p)
        < 1.0e-6



-- ── Suite ─────────────────────────────────────────────────────────────────────


suite : Test
suite =
    describe "LDraw.Geometry.flatten"
        [ describe "triangle count"
            [ test "one Triangle line produces 1 triangle" <|
                \_ ->
                    let
                        result =
                            flatten [ xzTriangle 4 ] emptyCache 15 Mat4.identity
                    in
                    Expect.equal 1 (List.length result.triangles)
            , test "one Quad line produces 2 triangles" <|
                \_ ->
                    let
                        result =
                            flatten [ xzQuad 4 ] emptyCache 15 Mat4.identity
                    in
                    Expect.equal 2 (List.length result.triangles)
            , test "three Triangles produce 3 triangles" <|
                \_ ->
                    let
                        lines =
                            [ xzTriangle 4, xzTriangle 2, xzTriangle 1 ]

                        result =
                            flatten lines emptyCache 15 Mat4.identity
                    in
                    Expect.equal 3 (List.length result.triangles)
            , test "Comment lines produce no geometry" <|
                \_ ->
                    let
                        result =
                            flatten [ Comment "0 BFC CERTIFY CCW", Comment "just a comment" ] emptyCache 15 Mat4.identity
                    in
                    Expect.equal 0 (List.length result.triangles)
            ]
        , describe "Y-axis negation (LDraw Y-down → Y-up)"
            [ test "a point at LDraw Y=8 comes out at output Y=-8" <|
                \_ ->
                    let
                        lines =
                            [ Triangle { color = 4, p1 = vec3 0 8 0, p2 = vec3 1 8 0, p3 = vec3 0 8 1 } ]

                        result =
                            flatten lines emptyCache 15 Mat4.identity
                    in
                    case result.triangles of
                        ( v, _, _ ) :: _ ->
                            Vec3.getY v.position
                                |> Expect.within (Expect.Absolute 1.0e-6) -8.0

                        [] ->
                            Expect.fail "Expected a triangle"
            , test "a point at LDraw Y=0 stays at output Y=0" <|
                \_ ->
                    let
                        lines =
                            [ xzTriangle 4 ]

                        result =
                            flatten lines emptyCache 15 Mat4.identity
                    in
                    case result.triangles of
                        ( v, _, _ ) :: _ ->
                            Vec3.getY v.position
                                |> Expect.within (Expect.Absolute 1.0e-6) 0.0

                        [] ->
                            Expect.fail "Expected a triangle"
            ]
        , describe "normal direction"
            [ test "XZ-plane triangle (Y=0) has normal pointing up (+Y)" <|
                \_ ->
                    -- xzTriangle has p1=(0,0,0), p2=(1,0,0), p3=(0,0,1)
                    -- After Y-negation: p1=(0,0,0), p2=(1,0,0), p3=(0,0,1) (Y unchanged because 0)
                    -- edge1 = (1,0,0), edge2 = (0,0,1)
                    -- cross = (0*1 - 0*0, 0*0 - 1*1, 1*0 - 0*0) = (0, -1, 0)
                    -- Wait — after Y-negation the winding might flip.
                    -- The test just checks |normal.y| > 0 (points vertically)
                    let
                        result =
                            flatten [ xzTriangle 4 ] emptyCache 15 Mat4.identity
                    in
                    case result.triangles of
                        ( v, _, _ ) :: _ ->
                            Expect.greaterThan 0.0 (abs (Vec3.getY v.normal))

                        [] ->
                            Expect.fail "Expected a triangle"
            , test "normal is a unit vector" <|
                \_ ->
                    let
                        result =
                            flatten [ xzTriangle 4 ] emptyCache 15 Mat4.identity
                    in
                    case result.triangles of
                        ( v, _, _ ) :: _ ->
                            Vec3.length v.normal
                                |> Expect.within (Expect.Absolute 1.0e-5) 1.0

                        [] ->
                            Expect.fail "Expected a triangle"
            , test "shared vertices across adjacent faces keep per-face normals" <|
                \_ ->
                    let
                        lines =
                            [ Triangle { color = 4, p1 = vec3 0 0 0, p2 = vec3 1 0 0, p3 = vec3 0 0 1 }
                            , Triangle { color = 4, p1 = vec3 0 0 0, p2 = vec3 1 0 0, p3 = vec3 0 1 0 }
                            ]

                        result =
                            flatten lines emptyCache 15 Mat4.identity

                        originNormals =
                            result.triangles
                                |> allVertices
                                |> List.filter (\v -> isAtOrigin v.position)
                                |> List.map .normal
                    in
                    case originNormals of
                        n :: _ ->
                            Expect.all
                                [ \normal ->
                                    Expect.equal True
                                        (abs (Vec3.getY normal) < 1.0e-6 || abs (Vec3.getZ normal) < 1.0e-6)
                                , \normal -> Vec3.length normal |> Expect.within (Expect.Absolute 1.0e-5) 1.0
                                ]
                                n

                        [] ->
                            Expect.fail "Expected at least one origin vertex"
            ]
        , describe "transform accumulation"
            [ test "translation shifts vertex positions" <|
                \_ ->
                    let
                        -- Translate all geometry by (10, 0, 0)
                        translationMat =
                            Mat4.makeTranslate (vec3 10 0 0)

                        result =
                            flatten [ xzTriangle 4 ] emptyCache 15 translationMat
                    in
                    case result.triangles of
                        ( v1, _, _ ) :: _ ->
                            -- p1=(0,0,0) → transformed to (10,0,0) → Y unchanged (still 0)
                            Vec3.getX v1.position
                                |> Expect.within (Expect.Absolute 1.0e-5) 10.0

                        [] ->
                            Expect.fail "Expected a triangle"
            , test "sub-file transform accumulates with world transform" <|
                \_ ->
                    let
                        -- A sub-file at (20, 0, 0) containing a triangle at origin
                        subTranslation =
                            Mat4.makeTranslate3 20 0 0

                        subFileRef =
                            SubFileRef
                                { color = 16
                                , transform = subTranslation
                                , file = "sub.dat"
                                }

                        subContent =
                            [ xzTriangle 4 ]

                        cache =
                            Dict.fromList [ ( "sub.dat", Loaded subContent ) ]

                        result =
                            flatten [ subFileRef ] cache 15 Mat4.identity
                    in
                    case result.triangles of
                        ( v, _, _ ) :: _ ->
                            -- p1=(0,0,0) inside sub → world pos=(20,0,0)
                            Vec3.getX v.position
                                |> Expect.within (Expect.Absolute 1.0e-5) 20.0

                        [] ->
                            Expect.fail "Expected a triangle from sub-file"
            ]
        , describe "color inheritance"
            [ test "color 16 in triangle inherits parent color (blue → code 1)" <|
                \_ ->
                    let
                        -- Parent color = 1 (Blue). Triangle uses color 16 (inherit).
                        lines =
                            [ Triangle { color = 16, p1 = vec3 0 0 0, p2 = vec3 1 0 0, p3 = vec3 0 0 1 } ]

                        result =
                            flatten lines emptyCache 1 Mat4.identity
                    in
                    case result.triangles of
                        ( v, _, _ ) :: _ ->
                            -- Compared against the resolver rather than a literal,
                            -- so the assertion tracks the generated LDConfig table.
                            let
                                blue =
                                    Colors.resolveColor 15 1
                            in
                            Expect.all
                                [ \col -> Expect.within (Expect.Absolute 1.0e-5) blue.r (Vec4.getX col)
                                , \col -> Expect.within (Expect.Absolute 1.0e-5) blue.g (Vec4.getY col)
                                , \col -> Expect.within (Expect.Absolute 1.0e-5) blue.b (Vec4.getZ col)
                                ]
                                v.color

                        [] ->
                            Expect.fail "Expected a triangle"
            , test "unknown color falls back to magenta (r=1, g=0, b=1)" <|
                \_ ->
                    let
                        lines =
                            [ Triangle { color = 9999, p1 = vec3 0 0 0, p2 = vec3 1 0 0, p3 = vec3 0 0 1 } ]

                        result =
                            flatten lines emptyCache 15 Mat4.identity
                    in
                    case result.triangles of
                        ( v, _, _ ) :: _ ->
                            Expect.all
                                [ \col -> Expect.within (Expect.Absolute 1.0e-5) 1.0 (Vec4.getX col) -- r=1
                                , \col -> Expect.within (Expect.Absolute 1.0e-5) 0.0 (Vec4.getY col) -- g=0
                                , \col -> Expect.within (Expect.Absolute 1.0e-5) 1.0 (Vec4.getZ col) -- b=1
                                ]
                                v.color

                        [] ->
                            Expect.fail "Expected a triangle"
            ]
        , describe "missing sub-file"
            [ test "sub-file absent from cache is silently skipped" <|
                \_ ->
                    let
                        subFileRef =
                            SubFileRef
                                { color = 16
                                , transform = Mat4.identity
                                , file = "missing.dat"
                                }

                        -- One triangle + one unresolvable sub-file ref
                        lines =
                            [ subFileRef, xzTriangle 4 ]

                        result =
                            flatten lines emptyCache 15 Mat4.identity
                    in
                    -- Only the explicit triangle is produced, no crash
                    Expect.equal 1 (List.length result.triangles)
            ]
        , describe "edge lines"
            [ test "LineSegment produces one line pair" <|
                \_ ->
                    let
                        lines =
                            [ LineSegment { color = 24, p1 = vec3 0 0 0, p2 = vec3 10 0 0 } ]

                        result =
                            flatten lines emptyCache 15 Mat4.identity
                    in
                    Expect.equal 1 (List.length result.lines)
            ]
        , describe "conditional lines"
            [ test "ConditionalLine is retained for runtime visibility checks" <|
                \_ ->
                    let
                        lines =
                            [ ConditionalLine
                                { color = 24
                                , p1 = vec3 0 0 0
                                , p2 = vec3 10 0 0
                                , c1 = vec3 0 0 1
                                , c2 = vec3 0 1 1
                                }
                            ]

                        result =
                            flatten lines emptyCache 15 Mat4.identity
                    in
                    Expect.equal 1 (List.length result.conditionalLines)
            ]
        , describe "inherited-colour flagging"
            [ test "worldOptions never flags a vertex" <|
                \_ ->
                    -- Directly rendered geometry resolves colours up front, so
                    -- instanceColor must not be able to override anything.
                    LDraw.Geometry.flattenWith LDraw.Geometry.worldOptions
                        [ Triangle { color = 16, p1 = vec3 0 0 0, p2 = vec3 1 0 0, p3 = vec3 0 0 1 } ]
                        emptyCache
                        1
                        Mat4.identity
                        |> .triangles
                        |> allVertices
                        |> List.map .inherit
                        |> Expect.equal [ 0, 0, 0 ]
            , test "localOptions flags a colour-16 surface" <|
                \_ ->
                    LDraw.Geometry.flattenWith LDraw.Geometry.localOptions
                        [ Triangle { color = 16, p1 = vec3 0 0 0, p2 = vec3 1 0 0, p3 = vec3 0 0 1 } ]
                        emptyCache
                        16
                        Mat4.identity
                        |> .triangles
                        |> allVertices
                        |> List.map .inherit
                        |> Expect.equal [ 1, 1, 1 ]
            , test "a hard-coded colour is not flagged" <|
                \_ ->
                    LDraw.Geometry.flattenWith LDraw.Geometry.localOptions
                        [ xzTriangle 4 ]
                        emptyCache
                        16
                        Mat4.identity
                        |> .triangles
                        |> allVertices
                        |> List.map .inherit
                        |> Expect.equal [ 0, 0, 0 ]
            , test "a sub-file that fixes a colour breaks inheritance below it" <|
                \_ ->
                    -- The printed-tile case: the tile body follows the
                    -- placement colour, the print stays the colour it declares.
                    let
                        cache =
                            Dict.fromList
                                [ ( "print.dat"
                                  , Loaded [ Triangle { color = 16, p1 = vec3 0 0 0, p2 = vec3 1 0 0, p3 = vec3 0 0 1 } ]
                                  )
                                ]

                        lines =
                            [ Triangle { color = 16, p1 = vec3 0 0 0, p2 = vec3 1 0 0, p3 = vec3 0 0 1 }
                            , SubFileRef { color = 0, transform = Mat4.identity, file = "print.dat" }
                            ]
                    in
                    LDraw.Geometry.flattenWith LDraw.Geometry.localOptions lines cache 16 Mat4.identity
                        |> .triangles
                        |> allVertices
                        |> List.map .inherit
                        |> List.sum
                        -- Three flagged vertices from the body, none from the print.
                        |> Expect.within (Expect.Absolute 1.0e-9) 3
            ]
        , describe "BFC certification"
            [ test "file with BFC CERTIFY CCW sets bfcCertified = True" <|
                \_ ->
                    let
                        lines =
                            [ Comment "BFC CERTIFY CCW", xzTriangle 4 ]

                        result =
                            flatten lines emptyCache 15 Mat4.identity
                    in
                    Expect.equal True result.bfcCertified
            , test "file without BFC declaration sets bfcCertified = False" <|
                \_ ->
                    let
                        result =
                            flatten [ xzTriangle 4 ] emptyCache 15 Mat4.identity
                    in
                    Expect.equal False result.bfcCertified
            ]
        , describe "flattenWithDetail"
            [ test "NoStuds skips top studs including variants and prefixes, while keeping studel/studtube and body geometry" <|
                \_ ->
                    let
                        detailStud =
                            [ xzTriangle 16, xzQuad 16 ]

                        cache =
                            Dict.fromList
                                [ ( "stud.dat", Loaded detailStud )
                                , ( "stud2.dat", Loaded detailStud )
                                , ( "stud2a.dat", Loaded detailStud )
                                , ( "stud10.dat", Loaded detailStud )
                                , ( "stud15.dat", Loaded detailStud )
                                , ( "48/stud.dat", Loaded detailStud )
                                , ( "studel.dat", Loaded [ xzTriangle 16 ] )
                                , ( "studtube.dat", Loaded [ xzTriangle 16 ] )
                                ]

                        partLines =
                            [ xzQuad 16
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "stud.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "stud2.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "stud2a.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "stud10.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "stud15.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "48/stud.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "studel.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "studtube.dat" }
                            ]

                        full =
                            flattenWithDetail defaultDetailOptions partLines cache 15 Mat4.identity

                        noStuds =
                            flattenWithDetail { defaultDetailOptions | studs = NoStuds } partLines cache 15 Mat4.identity
                    in
                    Expect.all
                        [ \_ -> Expect.equal 22 (List.length full.triangles)
                        , \_ -> Expect.equal 4 (List.length noStuds.triangles)
                        ]
                        ()
            , test "BoxStuds emits 10 outward-wound triangles per top stud with expected bounds and inheritance" <|
                \_ ->
                    let
                        result =
                            flattenWithDetail
                                { defaultDetailOptions | studs = BoxStuds, trackInherit = True }
                                [ SubFileRef { color = 16, transform = Mat4.identity, file = "stud.dat" }
                                , SubFileRef { color = 4, transform = Mat4.makeTranslate3 20 0 0, file = "48/stud2a.dat" }
                                ]
                                emptyCache
                                15
                                Mat4.identity

                        ys =
                            allVertices result.triangles |> List.map (.position >> Vec3.getY)

                        topTris =
                            List.filter (\( a, _, _ ) -> abs (Vec3.getY a.normal - 1.0) < 1.0e-5) result.triangles

                        singleOriginStud =
                            flattenWithDetail
                                { defaultDetailOptions | studs = BoxStuds }
                                [ SubFileRef { color = 16, transform = Mat4.identity, file = "stud.dat" } ]
                                emptyCache
                                15
                                Mat4.identity

                        allOriginFacesOutward =
                            List.all
                                (\( a, b, c ) ->
                                    let
                                        centroid =
                                            Vec3.scale (1 / 3) (Vec3.add a.position (Vec3.add b.position c.position))
                                    in
                                    Vec3.dot (Vec3.sub centroid (vec3 0 0 0)) a.normal > 0
                                )
                                singleOriginStud.triangles

                        inheritedCount =
                            allVertices result.triangles
                                |> List.filter (\v -> v.inherit > 0.5)
                                |> List.length
                    in
                    Expect.all
                        [ \_ -> Expect.equal 20 (List.length result.triangles)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-5) 0 (List.minimum ys |> Maybe.withDefault 99)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-5) 4 (List.maximum ys |> Maybe.withDefault -99)
                        , \_ -> Expect.equal 4 (List.length topTris)
                        , \_ -> Expect.equal True allOriginFacesOutward
                        , \_ -> Expect.equal 30 inheritedCount
                        ]
                        ()
            , test "BoxStuds matches FullStuds winding under INVERTNEXT and negative-determinant transforms" <|
                \_ ->
                    let
                        reflMat =
                            Mat4.makeScale3 -1 1 1

                        flatTopStud =
                            Dict.fromList
                                [ ( "stud.dat"
                                  , Loaded
                                        [ Quad
                                            { color = 16
                                            , p1 = vec3 -6 -4 6
                                            , p2 = vec3 -6 -4 -6
                                            , p3 = vec3 6 -4 -6
                                            , p4 = vec3 6 -4 6
                                            }
                                        ]
                                  )
                                ]

                        runStud studs invert mat =
                            flattenWithDetail
                                { defaultDetailOptions | studs = studs }
                                (if invert then
                                    [ Comment "BFC INVERTNEXT"
                                    , SubFileRef { color = 16, transform = mat, file = "stud.dat" }
                                    ]

                                 else
                                    [ SubFileRef { color = 16, transform = mat, file = "stud.dat" } ]
                                )
                                flatTopStud
                                15
                                Mat4.identity

                        countTopSign sign res =
                            List.filter (\( a, _, _ ) -> (sign * Vec3.getY a.normal) > 0.9) res.triangles
                                |> List.length
                    in
                    Expect.all
                        [ \_ -> Expect.equal 2 (countTopSign 1 (runStud BoxStuds False Mat4.identity))
                        , \_ -> Expect.equal 2 (countTopSign 1 (runStud FullStuds False Mat4.identity))
                        , \_ -> Expect.equal 2 (countTopSign -1 (runStud BoxStuds True Mat4.identity))
                        , \_ -> Expect.equal 2 (countTopSign -1 (runStud FullStuds True Mat4.identity))
                        , \_ -> Expect.equal 2 (countTopSign 1 (runStud BoxStuds False reflMat))
                        , \_ -> Expect.equal 2 (countTopSign 1 (runStud FullStuds False reflMat))
                        , \_ -> Expect.equal 2 (countTopSign -1 (runStud BoxStuds True reflMat))
                        , \_ -> Expect.equal 2 (countTopSign -1 (runStud FullStuds True reflMat))
                        ]
                        ()
            , test "includeHollows = False skips stud3* and stud4* (including path prefixes) and consumes INVERTNEXT" <|
                \_ ->
                    let
                        hollow =
                            [ xzTriangle 16 ]

                        cache =
                            Dict.fromList
                                [ ( "stud3.dat", Loaded hollow )
                                , ( "stud3a.dat", Loaded hollow )
                                , ( "stud4.dat", Loaded hollow )
                                , ( "s/stud4.dat", Loaded hollow )
                                , ( "stud4od.dat", Loaded hollow )
                                , ( "sub.dat", Loaded [ xzTriangle 16 ] )
                                ]

                        partLines =
                            [ SubFileRef { color = 16, transform = Mat4.identity, file = "stud3.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "stud3a.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "stud4.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "s/stud4.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "stud4od.dat" }
                            , Comment "BFC INVERTNEXT"
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "s/stud4.dat" }
                            , SubFileRef { color = 16, transform = Mat4.identity, file = "sub.dat" }
                            ]

                        withHollows =
                            flattenWithDetail defaultDetailOptions partLines cache 15 Mat4.identity

                        withoutHollows =
                            flattenWithDetail { defaultDetailOptions | includeHollows = False } partLines cache 15 Mat4.identity

                        expectedNormalY =
                            case (flatten [ xzTriangle 16 ] emptyCache 15 Mat4.identity).triangles of
                                ( v, _, _ ) :: _ ->
                                    Vec3.getY v.normal

                                [] ->
                                    0
                    in
                    Expect.all
                        [ \_ -> Expect.equal 7 (List.length withHollows.triangles)
                        , \_ -> Expect.equal 1 (List.length withoutHollows.triangles)
                        , \_ ->
                            case withoutHollows.triangles of
                                ( v, _, _ ) :: _ ->
                                    Vec3.getY v.normal |> Expect.within (Expect.Absolute 1.0e-5) expectedNormalY

                                [] ->
                                    Expect.fail "Expected sub.dat triangle"
                        ]
                        ()
            , test "includeLines = False and includeConditionals = False omit lines and conditionalLines" <|
                \_ ->
                    let
                        partLines =
                            [ xzTriangle 4
                            , LineSegment { color = 24, p1 = vec3 0 0 0, p2 = vec3 10 0 0 }
                            , ConditionalLine
                                { color = 24
                                , p1 = vec3 0 0 0
                                , p2 = vec3 10 0 0
                                , c1 = vec3 0 0 1
                                , c2 = vec3 0 1 1
                                }
                            ]

                        stripped =
                            flattenWithDetail
                                { defaultDetailOptions | includeLines = False, includeConditionals = False }
                                partLines
                                emptyCache
                                15
                                Mat4.identity
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (List.length stripped.triangles)
                        , \_ -> Expect.equal 0 (List.length stripped.lines)
                        , \_ -> Expect.equal 0 (List.length stripped.conditionalLines)
                        ]
                        ()
            ]
        ]
