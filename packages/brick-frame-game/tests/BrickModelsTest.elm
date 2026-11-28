module BrickModelsTest exposing (suite)

{-| Unit tests for `Game.BrickModels` and `Game.BrickProps`: every prop is
made of catalog parts, and loading, building and fitting a prop work.
-}

import Dict
import Expect
import Game.BrickModels as BrickModels
import Game.BrickProps as BrickProps
import LDraw.CuratedParts as CuratedParts
import LDraw.Geometry as Geometry
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Math.Vector4 exposing (vec4)
import Set
import Test exposing (Test, describe, test)


{-| The catalog's visible curated parts. The pine trees were added to the
catalog (brick-catalog 84a9a8c) after `CuratedParts` was generated.
-}
catalogParts : Set.Set String
catalogParts =
    Set.fromList ("3471.dat" :: CuratedParts.visibleFiles)


partsOf : String -> List String
partsOf ldraw =
    Parser.parseFile ldraw
        |> List.filterMap
            (\line ->
                case line of
                    SubFileRef ref ->
                        Just ref.file

                    _ ->
                        Nothing
            )


colouredPartsOf : String -> List ( Int, String )
colouredPartsOf ldraw =
    Parser.parseFile ldraw
        |> List.filterMap
            (\line ->
                case line of
                    SubFileRef ref ->
                        Just ( ref.color, ref.file )

                    _ ->
                        Nothing
            )


{-| A stand-in part: one triangle spanning a 40 x 24 x 20 LDU box.
-}
fakeBrick : List LDrawLine
fakeBrick =
    Parser.parseFile "3 16 -20 0 -10 20 24 10 20 -4 10"


{-| A one-triangle prop, BFC certified, half in colour 16 (inherited) and
half in hard-coded red: its vertices in Y-up units are (-20, 0, -10),
(20, -24, 10) and (20, 4, 10).
-}
mixedProp : BrickProps.PropModel
mixedProp =
    BrickProps.build (BrickModels.singlePart "3001.dat")
        (Dict.fromList [ ( "3001.dat", Loaded (Parser.parseFile "0 BFC CERTIFY CCW\n3 16 -20 0 -10 20 24 10 20 -4 10") ) ])


redProp : BrickProps.PropModel
redProp =
    BrickProps.build (BrickModels.singlePart "3001.dat")
        (Dict.fromList [ ( "3001.dat", Loaded (Parser.parseFile "0 BFC CERTIFY CCW\n3 4 -20 0 -10 20 24 10 20 -4 10") ) ])


mergeTests : List Test
mergeTests =
    let
        green =
            vec4 0 1 0 1

        positions prop =
            List.concatMap (\( a, b, c ) -> [ a.position, b.position, c.position ]) prop.scene.surface

        close a b =
            Vec3.distance a b < 1.0e-6
    in
    [ test "merging no placements is an empty prop at the origin" <|
        \_ ->
            BrickProps.merge []
                |> (\m -> ( m.triangleCount, m.boundsMin, m.boundsMax ))
                |> Expect.equal ( 0, vec3 0 0 0, vec3 0 0 0 )
    , test "every placement's triangles are baked at its model matrix" <|
        \_ ->
            let
                merged =
                    BrickProps.merge
                        [ { prop = mixedProp, model = Mat4.identity, tint = green }
                        , { prop = mixedProp, model = Mat4.makeTranslate3 100 0 0, tint = green }
                        , { prop = redProp, model = Mat4.makeScale3 2 2 2, tint = green }
                        ]

                original =
                    positions mixedProp
            in
            Expect.all
                [ \m -> Expect.equal 3 m.triangleCount
                , \m -> Expect.equal 3 (List.length m.scene.surface)
                , \m ->
                    Expect.equal True
                        (List.all identity
                            (List.map2 close
                                (positions m)
                                (original
                                    ++ List.map (Vec3.add (vec3 100 0 0)) original
                                    ++ List.map (Vec3.scale 2) original
                                )
                            )
                        )
                , \m -> Expect.within (Expect.Absolute 1.0e-6) 120 (Vec3.getX m.boundsMax)
                , \m -> Expect.within (Expect.Absolute 1.0e-6) -48 (Vec3.getY m.boundsMin)
                , \m -> Expect.equal True m.scene.bfcCertified
                ]
                merged
    , test "the tint is painted into colour-16 parts only, and nothing inherits after" <|
        \_ ->
            let
                merged =
                    BrickProps.merge
                        [ { prop = mixedProp, model = Mat4.identity, tint = green }
                        , { prop = redProp, model = Mat4.identity, tint = green }
                        ]

                colours =
                    List.map (\( a, _, _ ) -> ( a.color, a.inherit )) merged.scene.surface

                red =
                    List.filterMap (\( a, _, _ ) -> Just a.color) redProp.scene.surface
            in
            Expect.equal (( green, 0 ) :: List.map (\c -> ( c, 0 )) red) colours
    , test "normals are turned with the placement and stay unit length" <|
        \_ ->
            let
                quarterTurn =
                    Mat4.makeTranslate3 5 6 7
                        |> Mat4.rotate (pi / 2) (vec3 0 1 0)
                        |> Mat4.scale3 0.01 0.01 0.01

                expected =
                    List.map (\( a, _, _ ) -> Vec3.normalize (Mat4.transform (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) a.normal)) mixedProp.scene.surface

                got =
                    List.map (\( a, _, _ ) -> a.normal) (BrickProps.merge [ { prop = mixedProp, model = quarterTurn, tint = green } ]).scene.surface
            in
            Expect.equal True (List.all identity (List.map2 close expected got) && List.length got == 1)
    , test "a mirrored placement has its winding reversed" <|
        \_ ->
            let
                mirror =
                    Mat4.makeScale3 -1 1 1

                firstOf prop =
                    List.head prop.scene.surface |> Maybe.map (\( a, b, c ) -> [ a.position, b.position, c.position ])

                expected =
                    firstOf mixedProp |> Maybe.map (\ps -> List.map (Mat4.transform mirror) ps)
            in
            case ( expected, firstOf (BrickProps.merge [ { prop = mixedProp, model = mirror, tint = green } ]) ) of
                ( Just [ a, b, c ], Just [ a2, b2, c2 ] ) ->
                    Expect.equal [ True, True, True ] [ close a a2, close c b2, close b c2 ]

                _ ->
                    Expect.fail "expected one triangle each"
    , test "the merge is BFC certified only when every placed prop is" <|
        \_ ->
            BrickProps.merge
                [ { prop = mixedProp, model = Mat4.identity, tint = green }
                , { prop = BrickProps.build (BrickModels.singlePart "3001.dat") (Dict.fromList [ ( "3001.dat", Loaded fakeBrick ) ]), model = Mat4.identity, tint = green }
                ]
                |> .scene
                |> .bfcCertified
                |> Expect.equal False
    ]


suite : Test
suite =
    describe "Brick props"
        [ describe "models"
            (List.map
                (\( name, ldraw ) ->
                    test (name ++ " is built from catalog parts only") <|
                        \_ ->
                            let
                                parts =
                                    partsOf ldraw
                            in
                            Expect.all
                                [ \ps -> Expect.notEqual [] ps
                                , \ps -> Expect.equal [] (List.filter (\p -> not (Set.member p catalogParts)) ps)
                                ]
                                parts
                )
                BrickModels.all
            )
        , test "the summer trees are the green variants" <|
            \_ ->
                Expect.equal
                    ( BrickModels.leafyTreeIn { leaves = 2 }, BrickModels.pineTreeIn { needles = 288, cap = Nothing } )
                    ( BrickModels.leafyTree, BrickModels.pineTree )
        , test "an autumn leafy tree has no green left in its crown" <|
            \_ ->
                colouredPartsOf (BrickModels.leafyTreeIn { leaves = 484 })
                    |> List.map Tuple.first
                    |> List.filter (\c -> c == 2 || c == 10)
                    |> Expect.equal []
        , test "a snow cap adds one cone in the cap colour on top of the pine" <|
            \_ ->
                Expect.equal
                    [ ( 288, "3471.dat" ), ( 15, "3942c.dat" ) ]
                    (colouredPartsOf (BrickModels.pineTreeIn { needles = 288, cap = Just 15 }))
        , test "a single-part model takes the tint" <|
            \_ ->
                BrickModels.singlePart "3001.dat"
                    |> Parser.parseFile
                    |> List.filterMap
                        (\line ->
                            case line of
                                SubFileRef ref ->
                                    Just ( ref.color, ref.file )

                                _ ->
                                    Nothing
                        )
                    |> Expect.equal [ ( 16, "3001.dat" ) ]
        , test "progress asks for every part of a model once" <|
            \_ ->
                BrickProps.progress BrickModels.fence Resolve.initCache
                    |> .needed
                    |> Expect.equal [ "3005.dat", "3024.dat", "3633.dat" ]
        , test "a model with all parts loaded is ready" <|
            \_ ->
                let
                    cache =
                        Dict.fromList
                            [ ( "3005.dat", Loaded fakeBrick )
                            , ( "3024.dat", Loaded fakeBrick )
                            , ( "3633.dat", Loaded fakeBrick )
                            ]
                in
                BrickProps.progress BrickModels.fence cache
                    |> (\p -> ( p.needed, p.inFlight, p.settled ))
                    |> Expect.equal ( [], 0, 3 )
        , test "refresh builds the models that are ready and fetches the rest once" <|
            \_ ->
                let
                    cache =
                        Dict.fromList [ ( "3001.dat", Loaded fakeBrick ) ]

                    r =
                        BrickProps.refresh
                            [ ( "brick", BrickModels.singlePart "3001.dat" ), ( "fence", BrickModels.fence ) ]
                            cache
                            Dict.empty

                    again =
                        BrickProps.refresh [ ( "fence", BrickModels.fence ) ] r.cache r.props
                in
                Expect.all
                    [ \_ -> Expect.equal [ "brick" ] (Dict.keys r.props)
                    , \_ -> Expect.equal [ "3005.dat", "3024.dat", "3633.dat" ] r.fetch
                    , \_ -> Expect.equal [] again.fetch
                    ]
                    ()
        , test "build measures the model in Y-up units and fitting scales it" <|
            \_ ->
                let
                    prop =
                        BrickProps.build (BrickModels.singlePart "3001.dat") (Dict.fromList [ ( "3001.dat", Loaded fakeBrick ) ])

                    top =
                        Mat4.transform (BrickProps.fitHeight 2.0 prop) (vec3 0 (Vec3.getY prop.boundsMax) 0)

                    bottom =
                        Mat4.transform (BrickProps.fitHeight 2.0 prop) (vec3 0 (Vec3.getY prop.boundsMin) 0)
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 1.0e-6) 28 (Vec3.getY (BrickProps.size prop))
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) 2.0 (Vec3.getY top)
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) 0.0 (Vec3.getY bottom)
                    , \_ -> Expect.equal 1 prop.triangleCount
                    ]
                    ()
        , describe "merge" mergeTests
        , test "an arch has two posts and a beam that covers them" <|
            \_ ->
                let
                    bricks =
                        BrickModels.archBricks { halfWidth = 4.0, postHeight = 4.8, unit = 0.016, postColor = 1, bannerColors = [ 14, 15 ] }

                    xs =
                        List.map (\b -> Vec3.getX (Mat4.transform b.transform (vec3 0 0 0))) bricks

                    posts =
                        List.filter (\b -> b.color == 1) bricks
                in
                Expect.all
                    [ \_ -> Expect.equal 26 (List.length posts)
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) 0 (List.sum xs)
                    , \_ -> Expect.atLeast 4.0 (List.maximum xs |> Maybe.withDefault 0)
                    ]
                    ()
        , test "BFC certification requires all transitive subfiles to certify CCW cleanly" <|
            \_ ->
                let
                    certifiedPart =
                        Parser.parseFile "0 BFC CERTIFY CCW\n3 16 -20 0 -10 20 24 10 20 -4 10"

                    cwPart =
                        Parser.parseFile "0 BFC CERTIFY CW\n3 16 -20 0 -10 20 24 10 20 -4 10"

                    certifiedCache =
                        Dict.fromList [ ( "3001.dat", Loaded certifiedPart ) ]

                    uncertifiedCache =
                        Dict.fromList [ ( "3001.dat", Loaded fakeBrick ) ]

                    cwCache =
                        Dict.fromList [ ( "3001.dat", Loaded cwPart ) ]

                    builtCertified =
                        BrickProps.build (BrickModels.singlePart "3001.dat") certifiedCache

                    builtUncertified =
                        BrickProps.build (BrickModels.singlePart "3001.dat") uncertifiedCache

                    builtCw =
                        BrickProps.build (BrickModels.singlePart "3001.dat") cwCache
                in
                Expect.all
                    [ \_ -> Expect.equal True builtCertified.scene.bfcCertified
                    , \_ -> Expect.equal False builtUncertified.scene.bfcCertified
                    , \_ -> Expect.equal False builtCw.scene.bfcCertified
                    , \_ -> Expect.equal True (BrickProps.arePartsBfcCertified certifiedCache [ "3001.dat" ])
                    , \_ -> Expect.equal False (BrickProps.arePartsBfcCertified uncertifiedCache [ "3001.dat" ])
                    , \_ -> Expect.equal False (BrickProps.arePartsBfcCertified cwCache [ "3001.dat" ])
                    , \_ -> Expect.equal 1 (List.length (BrickProps.surfaceSettings False Mat4.identity))
                    , \_ -> Expect.equal 2 (List.length (BrickProps.surfaceSettings True Mat4.identity))
                    , \_ -> Expect.equal 2 (List.length (BrickProps.surfaceSettings True (Mat4.makeScale3 -1 1 1)))
                    ]
                    ()
        , test "buildStatic, buildWith and refreshWith apply StudDetail and hollow filtering" <|
            \_ ->
                let
                    studPart =
                        Parser.parseFile "0 BFC CERTIFY CCW\n4 16 -4 0 -4 4 0 -4 4 -4 4 -4 -4 4\n4 16 -4 -4 -4 4 -4 -4 4 -4 4 -4 -4 4\n2 24 0 0 0 4 0 0"

                    hollowPart =
                        Parser.parseFile "0 BFC CERTIFY CCW\n4 16 -2 0 -2 2 0 -2 2 8 2 -2 8 2"

                    platePart =
                        Parser.parseFile
                            (String.join "\n"
                                [ "0 BFC CERTIFY CCW"
                                , "4 16 -20 0 -20 20 0 -20 20 8 20 -20 8 20"
                                , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud.dat"
                                , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 48/stud2a.dat"
                                , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud15.dat"
                                , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud3.dat"
                                , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 s/stud4.dat"
                                , "2 24 -20 0 -20 20 0 -20"
                                ]
                            )

                    cache =
                        Dict.fromList
                            [ ( "3001.dat", Loaded platePart )
                            , ( "stud.dat", Loaded studPart )
                            , ( "48/stud2a.dat", Loaded studPart )
                            , ( "stud15.dat", Loaded studPart )
                            , ( "stud3.dat", Loaded hollowPart )
                            , ( "s/stud4.dat", Loaded hollowPart )
                            ]

                    fullProp =
                        BrickProps.build (BrickModels.singlePart "3001.dat") cache

                    noStudsProp =
                        BrickProps.buildStatic Geometry.NoStuds False (BrickModels.singlePart "3001.dat") cache

                    boxStudsProp =
                        BrickProps.buildStatic Geometry.BoxStuds True (BrickModels.singlePart "3001.dat") cache

                    refreshed =
                        BrickProps.refreshWith
                            (\name ->
                                if name == "stripped" then
                                    { flipYZ = True
                                    , trackInherit = True
                                    , studs = Geometry.NoStuds
                                    , includeHollows = False
                                    , includeLines = False
                                    , includeConditionals = False
                                    }

                                else
                                    BrickProps.defaultDetailOptions
                            )
                            [ ( "full", BrickModels.singlePart "3001.dat" )
                            , ( "stripped", BrickModels.singlePart "3001.dat" )
                            ]
                            cache
                            Dict.empty
                in
                Expect.all
                    [ \_ -> Expect.equal 18 fullProp.triangleCount
                    , \_ -> Expect.equal 2 noStudsProp.triangleCount
                    , \_ -> Expect.equal True noStudsProp.scene.bfcCertified
                    , \_ -> Expect.equal 32 boxStudsProp.triangleCount
                    , \_ -> Expect.within (Expect.Absolute 1.0e-5) 4.0 (Vec3.getY boxStudsProp.boundsMax)
                    , \_ -> Expect.equal (Just 18) (Dict.get "full" refreshed.props |> Maybe.map .triangleCount)
                    , \_ -> Expect.equal (Just 2) (Dict.get "stripped" refreshed.props |> Maybe.map .triangleCount)
                    ]
                    ()
        ]
