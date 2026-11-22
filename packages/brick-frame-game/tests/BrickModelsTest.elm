module BrickModelsTest exposing (suite)

{-| Unit tests for `Game.BrickModels` and `Game.BrickProps`: every prop is
made of catalog parts, and loading, building and fitting a prop work.
-}

import Dict
import Expect
import Game.BrickModels as BrickModels
import Game.BrickProps as BrickProps
import LDraw.CuratedParts as CuratedParts
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
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


{-| A stand-in part: one triangle spanning a 40 x 24 x 20 LDU box.
-}
fakeBrick : List LDrawLine
fakeBrick =
    Parser.parseFile "3 16 -20 0 -10 20 24 10 20 -4 10"


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
        ]
