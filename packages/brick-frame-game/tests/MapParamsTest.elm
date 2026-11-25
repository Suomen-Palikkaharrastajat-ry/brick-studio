module MapParamsTest exposing (suite)

{-| Tests for the shared map codes, weather compatibility and seed resolution.
-}

import Expect
import Game.MapParams as Map exposing (Biome(..), MapParams, Pick(..), Season(..), WeatherKind(..))
import Test exposing (Test, describe, test)


allCombinations : List ( Biome, Season, WeatherKind )
allCombinations =
    List.concatMap
        (\b ->
            List.concatMap
                (\s -> List.map (\w -> ( b, s, w )) Map.allWeathers)
                Map.allSeasons
        )
        Map.allBiomes


suite : Test
suite =
    describe "Game.MapParams"
        [ describe "weather compatibility"
            [ test "Overcast is allowed in every biome and season" <|
                \_ ->
                    Expect.equal []
                        (List.filter
                            (\( b, s, _ ) -> not (List.member Overcast (Map.allowedWeathers b s)))
                            (List.concatMap
                                (\b -> List.map (\s -> ( b, s, Overcast )) Map.allSeasons)
                                Map.allBiomes
                            )
                        )
            , test "coerce always returns an allowed weather" <|
                \_ ->
                    Expect.equal []
                        (List.filter
                            (\( b, s, w ) -> not (List.member (Map.coerce b s w) (Map.allowedWeathers b s)))
                            allCombinations
                        )
            , test "coerce keeps an allowed weather unchanged" <|
                \_ ->
                    Expect.equal []
                        (List.filter
                            (\( b, s, w ) ->
                                List.member w (Map.allowedWeathers b s) && Map.coerce b s w /= w
                            )
                            allCombinations
                        )
            , test "Snow in summer becomes Rain in Farmland" <|
                \_ ->
                    Expect.equal Rain (Map.coerce Farmland Summer Snow)
            , test "Desert has no snow in any season" <|
                \_ ->
                    Expect.equal []
                        (List.filter
                            (\s -> List.member Snow (Map.allowedWeathers Desert s))
                            Map.allSeasons
                        )
            ]
        , describe "map codes"
            [ test "toCode gives biome-season-weather-seed" <|
                \_ ->
                    Expect.equal "boreal-winter-snow-48213"
                        (Map.toCode { biome = Boreal, season = Winter, weather = Snow, seed = 48213 })
            , test "round trip for every combination" <|
                \_ ->
                    Expect.equal []
                        (List.filterMap
                            (\( b, s, w ) ->
                                let
                                    p =
                                        { biome = b, season = s, weather = Map.coerce b s w, seed = 7 }
                                in
                                if Map.fromCode (Map.toCode p) == Just p then
                                    Nothing

                                else
                                    Just (Map.toCode p)
                            )
                            allCombinations
                        )
            , test "fromCode ignores case and surrounding spaces" <|
                \_ ->
                    Expect.equal
                        (Just { biome = Fell, season = Autumn, weather = Rain, seed = 12 })
                        (Map.fromCode "  Fell-AUTUMN-rain-12 ")
            , test "fromCode coerces an impossible weather" <|
                \_ ->
                    Expect.equal (Just Overcast)
                        (Map.fromCode "farmland-winter-rain-5" |> Maybe.map .weather)
            , test "fromCode clamps the seed" <|
                \_ ->
                    Expect.equal
                        ( Just 1, Just 999999 )
                        ( Map.fromCode "desert-summer-clear-0" |> Maybe.map .seed
                        , Map.fromCode "desert-summer-clear-99999999" |> Maybe.map .seed
                        )
            , test "fromCode rejects Any and malformed codes" <|
                \_ ->
                    Expect.equal [ Nothing, Nothing, Nothing, Nothing ]
                        [ Map.fromCode "any-summer-clear-1"
                        , Map.fromCode "boreal-summer-clear"
                        , Map.fromCode "boreal-summer-clear-x"
                        , Map.fromCode "tropical-summer-clear-1"
                        ]
            ]
        , describe "resolve"
            [ test "concrete picks are kept (weather coerced)" <|
                \_ ->
                    Expect.equal
                        { biome = Desert, season = Winter, weather = Overcast, seed = 3 }
                        (Map.resolve
                            { biome = Exactly Desert
                            , season = Exactly Winter
                            , weather = Exactly Rain
                            , seed = 3
                            }
                        )
            , test "the same selection always resolves to the same map" <|
                \_ ->
                    let
                        sel =
                            { biome = Any, season = Any, weather = Any, seed = 4242 }
                    in
                    Expect.equal (Map.resolve sel) (Map.resolve sel)
            , test "Any picks vary with the seed" <|
                \_ ->
                    let
                        biomes =
                            List.range 1 40
                                |> List.map (\seed -> (Map.resolve { anyWith | seed = seed }).biome)
                    in
                    Expect.greaterThan 1 (List.length (List.foldl (\b acc -> if List.member b acc then acc else b :: acc) [] biomes))
            , test "every resolved Any map is allowed" <|
                \_ ->
                    Expect.equal []
                        (List.filter
                            (\seed ->
                                let
                                    p =
                                        Map.resolve { anyWith | seed = seed }
                                in
                                not (List.member p.weather (Map.allowedWeathers p.biome p.season))
                            )
                            (List.range 1 200)
                        )
            , test "seed is clamped" <|
                \_ ->
                    Expect.equal 1 (Map.resolve { anyWith | seed = -5 }).seed
            ]
        , describe "leaderboard key and streams"
            [ test "leaderboard key is one board per exact map code" <|
                \_ ->
                    let
                        p : MapParams
                        p =
                            { biome = Farmland, season = Spring, weather = Clear, seed = 9 }
                    in
                    Expect.equal ("g1:" ++ Map.toCode p) (Map.leaderboardKey p)
            , test "stream seeds differ by stream name" <|
                \_ ->
                    let
                        p : MapParams
                        p =
                            { biome = Fell, season = Winter, weather = Snow, seed = 1 }
                    in
                    Expect.notEqual (Map.streamSeed "course" p) (Map.streamSeed "scenery" p)
            , test "stream seeds are stable for one map" <|
                \_ ->
                    let
                        p : MapParams
                        p =
                            { biome = Fell, season = Winter, weather = Snow, seed = 1 }
                    in
                    Expect.equal (Map.streamSeed "course" p) (Map.streamSeed "course" p)
            ]
        ]


anyWith : Map.Selection
anyWith =
    Map.anySelection
