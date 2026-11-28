module BuildHistoryTest exposing (suite)

{-| Tests for the vehicle editors' build history (`Game.BuildHistory`): it
keeps the newest builds first, at most `maxEntries`, never the same build
twice, replaces the newest entry within one editing session, and survives a
round trip through its JSON (and a damaged one), and for the whole-model
picture requests (`Game.ModelPreview`).
-}

import Expect
import Game.BuildHistory as History exposing (Entry)
import Game.ModelPreview as ModelPreview
import LDraw.Document as Document
import Test exposing (Test, describe, test)


entry : String -> Int -> Entry
entry code session =
    { code = code, base = "narrow-racer", parts = 3, savedAt = session * 1000, session = session }


codes : History.History -> List String
codes history =
    List.map .code (History.entries history)


suite : Test
suite =
    describe "Game.BuildHistory"
        [ test "a build of a new session goes in front" <|
            \_ ->
                History.empty
                    |> History.record (entry "a" 1)
                    |> History.record (entry "b" 2)
                    |> codes
                    |> Expect.equal [ "b", "a" ]
        , test "a build of the same session replaces the newest entry" <|
            \_ ->
                History.empty
                    |> History.record (entry "a" 1)
                    |> History.record (entry "b" 2)
                    |> History.record (entry "c" 2)
                    |> codes
                    |> Expect.equal [ "c", "a" ]
        , test "the same build is listed once, as the newest" <|
            \_ ->
                History.empty
                    |> History.record (entry "a" 1)
                    |> History.record (entry "b" 2)
                    |> History.record (entry "a" 3)
                    |> codes
                    |> Expect.equal [ "a", "b" ]
        , test "saving the same build again in its session keeps the older builds" <|
            \_ ->
                History.empty
                    |> History.record (entry "a" 1)
                    |> History.record (entry "b" 2)
                    |> History.record (entry "b" 2)
                    |> codes
                    |> Expect.equal [ "b", "a" ]
        , test "the history keeps only the newest maxEntries builds" <|
            \_ ->
                let
                    history =
                        List.foldl (\i acc -> History.record (entry ("build-" ++ String.fromInt i) i) acc)
                            History.empty
                            (List.range 1 (History.maxEntries + 5))
                in
                Expect.all
                    [ \h -> Expect.equal History.maxEntries (List.length (History.entries h))
                    , \h -> Expect.equal (Just ("build-" ++ String.fromInt (History.maxEntries + 5))) (Maybe.map .code (History.latest h))
                    , \h -> Expect.equal False (History.member "build-5" h)
                    , \h -> Expect.equal True (History.member "build-6" h)
                    ]
                    history
        , test "an empty build is not saved" <|
            \_ ->
                History.empty
                    |> History.record (entry "" 1)
                    |> History.entries
                    |> Expect.equal []
        , test "remove drops the build" <|
            \_ ->
                History.empty
                    |> History.record (entry "a" 1)
                    |> History.record (entry "b" 2)
                    |> History.remove "a"
                    |> codes
                    |> Expect.equal [ "b" ]
        , test "the history survives a round trip through its JSON" <|
            \_ ->
                let
                    history =
                        History.empty
                            |> History.record (entry "a" 1)
                            |> History.record (entry "b" 2)
                in
                Expect.equal (History.entries history) (History.entries (History.fromJson (History.toJson history)))
        , test "unreadable JSON gives an empty history" <|
            \_ ->
                [ "", "null", "{", "[1,2]", "{\"entries\":3}" ]
                    |> List.map (History.fromJson >> History.entries)
                    |> Expect.equal [ [], [], [], [], [] ]
        , test "damaged and repeated entries are skipped and the list stays bounded" <|
            \_ ->
                let
                    good i =
                        "{\"code\":\"c" ++ String.fromInt i ++ "\",\"session\":" ++ String.fromInt i ++ "}"

                    json =
                        "{\"entries\":["
                            ++ String.join ","
                                ([ good 0, "{\"code\":3}", "{\"code\":\"\"}", good 0 ]
                                    ++ List.map good (List.range 1 30)
                                )
                            ++ "]}"

                    loaded =
                        History.fromJson json
                in
                Expect.all
                    [ \h -> Expect.equal History.maxEntries (List.length (History.entries h))
                    , \h -> Expect.equal (Just "c0") (Maybe.map .code (History.latest h))
                    , \h -> Expect.equal 1 (List.length (List.filter (\e -> e.code == "c0") (History.entries h)))
                    ]
                    loaded
        , describe "Game.ModelPreview"
            [ test "a request flattens the model and lists its colours" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "0 FILE main.ldr"
                                    , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 hull.ldr"
                                    , "1 15 0 -24 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                    , "0 NOFILE"
                                    , "0 FILE hull.ldr"
                                    , "1 70 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                    , "1 16 20 0 0 1 0 0 0 1 0 0 0 1 3003.dat"
                                    , "0 NOFILE"
                                    ]
                                )

                        req =
                            ModelPreview.request "hull:x" doc
                    in
                    Expect.all
                        [ \r -> Expect.equal "hull:x" r.key
                        , \r -> Expect.equal [ 15, 16, 70 ] (List.map .code r.colours)
                        , \r -> Expect.equal 3 (List.length (String.lines r.text))
                        , \r -> Expect.equal False (String.contains "hull.ldr" r.text)
                        , \r -> Expect.equal True (List.all (\c -> List.length c.rgb == 3) r.colours)
                        ]
                        req
            ]
        ]
