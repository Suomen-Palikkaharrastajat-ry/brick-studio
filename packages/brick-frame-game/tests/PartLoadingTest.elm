module PartLoadingTest exposing (suite)

{-| Unit tests for `Game.PartLoading`.
-}

import Dict
import Expect
import Game.PartLoading as PartLoading
import LDraw.Document as Document
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartStatus(..))
import Test exposing (Test, describe, test)


model : Document.Document
model =
    Document.parse
        (String.join "\n"
            [ "0 FILE main.ldr"
            , "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
            , "1 4 0 -24 0 1 0 0 0 1 0 0 0 1 3001.dat"
            , "1 1 0 -48 0 1 0 0 0 1 0 0 0 1 wheel.ldr"
            , "0 FILE wheel.ldr"
            , "1 0 0 0 0 1 0 0 0 1 0 0 0 1 3641.dat"
            , "1 15 0 0 0 1 0 0 0 1 0 0 0 1 4624.dat"
            ]
        )


suite : Test
suite =
    describe "Game.PartLoading"
        [ test "an empty cache needs every part once, through sub-models" <|
            \_ ->
                PartLoading.progress (PartLoading.mainAndSubfileParts model) model Resolve.initCache
                    |> Expect.equal { needed = [ "3001.dat", "3641.dat", "4624.dat" ], total = 3, settled = 0, inFlight = 0 }
        , test "flattened seeds reach the same parts" <|
            \_ ->
                PartLoading.progress (PartLoading.flattenedParts model) model Resolve.initCache
                    |> .needed
                    |> List.sort
                    |> Expect.equal [ "3001.dat", "3641.dat", "4624.dat" ]
        , test "loading, loaded and failed parts are counted" <|
            \_ ->
                let
                    cache =
                        Resolve.initCache
                            |> Resolve.markLoading [ "3001.dat" ]
                            |> Dict.insert "3641.dat" (Failed "404")
                            |> Resolve.updateCache "4624.dat" (Ok "1 16 0 0 0 1 0 0 0 1 0 0 0 1 4-4cyli.dat") Parser.parseFile
                in
                PartLoading.progress (PartLoading.mainAndSubfileParts model) model cache
                    |> Expect.equal { needed = [ "4-4cyli.dat" ], total = 4, settled = 2, inFlight = 1 }
        , test "resolverFromFlags treats a blank fallback as none" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        (PartLoading.resolverFromFlags { ldrawBase = "https://example.test/raw", ldrawFallbackBase = "  " }).fallbackBase
                            |> Expect.equal Nothing
                    , \_ ->
                        (PartLoading.resolverFromFlags { ldrawBase = "https://example.test/raw", ldrawFallbackBase = " /raw " }).fallbackBase
                            |> Expect.notEqual Nothing
                    ]
                    ()
        , test "uniqueNames keeps the first of each name in order" <|
            \_ ->
                PartLoading.uniqueNames [ "b", "a", "b", "c", "a" ]
                    |> Expect.equal [ "b", "a", "c" ]
        ]
