module LDraw.ResolveTest exposing (suite)

{-| Unit tests for LDraw part resolution, candidate ordering, and cache updates.
-}

import Dict
import Expect
import Http
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartStatus(..), ResolverConfig, canUseBundle, defaultResolverConfig, partCandidateUrls, resolverConfig, updateCache)
import Test exposing (Test, describe, test)


testConfig : ResolverConfig
testConfig =
    { primaryBase = "/ldraw"
    , fallbackBase = Just "https://example.com/ldraw"
    , bundlesBase = Nothing
    }


suite : Test
suite =
    describe "LDraw.Resolve"
        [ describe "partCandidateUrls"
            [ test "standard numeric parts prioritize parts/ before p/" <|
                \_ ->
                    partCandidateUrls testConfig "3001.dat"
                        |> List.head
                        |> Expect.equal (Just "/ldraw/parts/3001.dat")
            , test "technic gear parts prioritize parts/ before p/" <|
                \_ ->
                    partCandidateUrls testConfig "3647.dat"
                        |> List.head
                        |> Expect.equal (Just "/ldraw/parts/3647.dat")
            , test "stud primitive prioritizes p/ before parts/" <|
                \_ ->
                    partCandidateUrls testConfig "stud.dat"
                        |> List.head
                        |> Expect.equal (Just "/ldraw/p/stud.dat")
            , test "fractional cylinder primitive prioritizes p/ before parts/" <|
                \_ ->
                    partCandidateUrls testConfig "1-4cyli.dat"
                        |> List.head
                        |> Expect.equal (Just "/ldraw/p/1-4cyli.dat")
            , test "subpart prefix routes directly to parts/s/" <|
                \_ ->
                    partCandidateUrls testConfig "s/3001s01.dat"
                        |> Expect.equal
                            [ "/ldraw/parts/s/3001s01.dat"
                            , "https://example.com/ldraw/parts/s/3001s01.dat"
                            ]
            , test "high-res primitive prefix routes directly to p/48/" <|
                \_ ->
                    partCandidateUrls testConfig "48/stud.dat"
                        |> Expect.equal
                            [ "/ldraw/p/48/stud.dat"
                            , "https://example.com/ldraw/p/48/stud.dat"
                            ]
            ]
        , describe "resolverConfig"
            [ test "infers bundlesBase from /raw primaryBase" <|
                \_ ->
                    resolverConfig "https://catalog.palikkaharrastajat.fi/raw" Nothing
                        |> .bundlesBase
                        |> Expect.equal (Just "https://catalog.palikkaharrastajat.fi/bundles")
            , test "infers bundlesBase from /raw/ with trailing slash" <|
                \_ ->
                    resolverConfig "https://catalog.palikkaharrastajat.fi/raw/" Nothing
                        |> .bundlesBase
                        |> Expect.equal (Just "https://catalog.palikkaharrastajat.fi/bundles")
            , test "does not infer bundlesBase for non-raw path" <|
                \_ ->
                    resolverConfig "/ldraw" Nothing
                        |> .bundlesBase
                        |> Expect.equal Nothing
            , test "uses default primaryBase when empty" <|
                \_ ->
                    resolverConfig "" Nothing
                        |> .primaryBase
                        |> Expect.equal "/ldraw"
            ]
        , describe "canUseBundle"
            [ test "allows standard numeric top-level parts" <|
                \_ ->
                    canUseBundle "3001.dat"
                        |> Expect.equal True
            , test "allows alphanumeric top-level parts" <|
                \_ ->
                    canUseBundle "3647.dat"
                        |> Expect.equal True
            , test "allows SNOT top-level parts" <|
                \_ ->
                    canUseBundle "87087.dat"
                        |> Expect.equal True
            , test "allows part names with parts/ prefix" <|
                \_ ->
                    canUseBundle "parts/3001.dat"
                        |> Expect.equal True
            , test "rejects stud primitives" <|
                \_ ->
                    canUseBundle "stud.dat"
                        |> Expect.equal False
            , test "rejects subparts with s/ prefix" <|
                \_ ->
                    canUseBundle "s/3001s01.dat"
                        |> Expect.equal False
            , test "rejects subparts with parts/s/ prefix" <|
                \_ ->
                    canUseBundle "parts/s/3001s01.dat"
                        |> Expect.equal False
            , test "rejects high-res primitives with 48/ prefix" <|
                \_ ->
                    canUseBundle "48/stud.dat"
                        |> Expect.equal False
            , test "rejects primitives with p/ prefix" <|
                \_ ->
                    canUseBundle "p/stud.dat"
                        |> Expect.equal False
            , test "rejects fractional cylinder primitives" <|
                \_ ->
                    canUseBundle "1-4cyli.dat"
                        |> Expect.equal False
            , test "rejects empty string" <|
                \_ ->
                    canUseBundle ""
                        |> Expect.equal False
            ]
        , describe "updateCache"
            [ test "seeds all embedded subfiles from MPD text" <|
                \_ ->
                    let
                        mpdContent =
                            "0 FILE 3001.dat\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud.dat\n0 FILE stud.dat\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 4-4cyli.dat\n"

                        cache =
                            Resolve.initCache
                                |> updateCache "3001.dat" (Ok mpdContent) Parser.parseFile
                    in
                    case ( Dict.get "3001.dat" cache, Dict.get "stud.dat" cache ) of
                        ( Just (Loaded _), Just (Loaded _) ) ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected both 3001.dat and stud.dat to be Loaded in cache"
            , test "updates single part when given standard DAT content" <|
                \_ ->
                    let
                        datContent =
                            "0 Brick 2 x 4\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud.dat\n"

                        cache =
                            Resolve.initCache
                                |> updateCache "3001.dat" (Ok datContent) Parser.parseFile
                    in
                    case ( Dict.get "3001.dat" cache, Dict.get "stud.dat" cache ) of
                        ( Just (Loaded _), Nothing ) ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected only 3001.dat to be Loaded"
            , test "records failed HTTP status in cache" <|
                \_ ->
                    let
                        cache =
                            Resolve.initCache
                                |> updateCache "3001.dat" (Err (Http.BadStatus 404)) Parser.parseFile
                    in
                    case Dict.get "3001.dat" cache of
                        Just (Failed msg) ->
                            Expect.equal "HTTP 404" msg

                        _ ->
                            Expect.fail "Expected 3001.dat to be Failed"
            ]
        ]
