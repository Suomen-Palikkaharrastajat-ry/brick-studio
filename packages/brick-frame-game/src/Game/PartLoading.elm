module Game.PartLoading exposing
    ( PartProgress
    , fetchParts
    , flattenedParts
    , mainAndSubfileParts
    , progress
    , resolverFromFlags
    , uniqueNames
    )

{-| Part loading plumbing shared by the Brick Frame games.

A game keeps a `LDraw.Resolve.PartCache` and, for each build it wants to draw,
asks `progress` which part files are still missing. It marks those as loading
(`LDraw.Resolve.markLoading`), fetches them (`fetchParts`) and, after every
part that arrives, asks again. Once nothing is `needed` and nothing is
`inFlight`, every part of the build is in the cache and the game turns the
build into its own meshes.

-}

import Dict exposing (Dict)
import Http
import LDraw.Document as Document exposing (Document)
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..), ResolverConfig)
import LDraw.Types exposing (LDrawLine)
import Set exposing (Set)


{-| How far the parts of one build have loaded.

  - `needed`: part files the cache does not know yet; fetch them.
  - `total`: every part file the build references, through sub-models and
    already loaded parts.
  - `settled`: part files that loaded or failed.
  - `inFlight`: part files that are on their way.

-}
type alias PartProgress =
    { needed : List String
    , total : Int
    , settled : Int
    , inFlight : Int
    }


{-| Walk every part the build references, starting from `seeds`, through its
sub-models and already loaded part files, and report which ones the cache
does not have yet.
-}
progress : List String -> Document -> PartCache -> PartProgress
progress seeds doc cache =
    let
        subfileDict : Dict String (List LDrawLine)
        subfileDict =
            Document.subfiles doc
                |> Dict.fromList

        step : List String -> Set String -> PartProgress -> PartProgress
        step frontier visited acc =
            case frontier of
                [] ->
                    { acc | needed = List.reverse acc.needed }

                rawName :: rest ->
                    let
                        name : String
                        name =
                            normalizeName rawName
                    in
                    if String.isEmpty name || Set.member name visited then
                        step rest visited acc

                    else
                        let
                            nextVisited : Set String
                            nextVisited =
                                Set.insert name visited
                        in
                        case Dict.get name subfileDict of
                            Just subLines ->
                                step (Resolve.pendingParts subLines Dict.empty ++ rest) nextVisited acc

                            Nothing ->
                                case Dict.get name cache of
                                    Nothing ->
                                        step rest nextVisited { acc | needed = name :: acc.needed, total = acc.total + 1 }

                                    Just Loading ->
                                        step rest nextVisited { acc | total = acc.total + 1, inFlight = acc.inFlight + 1 }

                                    Just (Failed _) ->
                                        step rest nextVisited { acc | total = acc.total + 1, settled = acc.settled + 1 }

                                    Just (Loaded lines) ->
                                        step (Resolve.pendingParts lines Dict.empty ++ rest) nextVisited { acc | total = acc.total + 1, settled = acc.settled + 1 }
    in
    step seeds Set.empty { needed = [], total = 0, settled = 0, inFlight = 0 }


{-| Seeds for `progress`: the parts placed in the main model plus the parts
every sub-model references.
-}
mainAndSubfileParts : Document -> List String
mainAndSubfileParts doc =
    (Document.mainPlacements doc |> List.map .part)
        ++ (Document.subfiles doc |> List.concatMap (\( _, lines ) -> Resolve.pendingParts lines Dict.empty))


{-| Seeds for `progress`: every placement of the flattened model.
-}
flattenedParts : Document -> List String
flattenedParts doc =
    List.map .part (Document.flattenedPlacements doc)


normalizeName : String -> String
normalizeName raw =
    String.toLower (String.replace "\\" "/" (String.trim raw))


{-| Fetch part files from the catalog, one request per part.
-}
fetchParts : ResolverConfig -> (String -> Result Http.Error String -> msg) -> List String -> Cmd msg
fetchParts config toMsg names =
    if List.isEmpty names then
        Cmd.none

    else
        Cmd.batch (List.map (\name -> Resolve.fetchPart config name toMsg) names)


{-| Resolver for the `ldrawBase` and `ldrawFallbackBase` flags that
`game-boot.js` passes in. A blank fallback means none.
-}
resolverFromFlags : { flags | ldrawBase : String, ldrawFallbackBase : String } -> ResolverConfig
resolverFromFlags flags =
    let
        fallback : String
        fallback =
            String.trim flags.ldrawFallbackBase
    in
    Resolve.resolverConfig flags.ldrawBase
        (if String.isEmpty fallback then
            Nothing

         else
            Just fallback
        )


{-| Drop repeated names, keeping the first of each in order.
-}
uniqueNames : List String -> List String
uniqueNames names =
    List.foldl
        (\n ( seen, acc ) ->
            if Set.member n seen then
                ( seen, acc )

            else
                ( Set.insert n seen, n :: acc )
        )
        ( Set.empty, [] )
        names
        |> Tuple.second
        |> List.reverse
