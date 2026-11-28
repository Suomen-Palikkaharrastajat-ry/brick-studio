module Game.BuildHistory exposing
    ( Entry, History
    , empty, maxEntries, record, remove
    , entries, latest, member
    , toJson, fromJson
    )

{-| The build history of a game's vehicle editor (the ship editor of Brick
Sails, the car editor of Brick Racer): the last builds the player made, newest
first, kept in the browser so a reload, a closed tab or a discarded phone tab
does not lose them.

An entry holds the build as a build link (`code`, the same `#b/...` the game
shares, so it is restored with the game's own link decoder), the starter base
it stands on, its part count and when it was saved.

Entries are grouped by `session`: a save in the same session as the newest
entry replaces it (the autosave of the build being edited), a save in a new
session is added in front. The editor starts a new session at its boundaries
(opening, switching base, restoring an entry, a drawer tab change), so the
history keeps one entry per finished stretch of editing. The same build is
never listed twice, and only the newest `maxEntries` are kept.

@docs Entry, History
@docs empty, maxEntries, record, remove
@docs entries, latest, member
@docs toJson, fromJson

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| One saved build: its build link, the id of its starter base (`""` when
none), how many parts it has, when it was saved (ms since the epoch) and the
editing session that saved it.
-}
type alias Entry =
    { code : String
    , base : String
    , parts : Int
    , savedAt : Int
    , session : Int
    }


{-| Saved builds, newest first.
-}
type History
    = History (List Entry)


{-| No builds yet.
-}
empty : History
empty =
    History []


{-| How many builds the history keeps.
-}
maxEntries : Int
maxEntries =
    20


{-| Save a build. An empty build link is not saved. An entry with the same
build is dropped first; then the build replaces the newest entry when that is
from the same session, else it goes in front. The oldest entries beyond
`maxEntries` are dropped.
-}
record : Entry -> History -> History
record entry (History list) =
    if String.isEmpty entry.code then
        History list

    else
        let
            others =
                List.filter (\e -> e.code /= entry.code) list

            rest =
                case others of
                    newest :: older ->
                        if newest.session == entry.session then
                            older

                        else
                            others

                    [] ->
                        []
        in
        History (List.take maxEntries (entry :: rest))


{-| Drop the entry with this build link.
-}
remove : String -> History -> History
remove code (History list) =
    History (List.filter (\e -> e.code /= code) list)


{-| The saved builds, newest first.
-}
entries : History -> List Entry
entries (History list) =
    list


{-| The newest saved build.
-}
latest : History -> Maybe Entry
latest (History list) =
    List.head list


{-| Whether this build link is saved.
-}
member : String -> History -> Bool
member code (History list) =
    List.any (\e -> e.code == code) list


{-| The history as JSON text, for localStorage.
-}
toJson : History -> String
toJson (History list) =
    Encode.encode 0
        (Encode.object
            [ ( "version", Encode.int 1 )
            , ( "entries", Encode.list encodeEntry list )
            ]
        )


encodeEntry : Entry -> Encode.Value
encodeEntry e =
    Encode.object
        [ ( "code", Encode.string e.code )
        , ( "base", Encode.string e.base )
        , ( "parts", Encode.int e.parts )
        , ( "savedAt", Encode.int e.savedAt )
        , ( "session", Encode.int e.session )
        ]


{-| Read the history saved by `toJson`. Anything unreadable (no saved
history, another format, a damaged entry) gives what can be read: an empty
history, or the readable entries, still bounded and without repeats.
-}
fromJson : String -> History
fromJson json =
    case Decode.decodeString (Decode.field "entries" (Decode.list (Decode.maybe entryDecoder))) json of
        Ok found ->
            List.filterMap identity found
                |> firstOfEach
                |> List.take maxEntries
                |> History

        Err _ ->
            empty


{-| Keep the first entry of each build link, in order.
-}
firstOfEach : List Entry -> List Entry
firstOfEach list =
    List.foldl
        (\e acc ->
            if List.any (\x -> x.code == e.code) acc then
                acc

            else
                e :: acc
        )
        []
        list
        |> List.reverse


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map5 Entry
        (Decode.field "code" Decode.string)
        (Decode.oneOf [ Decode.field "base" Decode.string, Decode.succeed "" ])
        (Decode.oneOf [ Decode.field "parts" Decode.int, Decode.succeed 0 ])
        (Decode.oneOf [ Decode.field "savedAt" Decode.int, Decode.succeed 0 ])
        (Decode.oneOf [ Decode.field "session" Decode.int, Decode.succeed 0 ])
        |> Decode.andThen
            (\e ->
                if String.isEmpty e.code then
                    Decode.fail "empty build"

                else
                    Decode.succeed e
            )
