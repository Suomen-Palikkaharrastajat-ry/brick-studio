module Game.Leaderboard exposing
    ( Leaderboard
    , LeaderboardEntry
    , bestForCourse
    , decodeLeaderboard
    , empty
    , encodeLeaderboard
    , entriesForCourse
    , formatRaceTime
    , formatSplitDelta
    , recordRun
    )

{-| Universal high score and course record storage, timing formatting, and JSON codecs.
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A single timed run entry.
-}
type alias LeaderboardEntry =
    { courseId : String
    , player : String
    , timeSeconds : Float
    , isoDate : String
    , vehicleName : String
    }


{-| Persistent leaderboard collection.
-}
type alias Leaderboard =
    { entries : List LeaderboardEntry
    }


{-| An empty leaderboard.
-}
empty : Leaderboard
empty =
    { entries = [] }


{-| Record a new run, keeping only the top 10 fastest times per course.
-}
recordRun : LeaderboardEntry -> Leaderboard -> Leaderboard
recordRun newEntry board =
    let
        courseEntries =
            board.entries
                |> List.filter (\e -> e.courseId == newEntry.courseId)

        otherEntries =
            board.entries
                |> List.filter (\e -> e.courseId /= newEntry.courseId)

        updatedCourse =
            (newEntry :: courseEntries)
                |> List.sortBy .timeSeconds
                |> List.take 10
    in
    { entries = otherEntries ++ updatedCourse }


{-| Retrieve all entries for a specific course sorted fastest to slowest.
-}
entriesForCourse : String -> Leaderboard -> List LeaderboardEntry
entriesForCourse cid board =
    board.entries
        |> List.filter (\e -> e.courseId == cid)
        |> List.sortBy .timeSeconds


{-| Retrieve the best (fastest) time for a course, if one exists.
-}
bestForCourse : String -> Leaderboard -> Maybe LeaderboardEntry
bestForCourse cid board =
    entriesForCourse cid board
        |> List.head


{-| Format elapsed seconds into `MM:SS.cs` (e.g. `01:23.45`).
-}
formatRaceTime : Float -> String
formatRaceTime seconds =
    if seconds < 0.0 then
        "--:--.--"

    else
        let
            totalCentis =
                round (seconds * 100.0)

            mins =
                totalCentis // 6000

            remCentis =
                modBy 6000 totalCentis

            secs =
                remCentis // 100

            centis =
                modBy 100 remCentis

            padZero n =
                if n < 10 then
                    "0" ++ String.fromInt n

                else
                    String.fromInt n
        in
        padZero mins ++ ":" ++ padZero secs ++ "." ++ padZero centis


{-| Format a split delta in seconds (e.g. `+00.23` or `-01.12`).
-}
formatSplitDelta : Float -> String
formatSplitDelta delta =
    let
        sign =
            if delta < -0.001 then
                "-"

            else
                "+"

        absSecs =
            abs delta

        totalCentis =
            round (absSecs * 100.0)

        secs =
            totalCentis // 100

        centis =
            modBy 100 totalCentis

        padZero n =
            if n < 10 then
                "0" ++ String.fromInt n

            else
                String.fromInt n
    in
    sign ++ padZero secs ++ "." ++ padZero centis


{-| JSON encoder for Leaderboard.
-}
encodeLeaderboard : Leaderboard -> Encode.Value
encodeLeaderboard board =
    Encode.object
        [ ( "entries", Encode.list encodeEntry board.entries )
        ]


encodeEntry : LeaderboardEntry -> Encode.Value
encodeEntry entry =
    Encode.object
        [ ( "courseId", Encode.string entry.courseId )
        , ( "player", Encode.string entry.player )
        , ( "timeSeconds", Encode.float entry.timeSeconds )
        , ( "isoDate", Encode.string entry.isoDate )
        , ( "vehicleName", Encode.string entry.vehicleName )
        ]


{-| JSON decoder for Leaderboard.
-}
decodeLeaderboard : Decoder Leaderboard
decodeLeaderboard =
    Decode.map Leaderboard
        (Decode.field "entries" (Decode.list decodeEntry))


decodeEntry : Decoder LeaderboardEntry
decodeEntry =
    Decode.map5 LeaderboardEntry
        (Decode.field "courseId" Decode.string)
        (Decode.field "player" Decode.string)
        (Decode.field "timeSeconds" Decode.float)
        (Decode.field "isoDate" Decode.string)
        (Decode.field "vehicleName" Decode.string)
