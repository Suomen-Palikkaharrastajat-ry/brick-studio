module GameTest exposing (suite)

{-| Unit tests for Game Framework modules (Camera, Compass, Leaderboard, Multiplayer).
-}

import Expect
import Game.Camera as Camera
import Game.Compass as Compass exposing (PointOfSail(..))
import Game.Leaderboard as Leaderboard
import Game.Multiplayer as Multiplayer exposing (Role(..), RoomStatus(..))
import Json.Decode as Decode
import Math.Vector3 exposing (vec3)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Game Framework Tests"
        [ describe "Compass & Navigation"
            [ test "headingFromVector calculates cardinal directions" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.1) 0.0 (Compass.headingFromVector 0 1)
                        , \_ -> Expect.within (Expect.Absolute 0.1) 90.0 (Compass.headingFromVector 1 0)
                        , \_ -> Expect.within (Expect.Absolute 0.1) 180.0 (Compass.headingFromVector 0 -1)
                        , \_ -> Expect.within (Expect.Absolute 0.1) 270.0 (Compass.headingFromVector -1 0)
                        ]
                        ()
            , test "cardinalName matches headings" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "N" (Compass.cardinalName 0)
                        , \_ -> Expect.equal "E" (Compass.cardinalName 90)
                        , \_ -> Expect.equal "S" (Compass.cardinalName 180)
                        , \_ -> Expect.equal "W" (Compass.cardinalName 270)
                        , \_ -> Expect.equal "NE" (Compass.cardinalName 45)
                        , \_ -> Expect.equal "SW" (Compass.cardinalName 225)
                        ]
                        ()
            , test "apparentWind computes headwind correctly" <|
                \_ ->
                    let
                        -- Wind blowing south (180°), ship heading north (0°) at 5 m/s, wind speed 10 m/s
                        app =
                            Compass.apparentWind
                                { windSpeed = 10.0, windHeading = 180.0 }
                                { shipSpeed = 5.0, shipHeading = 0.0 }
                    in
                    -- Headwind of 15 m/s directly from front
                    Expect.within (Expect.Absolute 0.2) 15.0 app.speed
            , test "pointOfSail classifies correctly" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal InIrons (Compass.pointOfSail 20)
                        , \_ -> Expect.equal CloseHauled (Compass.pointOfSail 50)
                        , \_ -> Expect.equal BeamReach (Compass.pointOfSail 90)
                        , \_ -> Expect.equal BroadReach (Compass.pointOfSail 135)
                        , \_ -> Expect.equal Running (Compass.pointOfSail 175)
                        ]
                        ()
            ]
        , describe "Leaderboard"
            [ test "formats time correctly" <|
                \_ ->
                    Expect.equal "01:23.45" (Leaderboard.formatRaceTime 83.45)
            , test "formats split delta correctly" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "+00.23" (Leaderboard.formatSplitDelta 0.23)
                        , \_ -> Expect.equal "-01.12" (Leaderboard.formatSplitDelta -1.12)
                        ]
                        ()
            , test "records and sorts runs by fastest time" <|
                \_ ->
                    let
                        b1 =
                            Leaderboard.recordRun
                                { courseId = "regatta", player = "Alice", timeSeconds = 60.0, isoDate = "2026-11-22", vehicleName = "Sloop" }
                                Leaderboard.empty

                        b2 =
                            Leaderboard.recordRun
                                { courseId = "regatta", player = "Bob", timeSeconds = 55.0, isoDate = "2026-11-22", vehicleName = "Clipper" }
                                b1

                        best =
                            Leaderboard.bestForCourse "regatta" b2
                    in
                    case best of
                        Just entry ->
                            Expect.equal "Bob" entry.player

                        Nothing ->
                            Expect.fail "Expected best entry"
            ]
        , describe "Camera"
            [ test "damp reaches target over time" <|
                \_ ->
                    let
                        val =
                            Camera.damp 5.0 0.0 10.0 1.0
                    in
                    Expect.greaterThan 9.0 val
            ]
        , describe "Multiplayer"
            [ test "decodes room state JSON correctly" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "roomId": "RACE99",
                              "role": "host",
                              "mySlot": 1,
                              "status": "in_race",
                              "countdown": 0,
                              "players": [
                                {
                                  "slot": 1,
                                  "id": "host",
                                  "name": "Host Racer",
                                  "vehicleHash": "#b/123",
                                  "position": [10.0, 2.0, 30.0],
                                  "rotation": [0.5, 0.0, 0.0],
                                  "speed": 12.5,
                                  "progress": 45.0,
                                  "finished": false
                                }
                              ]
                            }
                            """
                    in
                    case Decode.decodeString Multiplayer.decodeRoomState json of
                        Ok st ->
                            Expect.all
                                [ \s -> Expect.equal "RACE99" s.roomId
                                , \s -> Expect.equal HostCommentator s.role
                                , \s -> Expect.equal 1 (List.length s.players)
                                , \s -> Expect.equal [ "" ] (List.map .vehicleId s.players)
                                ]
                                st

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes vehicle ids and flags" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "roomId": "RACE99",
                              "role": "player",
                              "mySlot": 2,
                              "status": "lobby",
                              "countdown": 3,
                              "players": [
                                {
                                  "slot": 1,
                                  "id": "host",
                                  "name": "Host",
                                  "vehicleId": "0123456789abcdef",
                                  "vehicleReady": true,
                                  "vehicleLoaded": false,
                                  "position": [0, 0, 0],
                                  "rotation": [0, 0, 0],
                                  "speed": 0,
                                  "progress": 0,
                                  "finished": false,
                                  "finishTime": null
                                }
                              ]
                            }
                            """
                    in
                    case Decode.decodeString Multiplayer.decodeRoomState json of
                        Ok st ->
                            Expect.equal [ ( "0123456789abcdef", True, False ) ]
                                (List.map (\p -> ( p.vehicleId, p.vehicleReady, p.vehicleLoaded )) st.players)

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "slotColor provides distinct colors" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.notEqual (Multiplayer.slotColor 1) (Multiplayer.slotColor 2)
                        , \_ -> Expect.notEqual (Multiplayer.slotColor 2) (Multiplayer.slotColor 3)
                        , \_ -> Expect.notEqual (Multiplayer.slotColor 3) (Multiplayer.slotColor 4)
                        ]
                        ()
            ]
        ]
