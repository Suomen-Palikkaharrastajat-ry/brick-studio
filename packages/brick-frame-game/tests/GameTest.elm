module GameTest exposing (suite)

import Expect
import Game.Camera as Camera
import Game.Compass as Compass exposing (PointOfSail(..))
import Game.Leaderboard as Leaderboard
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
        ]
