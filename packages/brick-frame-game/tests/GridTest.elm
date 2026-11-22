module GridTest exposing (suite)

{-| Tests for the player-view grid, its screen orientation and self-first
order, the session modes, room codes and room capacity on the wire.
-}

import Expect
import Game.Grid as Grid exposing (Orientation(..))
import Game.Multiplayer as Multiplayer exposing (SessionMode(..))
import Json.Decode as Decode
import Test exposing (Test, describe, test)


shapeOf : Orientation -> Int -> ( Int, Int )
shapeOf o n =
    let
        shape =
            Grid.gridShape o n
    in
    ( shape.cols, shape.rows )


suite : Test
suite =
    describe "Game.Grid"
        [ describe "gridShape"
            [ test "one player fills the screen in either orientation" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 1 ), ( 1, 1 ) ]
                        [ shapeOf Landscape 1, shapeOf Portrait 1 ]
            , test "two players split side by side in landscape and stacked in portrait" <|
                \_ ->
                    Expect.equal
                        [ ( 2, 1 ), ( 1, 2 ) ]
                        [ shapeOf Landscape 2, shapeOf Portrait 2 ]
            , test "three and four players share a 2 by 2 grid" <|
                \_ ->
                    Expect.equal
                        [ ( 2, 2 ), ( 2, 2 ), ( 2, 2 ), ( 2, 2 ) ]
                        [ shapeOf Landscape 3, shapeOf Portrait 3, shapeOf Landscape 4, shapeOf Portrait 4 ]
            , test "five and six players use 3 by 2 in landscape and 2 by 3 in portrait" <|
                \_ ->
                    Expect.equal
                        [ ( 3, 2 ), ( 3, 2 ), ( 2, 3 ), ( 2, 3 ) ]
                        [ shapeOf Landscape 5, shapeOf Landscape 6, shapeOf Portrait 5, shapeOf Portrait 6 ]
            , test "seven and eight players use 4 by 2 in landscape and 2 by 4 in portrait" <|
                \_ ->
                    Expect.equal
                        [ ( 4, 2 ), ( 4, 2 ), ( 2, 4 ), ( 2, 4 ) ]
                        [ shapeOf Landscape 7, shapeOf Landscape 8, shapeOf Portrait 7, shapeOf Portrait 8 ]
            , test "every grid has room for its players" <|
                \_ ->
                    let
                        fits o n =
                            let
                                ( c, r ) =
                                    shapeOf o n
                            in
                            c * r >= n && c * r < n + max c r
                    in
                    Expect.equal True
                        (List.all (\n -> fits Landscape n && fits Portrait n) (List.range 1 8))
            , test "counts above eight are clamped to the eight-player grid" <|
                \_ ->
                    Expect.equal (shapeOf Landscape 8) (shapeOf Landscape 12)
            , test "zero or negative counts show one view" <|
                \_ ->
                    Expect.equal (shapeOf Portrait 1) (shapeOf Portrait 0)
            ]
        , describe "layoutFor"
            [ test "one player is the single full view" <|
                \_ ->
                    Expect.equal [ Grid.SingleView, Grid.SingleView ]
                        [ Grid.layoutFor Landscape 1, Grid.layoutFor Portrait 0 ]
            , test "two or more players are a grid of their views" <|
                \_ ->
                    Expect.equal
                        [ Grid.Tiles { cols = 2, rows = 1 }, Grid.Tiles { cols = 1, rows = 2 }, Grid.Tiles { cols = 2, rows = 2 } ]
                        [ Grid.layoutFor Landscape 2, Grid.layoutFor Portrait 2, Grid.layoutFor Landscape 4 ]
            ]
        , describe "orientation"
            [ test "wider than tall is landscape, taller than wide is portrait" <|
                \_ ->
                    Expect.equal
                        [ Landscape, Portrait, Landscape ]
                        [ Grid.orientation 1280 720, Grid.orientation 390 844, Grid.orientation 500 500 ]
            ]
        , describe "selfFirst"
            [ test "puts the local player first and keeps the others in order" <|
                \_ ->
                    Expect.equal [ 3, 1, 2, 4 ]
                        (Grid.selfFirst identity 3 [ 1, 2, 3, 4 ])
            , test "leaves the list alone when the local player is not in it" <|
                \_ ->
                    Expect.equal [ 1, 2 ]
                        (Grid.selfFirst identity 9 [ 1, 2 ])
            ]
        , describe "room codes"
            [ test "a bare code is kept, in upper case" <|
                \_ ->
                    Expect.equal "AB12CD" (Multiplayer.roomCodeFrom " ab12cd ")
            , test "a room link gives its code" <|
                \_ ->
                    Expect.equal "XYZ987" (Multiplayer.roomCodeFrom "https://example.org/play/#room=xyz987")
            , test "a join link gives its code too" <|
                \_ ->
                    Expect.equal "Q1W2E3" (Multiplayer.roomCodeFrom "#join=q1w2e3")
            , test "stray characters are dropped and the code is capped" <|
                \_ ->
                    Expect.equal "ABCDEFGHIJKL" (Multiplayer.roomCodeFrom "abc-def gh ijk lmnop")
            ]
        , describe "session modes"
            [ test "two-player rooms hold two, grid sessions up to eight" <|
                \_ ->
                    Expect.equal [ 2, Grid.maxGridPlayers ]
                        [ Multiplayer.sessionCapacity TwoPlayers, Multiplayer.sessionCapacity GridSession ]
            , test "a grid session's capacity is exactly the largest grid" <|
                \_ ->
                    Expect.equal Grid.maxGridPlayers (Multiplayer.sessionCapacity GridSession)
            ]
        , describe "room capacity on the wire"
            [ test "a room state carries its capacity" <|
                \_ ->
                    let
                        json =
                            """{"roomId":"ABC123","role":"host","mySlot":1,"status":"lobby","countdown":3,"maxPlayers":8,"players":[]}"""
                    in
                    case Decode.decodeString Multiplayer.decodeRoomState json of
                        Ok room ->
                            Expect.equal 8 room.maxPlayers

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "a room from an older host holds the default four" <|
                \_ ->
                    let
                        json =
                            """{"roomId":"ABC123","role":"player","mySlot":2,"status":"lobby","countdown":3,"players":[]}"""
                    in
                    case Decode.decodeString Multiplayer.decodeRoomState json of
                        Ok room ->
                            Expect.equal Multiplayer.defaultCapacity room.maxPlayers

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            ]
        ]
