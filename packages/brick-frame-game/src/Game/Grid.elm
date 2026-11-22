module Game.Grid exposing
    ( GridShape
    , Orientation(..)
    , gridShape
    , maxGridPlayers
    , orientation
    , selfFirst
    )

{-| Layout of the player views on one shared screen: one full view for a
single player, and a grid of player views that fills the screen for two or
more (2-player split screen, and grid sessions of up to eight players).

The table, by number of player views shown:

    players landscape portrait

    1 1 x 1 1 x 1

    2 2 cols x 1 1 col x 2

    3 - 4 2 x 2 2 x 2

    5 - 6 3 cols x 2 2 cols x 3

    7 - 8 4 cols x 2 2 cols x 4

Side by side in landscape, stacked in portrait. Counts above eight are
clamped to eight.

-}


{-| Screen shape: landscape when wider than tall (ties count as landscape).
-}
type Orientation
    = Landscape
    | Portrait


{-| Number of columns and rows of the player-view grid.
-}
type alias GridShape =
    { cols : Int
    , rows : Int
    }


{-| The most player views one grid holds.
-}
maxGridPlayers : Int
maxGridPlayers =
    8


{-| Orientation of a screen of the given size in CSS pixels.
-}
orientation : Int -> Int -> Orientation
orientation width height =
    if width >= height then
        Landscape

    else
        Portrait


{-| Grid of `count` player views in the given orientation.
-}
gridShape : Orientation -> Int -> GridShape
gridShape o count =
    case clamp 1 maxGridPlayers count of
        1 ->
            { cols = 1, rows = 1 }

        2 ->
            case o of
                Landscape ->
                    { cols = 2, rows = 1 }

                Portrait ->
                    { cols = 1, rows = 2 }

        3 ->
            { cols = 2, rows = 2 }

        4 ->
            { cols = 2, rows = 2 }

        5 ->
            wideOrTall o 3 2

        6 ->
            wideOrTall o 3 2

        7 ->
            wideOrTall o 4 2

        _ ->
            wideOrTall o 4 2


{-| `long` by `short` in landscape, `short` by `long` in portrait.
-}
wideOrTall : Orientation -> Int -> Int -> GridShape
wideOrTall o long short =
    case o of
        Landscape ->
            { cols = long, rows = short }

        Portrait ->
            { cols = short, rows = long }


{-| The entries in order with the local player's first (their own view sits
top left or first in the stack), the rest in their existing order.
-}
selfFirst : (a -> Int) -> Int -> List a -> List a
selfFirst slotOf mySlot items =
    let
        ( mine, others ) =
            List.partition (\item -> slotOf item == mySlot) items
    in
    mine ++ others
