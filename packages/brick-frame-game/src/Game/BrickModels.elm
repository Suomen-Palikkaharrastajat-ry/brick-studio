module Game.BrickModels exposing
    ( ArchBrick, Turn(..), all, archBricks, singlePart
    , leafyTree, leafyTreeIn, pineTree, pineTreeIn, bush, fence, rock, signpost, hayBale, hayStack, cottage, barn, tireBarrier
    , buoy, beacon, floatingTarget, island, lighthouse, cannonball, splash, palmTree
    )

{-| The props of the Brick Frame games, as LDraw models built from parts of
the brick catalog.

Every model is plain LDraw text: one type-1 line per part, with the usual
LDraw conventions (-Y is up, 1 stud = 20 LDU, 1 brick = 24 LDU, a part's
origin at the top of its body). The ground is at y = 0 and every model is
centred on x = z = 0. Parts in colour 16 take the colour the game draws the
model in (`Game.BrickProps.draw`'s tint); every other part keeps its own.

Both games load these through `Game.BrickProps`, which resolves the parts
through the catalog like any other build, so a tree or a fence is the same
bricks in Brick Racer and Brick Sails. The models are small on purpose: a
track shows a few hundred of them at once, also on phones.

Which side is the front: LDraw -Z, which is +Z once drawn (the doors of the
cottage and the barn, the face of a sign). Fences run along Z.

@docs ArchBrick, Turn, all, archBricks, singlePart
@docs leafyTree, leafyTreeIn, pineTree, pineTreeIn, bush, fence, rock, signpost, hayBale, hayStack, cottage, barn, tireBarrier
@docs buoy, beacon, floatingTarget, island, lighthouse, cannonball, splash, palmTree

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (vec3)



-- LDRAW LINES


{-| A part's turn about the vertical axis, in quarter turns. `Quarter` turns
a part's -Z side to -X (and its long X side along Z).
-}
type Turn
    = Straight
    | Quarter
    | Half
    | ThreeQuarter


matrixOf : Turn -> List String
matrixOf turn =
    case turn of
        Straight ->
            [ "1", "0", "0", "0", "1", "0", "0", "0", "1" ]

        Quarter ->
            [ "0", "0", "1", "0", "1", "0", "-1", "0", "0" ]

        Half ->
            [ "-1", "0", "0", "0", "1", "0", "0", "0", "-1" ]

        ThreeQuarter ->
            [ "0", "0", "-1", "0", "1", "0", "1", "0", "0" ]


{-| One type-1 line: a part in a colour at an LDraw position.
-}
part : Int -> ( Float, Float, Float ) -> Turn -> String -> String
part colour ( x, y, z ) turn file =
    String.join " " ([ "1", String.fromInt colour, String.fromFloat x, String.fromFloat y, String.fromFloat z ] ++ matrixOf turn ++ [ file ])


{-| A part scaled in plan (`sxz`), for the tiers of an island.
-}
scaledPart : Int -> ( Float, Float, Float ) -> Float -> String -> String
scaledPart colour ( x, y, z ) sxz file =
    let
        s =
            String.fromFloat sxz
    in
    String.join " " [ "1", String.fromInt colour, String.fromFloat x, String.fromFloat y, String.fromFloat z, s, "0", "0", "0", "1", "0", "0", "0", s, file ]


model : String -> List String -> String
model name lines =
    String.join "\n" (("0 " ++ name) :: "0 Author: Brick Frame games" :: "0 BFC CERTIFY CCW" :: lines)


{-| The origin height of a brick whose body sits on `layer` bricks (and
`plates` plates) above the ground.
-}
onLayer : Int -> Float
onLayer layer =
    -24 * toFloat (layer + 1)


{-| A model of one part in colour 16, so it takes the tint it is drawn in.
Used for loose debris bricks and for the bricks of an arch.
-}
singlePart : String -> String
singlePart file =
    model ("Single " ++ file) [ part 16 ( 0, 0, 0 ) Straight file ]


{-| Matrices of a part placed without a quarter turn: upright, half turned,
and lying along Z (the palm's trunk and fronds use these and tilted ones).
-}
uprightM : String
uprightM =
    "1 0 0 0 1 0 0 0 1"


turnedM : String
turnedM =
    "-1 0 0 0 1 0 0 0 -1"


alongZM : String
alongZM =
    "0 0 1 0 1 0 -1 0 0"


{-| One type-1 line with a raw matrix, for parts that are not quarter turns.
-}
placed : Int -> ( Float, Float, Float ) -> String -> String -> String
placed colour ( x, y, z ) matrix file =
    String.join " "
        [ "1"
        , String.fromInt colour
        , String.fromFloat x
        , String.fromFloat y
        , String.fromFloat z
        , matrix
        , file
        ]


{-| A palm tree: a leaning trunk of reddish-brown 1 x 1 bricks, coconuts of
dark tan round bricks under the crown, and eight green 2 x 2 slopes drooping
out from a green plate as the fronds.
-}
palmTree : String
palmTree =
    let
        crownX =
            18

        frond colour ( x, y, z ) matrix =
            placed colour ( crownX + x, y, z ) matrix "3039.dat"
    in
    model "Palm tree"
        [ "0 // Trunk"
        , placed 70 ( 0, -24, 0 ) uprightM "3005.dat"
        , placed 70 ( 0, -48, 0 ) uprightM "3005.dat"
        , placed 70 ( 3, -72, 0 ) uprightM "3005.dat"
        , placed 70 ( 7, -96, 0 ) uprightM "3005.dat"
        , placed 70 ( 12, -120, 0 ) uprightM "3005.dat"
        , placed 70 ( crownX, -144, 0 ) uprightM "3005.dat"
        , "0 // Coconuts"
        , placed 28 ( crownX, -128, -20 ) uprightM "3062b.dat"
        , placed 28 ( crownX + 20, -128, 0 ) uprightM "3062b.dat"
        , placed 28 ( crownX - 20, -128, 0 ) uprightM "3062b.dat"
        , "0 // Crown and fronds"
        , placed 2 ( crownX, -152, 0 ) uprightM "3022.dat"
        , frond 2 ( 0, -152, -40 ) uprightM
        , frond 2 ( 0, -152, 40 ) turnedM
        , frond 2 ( -40, -152, 0 ) alongZM
        , frond 2 ( 40, -152, 0 ) "0 0 -1 0 1 0 1 0 0"
        , frond 10 ( -28, -144, -28 ) "0.7071 0 0.7071 0 1 0 -0.7071 0 0.7071"
        , frond 10 ( -28, -144, 28 ) "-0.7071 0 0.7071 0 1 0 -0.7071 0 -0.7071"
        , frond 10 ( 28, -144, -28 ) "0.7071 0 -0.7071 0 1 0 0.7071 0 0.7071"
        , frond 10 ( 28, -144, 28 ) "-0.7071 0 -0.7071 0 1 0 0.7071 0 -0.7071"
        , placed 10 ( crownX, -160, 0 ) uprightM "3024.dat"
        ]


{-| Every model with its name, for loading them all and for the tests.
-}
all : List ( String, String )
all =
    [ ( "leafyTree", leafyTree )
    , ( "autumnLeafyTree", leafyTreeIn { leaves = darkOrange } )
    , ( "pineTree", pineTree )
    , ( "snowyPineTree", pineTreeIn { needles = darkGreen, cap = Just white } )
    , ( "bush", bush )
    , ( "fence", fence )
    , ( "rock", rock )
    , ( "signpost", signpost )
    , ( "hayBale", hayBale )
    , ( "hayStack", hayStack )
    , ( "cottage", cottage )
    , ( "barn", barn )
    , ( "tireBarrier", tireBarrier )
    , ( "buoy", buoy )
    , ( "beacon", beacon )
    , ( "floatingTarget", floatingTarget )
    , ( "island", island )
    , ( "lighthouse", lighthouse )
    , ( "cannonball", cannonball )
    , ( "splash", splash )
    , ( "palmTree", palmTree )
    ]



-- COLOURS


reddishBrown : Int
reddishBrown =
    70


green : Int
green =
    2


brightGreen : Int
brightGreen =
    10


darkGreen : Int
darkGreen =
    288


darkOrange : Int
darkOrange =
    484


tan : Int
tan =
    19


darkTan : Int
darkTan =
    28


white : Int
white =
    15


red : Int
red =
    4


darkBluishGrey : Int
darkBluishGrey =
    72


lightBluishGrey : Int
lightBluishGrey =
    71



-- COUNTRYSIDE


{-| A leafy tree: a trunk of three 1 x 1 bricks under a crown of 1 x 4 bricks
laid crosswise, topped with a 2 x 2 brick. The summer tree, in green leaves
(`leafyTreeIn { leaves = 2 }`).
-}
leafyTree : String
leafyTree =
    leafyTreeIn { leaves = green }


{-| The leafy tree with its crown in another colour, such as dark orange
(484) for autumn. The 2 x 4 brick in the crown is bright green on a green
tree and the leaf colour on any other.
-}
leafyTreeIn : { leaves : Int } -> String
leafyTreeIn { leaves } =
    let
        accent =
            if leaves == green then
                brightGreen

            else
                leaves
    in
    model "Leafy tree"
        (List.map (\layer -> part reddishBrown ( 0, onLayer layer, 0 ) Straight "3005.dat") [ 0, 1, 2 ]
            ++ List.map (\z -> part leaves ( 0, onLayer 3, z ) Straight "3010.dat") [ -30, -10, 10, 30 ]
            ++ List.map (\x -> part leaves ( x, onLayer 4, 0 ) Quarter "3010.dat") [ -30, -10, 10, 30 ]
            ++ [ part accent ( 0, onLayer 5, 0 ) Straight "3001.dat"
               , part leaves ( 0, onLayer 6, 0 ) Straight "3003.dat"
               ]
        )


{-| A pine tree: the catalog's large pine, in dark green
(`pineTreeIn { needles = 288, cap = Nothing }`).
-}
pineTree : String
pineTree =
    pineTreeIn { needles = darkGreen, cap = Nothing }


{-| The pine in other colours. `cap = Just colour` puts a 2 x 2 x 2 cone in
that colour over the top tier, such as white (15) for snow in winter: the
cone's foot sits 112 LDU up the 136 LDU pine and its tip 24 LDU above the
pine's, so the snow covers the top of the tree.
-}
pineTreeIn : { needles : Int, cap : Maybe Int } -> String
pineTreeIn { needles, cap } =
    model "Pine tree"
        (part needles ( 0, 0, 0 ) Straight "3471.dat"
            :: (case cap of
                    Just colour ->
                        [ part colour ( 0, -160, 0 ) Straight "3942c.dat" ]

                    Nothing ->
                        []
               )
        )


{-| A bush: a 2 x 4 brick with a 1 x 2 brick and a flower on top.
-}
bush : String
bush =
    model "Bush"
        [ part green ( 0, onLayer 0, 0 ) Straight "3001.dat"
        , part brightGreen ( 0, onLayer 1, 10 ) Straight "3004.dat"
        , part 14 ( -30, onLayer 0, -10 ) Straight "3742.dat"
        ]


{-| A fence section along Z: a white lattice between two posts of a 1 x 1
brick and a 1 x 1 plate.
-}
fence : String
fence =
    model "Fence"
        (List.concatMap
            (\z ->
                [ part reddishBrown ( 0, onLayer 0, z ) Straight "3005.dat"
                , part reddishBrown ( 0, -32, z ) Straight "3024.dat"
                ]
            )
            [ -50, 50 ]
            ++ [ part white ( 0, onLayer 0, 0 ) Quarter "3633.dat" ]
        )


{-| A rock: a dark grey 2 x 4 brick with two grey slopes back to back on top.
-}
rock : String
rock =
    model "Rock"
        [ part darkBluishGrey ( 0, onLayer 0, 0 ) Straight "3001.dat"
        , part lightBluishGrey ( -10, onLayer 1, 0 ) Quarter "3039.dat"
        , part lightBluishGrey ( 10, onLayer 1, 0 ) ThreeQuarter "3039.dat"
        ]


{-| A signpost: a post of three 1 x 1 bricks carrying a yellow board of two
1 x 4 bricks with a red plate between them. The board faces Z.
-}
signpost : String
signpost =
    model "Signpost"
        (List.map (\layer -> part reddishBrown ( 0, onLayer layer, 0 ) Straight "3005.dat") [ 0, 1, 2 ]
            ++ [ part 14 ( 0, onLayer 3, 0 ) Straight "3010.dat"
               , part red ( 0, onLayer 3 - 8, 0 ) Straight "3710.dat"
               , part 14 ( 0, onLayer 4 - 8, 0 ) Straight "3010.dat"
               ]
        )


{-| One hay bale: two tan 2 x 4 bricks with a dark tan plate of twine between.
-}
hayBale : String
hayBale =
    model "Hay bale" (bale 0)


bale : Float -> List String
bale z =
    [ part tan ( 0, onLayer 0, z ) Straight "3001.dat"
    , part darkTan ( 0, onLayer 0 - 8, z ) Straight "3020.dat"
    , part tan ( 0, onLayer 1 - 8, z ) Straight "3001.dat"
    ]


{-| Three hay bales: two side by side and one across on top.
-}
hayStack : String
hayStack =
    model "Hay stack"
        (bale -20
            ++ bale 20
            ++ [ part tan ( 0, onLayer 2 - 8, 0 ) Quarter "3001.dat" ]
        )


{-| A cottage, 6 x 4 studs: white walls of 1 x N bricks with a door and a
window at the front, a red roof of 45 degree slopes and a chimney.
-}
cottage : String
cottage =
    let
        wall =
            white

        roof =
            red

        sides layer =
            [ part wall ( -50, onLayer layer, 0 ) Quarter "3004.dat"
            , part wall ( 50, onLayer layer, 0 ) Quarter "3004.dat"
            , part wall ( 0, onLayer layer, 30 ) Straight "3009.dat"
            ]
    in
    model "Cottage"
        (List.concatMap sides [ 0, 1, 2 ]
            ++ [ part reddishBrown ( -40, onLayer 1, -30 ) Straight "87552.dat"
               , part wall ( 0, onLayer 0, -30 ) Straight "3004.dat"
               , part wall ( 0, onLayer 1, -30 ) Straight "3004.dat"
               , part 1 ( 40, onLayer 1, -30 ) Straight "60592.dat"
               , part wall ( 0, onLayer 2, -30 ) Straight "3009.dat"
               , part roof ( -20, onLayer 3, -10 ) Straight "3037.dat"
               , part roof ( 40, onLayer 3, -10 ) Straight "3039.dat"
               , part roof ( 20, onLayer 3, 10 ) Half "3037.dat"
               , part roof ( -40, onLayer 3, 10 ) Half "3039.dat"
               , part darkBluishGrey ( 40, onLayer 4, 20 ) Straight "3005.dat"
               , part darkBluishGrey ( 40, onLayer 5, 20 ) Straight "3005.dat"
               ]
        )


{-| A barn, 8 x 6 studs: dark red walls with a white arched door at the
front and a dark grey roof of 33 degree slopes.
-}
barn : String
barn =
    let
        wall =
            320

        roof =
            darkBluishGrey

        sides layer =
            [ part wall ( -70, onLayer layer, 0 ) Quarter "3010.dat"
            , part wall ( 70, onLayer layer, 0 ) Quarter "3010.dat"
            , part wall ( 0, onLayer layer, 50 ) Straight "3008.dat"
            ]
    in
    model "Barn"
        (List.concatMap sides [ 0, 1, 2 ]
            ++ List.concatMap
                (\layer ->
                    [ part wall ( -60, onLayer layer, -50 ) Straight "3004.dat"
                    , part wall ( 60, onLayer layer, -50 ) Straight "3004.dat"
                    ]
                )
                [ 0, 1 ]
            ++ [ part white ( 0, onLayer 1, -50 ) Straight "6182.dat"
               , part wall ( 0, onLayer 2, -50 ) Straight "3008.dat"
               ]
            ++ List.concatMap
                (\x ->
                    [ part roof ( x, onLayer 3, -10 ) Straight "3298.dat"
                    , part roof ( x, onLayer 3, 10 ) Half "3298.dat"
                    ]
                )
                [ -60, -20, 20, 60 ]
        )


{-| A tire barrier for trackside runoffs: two racing tires on rims stacked
together.
-}
tireBarrier : String
tireBarrier =
    model "Tire barrier"
        [ part 0 ( 0, onLayer 0, 0 ) Straight "6015.dat"
        , part white ( 0, onLayer 0, 0 ) Straight "6014b.dat"
        , part 0 ( 0, onLayer 1, 0 ) Straight "6015.dat"
        , part white ( 0, onLayer 1, 0 ) Straight "6014b.dat"
        ]



-- SEA


{-| A buoy: an inverted 2 x 2 dish hull, a 2 x 2 round brick, a column of two
1 x 1 round bricks (the lower one white) and a cone lantern top. The coloured
parts take the buoy's colour.
-}
buoy : String
buoy =
    model "Buoy"
        [ part 71 ( 0, 0, 0 ) Straight "4740.dat"
        , part 16 ( 0, onLayer 0, 0 ) Straight "3941.dat"
        , part white ( 0, onLayer 1, 0 ) Straight "3062b.dat"
        , part 16 ( 0, onLayer 2, 0 ) Straight "3062b.dat"
        , part 14 ( 0, onLayer 3, 0 ) Straight "4589.dat"
        ]


{-| The beacon over the next buoy: a pole of two white bars with a yellow
flag at the top, flying towards +X.
-}
beacon : String
beacon =
    model "Beacon"
        [ part white ( 0, -80, 0 ) Straight "30374.dat"
        , part white ( 0, -160, 0 ) Straight "30374.dat"
        , part 14 ( 0, -160, 0 ) Quarter "2335.dat"
        ]


{-| A floating target: a raft of two reddish-brown 2 x 2 barrels, a dark tan
plank deck, an upright bullseye target and a flag on a mast.
-}
floatingTarget : String
floatingTarget =
    model "Floating target"
        [ part reddishBrown ( 0, onLayer 0, -20 ) Straight "2489.dat"
        , part reddishBrown ( 0, onLayer 0, 20 ) Straight "2489.dat"
        , part darkTan ( 0, onLayer 1, 0 ) Straight "3001.dat"
        , part red ( -10, onLayer 2, 0 ) Straight "14769.dat"
        , part white ( -10, onLayer 2 - 8, 0 ) Straight "98138.dat"
        , part white ( 20, -60, 0 ) Straight "30374.dat"
        , part 14 ( 20, -60, 0 ) Quarter "2335.dat"
        ]


{-| An island, drawn stretched to each island's radius and height: a tan
beach of two 2 x 2 round bricks, a green round brick a little narrower and a
small green hill.
-}
island : String
island =
    model "Island"
        [ part tan ( 0, onLayer 0, 0 ) Straight "3941.dat"
        , part tan ( 0, onLayer 1, 0 ) Straight "3941.dat"
        , scaledPart green ( 0, onLayer 2, 0 ) 0.8 "3941.dat"
        , scaledPart brightGreen ( 4, onLayer 3, -2 ) 0.5 "3941.dat"
        ]


{-| A lighthouse: a tower of red and white 2 x 2 bricks, a black gallery
plate, a lamp of a trans-yellow round brick and a red cone roof.
-}
lighthouse : String
lighthouse =
    model "Lighthouse"
        (List.map
            (\layer ->
                part
                    (if modBy 2 layer == 0 then
                        white

                     else
                        red
                    )
                    ( 0, onLayer layer, 0 )
                    Straight
                    "3003.dat"
            )
            [ 0, 1, 2, 3, 4 ]
            ++ [ part 0 ( 0, onLayer 4 - 8, 0 ) Straight "3022.dat"
               , part 46 ( 0, onLayer 5 - 8, 0 ) Straight "3941.dat"
               , part red ( 0, onLayer 7 - 8, 0 ) Straight "3942c.dat"
               ]
        )


{-| A cannonball: a black 1 x 1 round brick.
-}
cannonball : String
cannonball =
    model "Cannonball" [ part 0 ( 0, onLayer 0, 0 ) Straight "3062b.dat" ]


{-| A splash: three shrinking tiers of round bricks in the splash's colour.
-}
splash : String
splash =
    model "Splash"
        [ part 16 ( 0, onLayer 0, 0 ) Straight "3941.dat"
        , scaledPart 16 ( 0, onLayer 1, 0 ) 0.65 "3941.dat"
        , part 16 ( 0, onLayer 2, 0 ) Straight "3062b.dat"
        ]



-- ARCHES


{-| One brick of an arch: the part (`3003.dat` for the posts, `3001.dat` and
`3003.dat` for the beam), its colour and where it goes. `transform` places
the part's `singlePart` model in arch space, in metres: X across the arch,
Y up from the ground, Z through it.
-}
type alias ArchBrick =
    { part : String
    , color : Int
    , transform : Mat4
    }


{-| The bricks of a start, checkpoint or finish arch: two posts of 2 x 2
bricks at x = ±`halfWidth`, `postHeight` metres tall, and a two-layer beam of
2 x 4 bricks across their tops, laid in running bond and coloured by cycling
through `bannerColors`. `unit` is the size of one LDU in metres.
-}
archBricks : { halfWidth : Float, postHeight : Float, unit : Float, postColor : Int, bannerColors : List Int } -> List ArchBrick
archBricks spec =
    let
        u =
            spec.unit

        brickH =
            24 * u

        postCount =
            max 1 (round (spec.postHeight / brickH))

        -- A part's origin is at the top of its body.
        place file color x bottomY =
            { part = file
            , color = color
            , transform = Mat4.mul (Mat4.makeTranslate (vec3 x (bottomY + brickH) 0)) (Mat4.makeScale3 u u u)
            }

        posts =
            List.concatMap
                (\i ->
                    [ place "3003.dat" spec.postColor -spec.halfWidth (toFloat i * brickH)
                    , place "3003.dat" spec.postColor spec.halfWidth (toFloat i * brickH)
                    ]
                )
                (List.range 0 (postCount - 1))

        beamY =
            toFloat postCount * brickH

        span =
            2 * spec.halfWidth + 40 * u

        count =
            max 1 (ceiling (span / (80 * u)))

        bannerColor i =
            case List.drop (modBy (max 1 (List.length spec.bannerColors)) i) spec.bannerColors of
                c :: _ ->
                    c

                [] ->
                    spec.postColor

        lowerStart =
            -(toFloat count * 80 * u) / 2 + 40 * u

        lower =
            List.map (\i -> place "3001.dat" (bannerColor i) (lowerStart + toFloat i * 80 * u) beamY)
                (List.range 0 (count - 1))

        upperEnds =
            [ place "3003.dat" (bannerColor 1) (lowerStart - 20 * u) (beamY + brickH)
            , place "3003.dat" (bannerColor count) (lowerStart + toFloat (count - 1) * 80 * u + 20 * u) (beamY + brickH)
            ]

        upper =
            List.map (\i -> place "3001.dat" (bannerColor (i + 2)) (lowerStart + 40 * u + toFloat i * 80 * u) (beamY + brickH))
                (List.range 0 (count - 2))
    in
    posts ++ lower ++ upper ++ upperEnds
