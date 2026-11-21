module Gear.DetectTest exposing (suite)

{-| Unit tests for gear detection and graph construction logic.
-}

import Array
import Dict
import Expect
import Gear.Detect exposing (MeshMode(..), buildGearGraph, buildGearGraphReference, crossedLayouts, extractGears, isWorm, meshFacts, meshMode, meshing)
import Gear.Physics as Physics
import Gear.Types exposing (GearAxis(..), GearInstance, GearKind(..), GearSpec)
import LDraw.Resolve exposing (PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (Vec3, vec3)
import Test exposing (Test, describe, test)



-- ── Helpers ───────────────────────────────────────────────────────────────────


{-| Build a SubFileRef line at the given world translation.
-}
gearRef : String -> Float -> Float -> Float -> LDrawLine
gearRef file x y z =
    SubFileRef
        { color = 16
        , transform = Mat4.makeTranslate3 x y z
        , file = file
        }


emptyCache : LDraw.Resolve.PartCache
emptyCache =
    Dict.empty


spec8T : GearSpec
spec8T =
    { partFile = "3647.dat", teeth = 8, pitchRadius = 10.0, kind = SpurGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


spec16T : GearSpec
spec16T =
    { partFile = "4019.dat", teeth = 16, pitchRadius = 20.0, kind = SpurGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


specBevel20T : GearSpec
specBevel20T =
    { partFile = "32198.dat", teeth = 20, pitchRadius = 25.0, kind = BevelGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


specCrown24T : GearSpec
specCrown24T =
    { partFile = "3650b.dat", teeth = 24, pitchRadius = 30.0, kind = CrownGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


specWorm : GearSpec
specWorm =
    { partFile = "4716.dat", teeth = 1, pitchRadius = 10.0, kind = WormGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


spec12T : GearSpec
spec12T =
    { partFile = "69778.dat", teeth = 12, pitchRadius = 15.0, kind = SpurGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


spec24T : GearSpec
spec24T =
    { partFile = "3648.dat", teeth = 24, pitchRadius = 30.0, kind = SpurGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


spec40T : GearSpec
spec40T =
    { partFile = "3649.dat", teeth = 40, pitchRadius = 50.0, kind = SpurGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


specDb12 : GearSpec
specDb12 =
    { partFile = "32270.dat", teeth = 12, pitchRadius = 15.0, kind = DoubleBevelGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


specDb20 : GearSpec
specDb20 =
    { partFile = "32269.dat", teeth = 20, pitchRadius = 25.0, kind = DoubleBevelGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


specBevel12 : GearSpec
specBevel12 =
    { partFile = "6589.dat", teeth = 12, pitchRadius = 15.0, kind = BevelGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


specBevel14 : GearSpec
specBevel14 =
    { partFile = "4143.dat", teeth = 14, pitchRadius = 17.5, kind = BevelGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


specBevel20a : GearSpec
specBevel20a =
    { partFile = "32198a.dat", teeth = 20, pitchRadius = 25.0, kind = BevelGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }


{-| Local +Z onto world +X.
-}
axisX : Mat4.Mat4
axisX =
    Mat4.makeRotate (pi / 2) (vec3 0 1 0)


{-| Local +Z onto world +Y.
-}
axisY : Mat4.Mat4
axisY =
    Mat4.makeRotate (-pi / 2) (vec3 1 0 0)


{-| A gear with rotation `rot` and its centre at `pos`.
-}
placed : Int -> GearSpec -> Mat4.Mat4 -> Vec3 -> GearInstance
placed id spec rot pos =
    gearInst id spec (Mat4.mul (Mat4.makeTranslate pos) rot) pos


{-| Move a whole scene by the rigid (or mirroring) transform `m`.
-}
moveScene : Mat4.Mat4 -> GearInstance -> GearInstance
moveScene m g =
    { g | worldMatrix = Mat4.mul m g.worldMatrix, worldPosition = Mat4.transform m g.worldPosition }


{-| Turn a gear about its own axle.
-}
roll : Float -> GearInstance -> GearInstance
roll angle g =
    { g | worldMatrix = Mat4.mul g.worldMatrix (Mat4.makeRotate angle (vec3 0 0 1)) }


{-| An arbitrary rigid transform: a rotation about a skew axis plus a shift.
-}
someRigid : Mat4.Mat4
someRigid =
    Mat4.mul (Mat4.makeTranslate3 123.4 -56.7 8.9)
        (Mat4.mul (Mat4.makeRotate 0.7 (vec3 1 2 3)) (Mat4.makeRotate 1.3 (vec3 0 1 1)))


{-| Whether the pair meshes, in both argument orders and in the graph (where
the first gear is index 0 and the second index 1).
-}
expectMesh : Bool -> GearInstance -> GearInstance -> Expect.Expectation
expectMesh expected g1 g2 =
    Expect.equal ( expected, expected, expected )
        ( meshing g1 g2
        , meshing g2 g1
        , Dict.get 0 (buildGearGraph [ { g1 | id = 0 }, { g2 | id = 1 } ]).connections
            |> Maybe.withDefault []
            |> List.member 1
        )


testGearSpecs : List GearSpec
testGearSpecs =
    [ spec8T
    , spec16T
    , specBevel20T
    , specCrown24T
    ]


{-| Build a gear instance with the given id, spec, world matrix and position.
-}
gearInst : Int -> GearSpec -> Mat4.Mat4 -> Math.Vector3.Vec3 -> GearInstance
gearInst id spec mat pos =
    { id = id, spec = spec, color = 16, worldPosition = pos, worldMatrix = mat }


{-| Assert the spatial-hash `buildGearGraph` produces byte-identical output to
the O(n²) `buildGearGraphReference` — same connections and rigid axles, in the
same adjacency-list order.
-}
expectSameGraph : List GearInstance -> Expect.Expectation
expectSameGraph insts =
    let
        fast =
            buildGearGraph insts

        ref =
            buildGearGraphReference insts
    in
    Expect.all
        [ \_ -> Expect.equal ref.connections fast.connections
        , \_ -> Expect.equal ref.rigidAxles fast.rigidAxles
        ]
        ()



-- ── Suite ─────────────────────────────────────────────────────────────────────


suite : Test
suite =
    describe "Gear.Detect"
        [ describe "extractGears"
            [ test "no SubFileRefs → empty list" <|
                \_ ->
                    extractGears testGearSpecs [] emptyCache
                        |> List.length
                        |> Expect.equal 0
            , test "non-gear SubFileRef is ignored" <|
                \_ ->
                    let
                        lines =
                            [ SubFileRef { color = 16, transform = Mat4.identity, file = "3001.dat" } ]
                    in
                    extractGears testGearSpecs lines emptyCache
                        |> List.length
                        |> Expect.equal 0
            , test "single 8T gear reference is detected" <|
                \_ ->
                    extractGears testGearSpecs [ gearRef "3647.dat" 0 0 0 ] emptyCache
                        |> List.length
                        |> Expect.equal 1
            , test "detected gear has correct spec" <|
                \_ ->
                    case extractGears testGearSpecs [ gearRef "3647.dat" 0 0 0 ] emptyCache of
                        [ inst ] ->
                            Expect.equal "3647.dat" inst.spec.partFile

                        _ ->
                            Expect.fail "Expected exactly one gear"
            , test "world position reflects translation" <|
                \_ ->
                    case extractGears testGearSpecs [ gearRef "3647.dat" 10 0 0 ] emptyCache of
                        [ inst ] ->
                            Math.Vector3.getX inst.worldPosition
                                |> Expect.within (Expect.Absolute 1.0e-4) 10.0

                        _ ->
                            Expect.fail "Expected exactly one gear"
            , test "two gear refs produce two instances" <|
                \_ ->
                    let
                        lines =
                            [ gearRef "3647.dat" 0 0 0
                            , gearRef "4019.dat" 40 0 0
                            ]
                    in
                    extractGears testGearSpecs lines emptyCache
                        |> List.length
                        |> Expect.equal 2
            , test "IDs are assigned 0, 1, 2, ..." <|
                \_ ->
                    let
                        lines =
                            [ gearRef "3647.dat" 0 0 0
                            , gearRef "4019.dat" 40 0 0
                            ]

                        ids =
                            extractGears testGearSpecs lines emptyCache |> List.map .id
                    in
                    Expect.equal [ 0, 1 ] ids
            ]
        , describe "explicit gear kinds"
            [ test "worm identity is independent of tooth count" <|
                \_ ->
                    Expect.equal
                        [ True, False ]
                        [ isWorm specWorm, isWorm { spec8T | teeth = 1 } ]
            , test "a fixed turntable base is not a rotating mesh node" <|
                \_ ->
                    let
                        fixedBase =
                            { partFile = "99009.dat"
                            , teeth = 28
                            , pitchRadius = 35
                            , kind = TurntableGear
                            , axis = AxisY
                            , motorCapable = False
                            , rotates = False
                            , holes = []
                            }

                        pinion =
                            { spec8T | axis = AxisY }
                    in
                    meshing
                        (gearInst 0 fixedBase Mat4.identity (vec3 0 0 0))
                        (gearInst 1 pinion Mat4.identity (vec3 45 0 0))
                        |> Expect.equal False
            , test "fixed 99009 cannot join rotatable 99010 through a rigid axle or mesh" <|
                \_ ->
                    let
                        fixedBase =
                            { partFile = "99009.dat"
                            , teeth = 28
                            , pitchRadius = 35
                            , kind = TurntableGear
                            , axis = AxisY
                            , motorCapable = False
                            , rotates = False
                            , holes = []
                            }

                        top =
                            { partFile = "99010.dat"
                            , teeth = 28
                            , pitchRadius = 35
                            , kind = TurntableGear
                            , axis = AxisY
                            , motorCapable = True
                            , rotates = True
                            , holes = []
                            }

                        graph =
                            buildGearGraph
                                [ gearInst 0 fixedBase Mat4.identity (vec3 0 0 0)
                                , gearInst 1 top Mat4.identity (vec3 0 0 0)
                                ]
                    in
                    Expect.all
                        [ \g -> Expect.equal Dict.empty g.connections
                        , \g -> Expect.equal Dict.empty g.rigidAxles
                        ]
                        graph
            ]
        , describe "buildGearGraph — adjacency"
            [ test "no instances → empty connections" <|
                \_ ->
                    buildGearGraph []
                        |> .connections
                        |> Dict.isEmpty
                        |> Expect.equal True
            , test "single gear → no connections" <|
                \_ ->
                    let
                        gear =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }
                    in
                    buildGearGraph [ gear ]
                        |> .connections
                        |> Dict.isEmpty
                        |> Expect.equal True
            , test "8T + 16T at exact mesh distance → connected" <|
                \_ ->
                    -- pitch radii: 10 + 20 = 30 LDU apart on X axis
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 30 0 0, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> List.member 1
                        |> Expect.equal True
            , test "connection is symmetric (both directions)" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 30 0 0, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Expect.all
                        [ \g -> Dict.get 0 g.connections |> Maybe.withDefault [] |> List.member 1 |> Expect.equal True
                        , \g -> Dict.get 1 g.connections |> Maybe.withDefault [] |> List.member 0 |> Expect.equal True
                        ]
                        graph
            , test "two gears too far apart → not connected" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 200 0 0, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> Expect.equal []
            , test "parallel gears with axial offset do not mesh even at same distance" <|
                \_ ->
                    let
                        -- Local Z is gear axis for identity transform.
                        -- g2 is offset in both X and Z, but keeps centre distance ~30.
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1
                            , spec = spec16T
                            , color = 16
                            , worldPosition = vec3 29.5804 0 5
                            , worldMatrix = Mat4.identity
                            }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> Expect.equal []
            , test "instances array length matches input" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 40 0 0, worldMatrix = Mat4.identity }
                    in
                    buildGearGraph [ g1, g2 ]
                        |> .instances
                        |> Array.length
                        |> Expect.equal 2
            , test "double bevels 12T + 20T mesh at right angles, 1.5 / 1 stud from the other axle" <|
                \_ ->
                    -- The 12T sits 30 LDU from the 20T's axle, the 20T 20 LDU
                    -- from the 12T's axle; the axles cross at the origin.
                    expectMesh True
                        (placed 0 specDb12 Mat4.identity (vec3 0 0 30))
                        (placed 1 specDb20 axisX (vec3 20 0 0))
            , test "double bevels 12T + 20T swapped (12T at 20, 20T at 30) do not mesh" <|
                \_ ->
                    expectMesh False
                        (placed 0 specDb12 Mat4.identity (vec3 0 0 20))
                        (placed 1 specDb20 axisX (vec3 30 0 0))
            , test "double bevels 12T + 12T mesh at right angles, each 1 stud from the other axle" <|
                \_ ->
                    expectMesh True
                        (placed 0 specDb12 Mat4.identity (vec3 0 0 20))
                        (placed 1 specDb12 axisX (vec3 20 0 0))
            , test "double bevels 12T + 12T at 2 studs from the other axle do not mesh" <|
                \_ ->
                    expectMesh False
                        (placed 0 specDb12 Mat4.identity (vec3 0 0 40))
                        (placed 1 specDb12 axisX (vec3 20 0 0))
            , test "double bevels 20T + 20T mesh at right angles, each 1.5 studs from the other axle" <|
                \_ ->
                    expectMesh True
                        (placed 0 specDb20 Mat4.identity (vec3 0 0 -30))
                        (placed 1 specDb20 axisX (vec3 -30 0 0))
            , test "double bevels on skew axles do not mesh" <|
                \_ ->
                    -- Right angles and the right offsets, but the axles miss
                    -- each other by 5 LDU.
                    expectMesh False
                        (placed 0 specDb12 Mat4.identity (vec3 0 0 20))
                        (placed 1 specDb12 axisX (vec3 20 5 0))
            , test "the old skew bevel layout (20T at (50,0,0), axis along Y) does not mesh" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            expectMesh False
                                (placed 0 specBevel20T Mat4.identity (vec3 0 0 0))
                                (placed 1 specBevel20T (Mat4.makeRotate (pi / 2) (vec3 1 0 0)) (vec3 50 0 0))
                        , \_ ->
                            expectMesh False
                                (placed 0 specDb20 Mat4.identity (vec3 0 0 0))
                                (placed 1 specDb20 (Mat4.makeRotate (pi / 2) (vec3 1 0 0)) (vec3 50 0 0))
                        ]
                        ()
            , test "bevel gears do not connect when axes are parallel" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = specBevel20T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1
                            , spec = specBevel20T
                            , color = 16
                            , worldPosition = vec3 50 0 0
                            , worldMatrix = Mat4.identity
                            }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> Expect.equal []
            , test "double bevels 12T + 20T mesh as spur gears on parallel axles 40 apart" <|
                \_ ->
                    expectMesh True
                        (placed 0 specDb12 Mat4.identity (vec3 0 0 0))
                        (placed 1 specDb20 Mat4.identity (vec3 40 0 0))
            , test "a double bevel 12T meshes with a 24T spur on parallel axles" <|
                \_ ->
                    expectMesh True
                        (placed 0 specDb12 Mat4.identity (vec3 0 0 0))
                        (placed 1 spec24T Mat4.identity (vec3 0 45 0))
            , test "single bevels 6589 mesh at right angles with the teeth toward the crossing" <|
                \_ ->
                    -- 6589 teeth face its local +Z; each centre 17 LDU from the
                    -- other axle.
                    expectMesh True
                        (placed 0 specBevel12 Mat4.identity (vec3 0 0 -17))
                        (placed 1 specBevel12 axisX (vec3 -17 0 0))
            , test "single bevels 6589 with the teeth facing away do not mesh" <|
                \_ ->
                    expectMesh False
                        (placed 0 specBevel12 Mat4.identity (vec3 0 0 17))
                        (placed 1 specBevel12 axisX (vec3 -17 0 0))
            , test "a flipped 6589 (turned over about its X axis) faces away" <|
                \_ ->
                    expectMesh False
                        (placed 0 specBevel12 (Mat4.makeRotate pi (vec3 1 0 0)) (vec3 0 0 -17))
                        (placed 1 specBevel12 axisX (vec3 -17 0 0))
            , test "single bevels 32198a mesh at right angles, each 27 LDU from the other axle" <|
                \_ ->
                    expectMesh True
                        (placed 0 specBevel20a Mat4.identity (vec3 0 0 27))
                        (placed 1 specBevel20a axisX (vec3 27 0 0))
            , test "single bevels 4143 mesh at right angles, each 17 LDU from the other axle" <|
                \_ ->
                    expectMesh True
                        (placed 0 specBevel14 Mat4.identity (vec3 0 0 17))
                        (placed 1 specBevel14 axisX (vec3 17 0 0))
            , test "a 6589 and a 32198a (different tooth counts) do not mesh" <|
                \_ ->
                    Expect.all
                        [ \_ -> crossedLayouts specBevel12 specBevel20a |> Expect.equal []
                        , \_ ->
                            expectMesh False
                                (placed 0 specBevel12 Mat4.identity (vec3 0 0 -27))
                                (placed 1 specBevel20a axisX (vec3 17 0 0))
                        ]
                        ()
            , test "crossed rules survive a rigid move of the scene and rolling each gear" <|
                \_ ->
                    let
                        cases =
                            [ ( True, placed 0 specDb12 Mat4.identity (vec3 0 0 30), placed 1 specDb20 axisX (vec3 20 0 0) )
                            , ( False, placed 0 specDb12 Mat4.identity (vec3 0 0 20), placed 1 specDb20 axisX (vec3 30 0 0) )
                            , ( True, placed 0 specBevel12 Mat4.identity (vec3 0 0 -17), placed 1 specBevel12 axisX (vec3 -17 0 0) )
                            , ( False, placed 0 specBevel12 Mat4.identity (vec3 0 0 17), placed 1 specBevel12 axisX (vec3 -17 0 0) )
                            , ( True, placed 0 specCrown24T Mat4.identity (vec3 0 0 0), placed 1 spec8T axisX (vec3 30 0 12.5) )
                            , ( False, placed 0 specCrown24T Mat4.identity (vec3 0 0 0), placed 1 spec8T axisX (vec3 30 0 -12.5) )
                            ]

                        moved ( expected, a, b ) =
                            ( expected
                            , moveScene someRigid (roll 0.4 a)
                            , moveScene someRigid (roll 2.1 b)
                            )
                    in
                    Expect.all
                        (List.map (\( expected, a, b ) -> \_ -> expectMesh expected a b) (List.map moved cases))
                        ()
            , test "crossed rules hold in a mirrored scene" <|
                \_ ->
                    let
                        mirror =
                            Mat4.makeScale3 -1 1 1
                    in
                    Expect.all
                        [ \_ ->
                            expectMesh True
                                (moveScene mirror (placed 0 specBevel12 Mat4.identity (vec3 0 0 -17)))
                                (moveScene mirror (placed 1 specBevel12 axisX (vec3 -17 0 0)))
                        , \_ ->
                            expectMesh False
                                (moveScene mirror (placed 0 specBevel12 Mat4.identity (vec3 0 0 17)))
                                (moveScene mirror (placed 1 specBevel12 axisX (vec3 -17 0 0)))
                        ]
                        ()
            , test "crown meshes with an 8T at right angles on a crossing axle" <|
                \_ ->
                    -- The 8T axle points at the crown axle; the 8T sits on the
                    -- crown's pitch circle (30) and its pitch circle touches the
                    -- crown's pitch plane (z = 2.5).
                    expectMesh True
                        (placed 0 specCrown24T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec8T axisX (vec3 30 0 12.5))
            , test "crown meshes with an 8T on a vertical axle 1.5 studs out (s = 12)" <|
                \_ ->
                    expectMesh True
                        (placed 0 specCrown24T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec8T axisY (vec3 0 30 12))
            , test "crown meshes with a 24T at right angles" <|
                \_ ->
                    expectMesh True
                        (placed 0 specCrown24T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec24T axisX (vec3 30 0 32.5))
            , test "crown meshes with a 12T (69778) at right angles" <|
                \_ ->
                    expectMesh True
                        (placed 0 specCrown24T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec12T axisX (vec3 30 0 17.5))
            , test "an 8T behind the crown (wrong side) does not mesh" <|
                \_ ->
                    expectMesh False
                        (placed 0 specCrown24T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec8T axisX (vec3 30 0 -12.5))
            , test "the old crown layout (8T at (40,0,0), tangential axle) does not mesh" <|
                \_ ->
                    expectMesh False
                        (placed 0 specCrown24T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec8T (Mat4.makeRotate (pi / 2) (vec3 1 0 0)) (vec3 40 0 0))
            , test "a crown and a double bevel do not mesh at right angles" <|
                \_ ->
                    Expect.all
                        [ \_ -> crossedLayouts specCrown24T specDb12 |> Expect.equal []
                        , \_ ->
                            expectMesh False
                                (placed 0 specCrown24T Mat4.identity (vec3 0 0 0))
                                (placed 1 specDb12 axisX (vec3 30 0 17.5))
                        ]
                        ()
            , test "a single bevel and a spur can never mesh" <|
                \_ ->
                    Expect.all
                        [ \_ -> meshMode (placed 0 specBevel12 Mat4.identity (vec3 0 0 0)) (placed 1 spec24T Mat4.identity (vec3 45 0 0)) |> Expect.equal Nothing
                        , \_ -> meshMode (placed 0 specBevel12 Mat4.identity (vec3 0 0 0)) (placed 1 spec24T axisX (vec3 30 0 17)) |> Expect.equal Nothing
                        ]
                        ()
            , test "mesh mode follows the axle angle for kinds that allow both" <|
                \_ ->
                    Expect.equal
                        [ Just ParallelMesh, Just CrossedMesh, Just ParallelMesh, Just WormMesh ]
                        [ meshMode (placed 0 specDb12 Mat4.identity (vec3 0 0 0)) (placed 1 specDb12 Mat4.identity (vec3 30 0 0))
                        , meshMode (placed 0 specDb12 Mat4.identity (vec3 0 0 20)) (placed 1 specDb12 axisX (vec3 20 0 0))
                        , meshMode (placed 0 spec8T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T axisX (vec3 20 0 0))
                        , meshMode (placed 0 specWorm axisX (vec3 0 0 0)) (placed 1 spec24T Mat4.identity (vec3 0 40 0))
                        ]
            , test "crossed facts report each gear's offset along its axle" <|
                \_ ->
                    meshFacts
                        (placed 0 specDb12 Mat4.identity (vec3 0 0 20))
                        (placed 1 specDb20 axisX (vec3 30 0 0))
                        |> .crossOffsets
                        |> Maybe.map (\c -> List.map (\x -> round (x * 10)) [ c.first, c.firstRequired, c.firstExcess, c.second, c.secondRequired, c.secondExcess ])
                        |> Expect.equal (Just [ -200, 300, 85, -300, 200, 75 ])
            , test "crown meshes with an 8T on parallel axles 40 apart (rim teeth)" <|
                \_ ->
                    expectMesh True
                        (placed 0 specCrown24T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec8T Mat4.identity (vec3 40 0 0))
            , test "crown + parallel 8T flush on the same beam face (offset +2) meshes" <|
                \_ ->
                    expectMesh True
                        (placed 0 specCrown24T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec8T Mat4.identity (vec3 40 0 2))
            , test "crown + parallel 8T 5 LDU along the axle does not mesh" <|
                \_ ->
                    expectMesh False
                        (placed 0 specCrown24T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec8T Mat4.identity (vec3 40 0 5))
            ]
        , describe "crossed and parallel window edges"
            [ test "double bevels 12T + 12T: |s| 22.4 (+2.4) meshes, 22.6 (+2.6) does not" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh True (placed 0 specDb12 Mat4.identity (vec3 0 0 22.4)) (placed 1 specDb12 axisX (vec3 20 0 0))
                        , \_ -> expectMesh False (placed 0 specDb12 Mat4.identity (vec3 0 0 22.6)) (placed 1 specDb12 axisX (vec3 20 0 0))
                        ]
                        ()
            , test "double bevels 12T + 12T: |s| 18.6 (-1.4) meshes, 18.4 (-1.6) does not" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh True (placed 0 specDb12 Mat4.identity (vec3 0 0 18.6)) (placed 1 specDb12 axisX (vec3 20 0 0))
                        , \_ -> expectMesh False (placed 0 specDb12 Mat4.identity (vec3 0 0 18.4)) (placed 1 specDb12 axisX (vec3 20 0 0))
                        ]
                        ()
            , test "crown + 8T: crown offset 11.6 (-0.9) meshes, 11.4 (-1.1) does not" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh True (placed 0 specCrown24T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T axisX (vec3 30 0 11.6))
                        , \_ -> expectMesh False (placed 0 specCrown24T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T axisX (vec3 30 0 11.4))
                        ]
                        ()
            , test "crown + 8T: crown offset 14.9 (+2.4) meshes, 15.1 (+2.6) does not" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh True (placed 0 specCrown24T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T axisX (vec3 30 0 14.9))
                        , \_ -> expectMesh False (placed 0 specCrown24T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T axisX (vec3 30 0 15.1))
                        ]
                        ()
            , test "spur on a crown: 32.9 from the crown axle meshes, 33.1 does not (window ±3)" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh True (placed 0 specCrown24T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T axisX (vec3 32.9 0 12.5))
                        , \_ -> expectMesh False (placed 0 specCrown24T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T axisX (vec3 33.1 0 12.5))
                        , \_ -> expectMesh True (placed 0 specCrown24T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T axisX (vec3 27.1 0 12.5))
                        , \_ -> expectMesh False (placed 0 specCrown24T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T axisX (vec3 26.9 0 12.5))
                        ]
                        ()
            , test "crossed axles 1.9 apart still mesh, 2.1 apart do not" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh True (placed 0 specDb12 Mat4.identity (vec3 0 0 20)) (placed 1 specDb12 axisX (vec3 20 1.9 0))
                        , \_ -> expectMesh False (placed 0 specDb12 Mat4.identity (vec3 0 0 20)) (placed 1 specDb12 axisX (vec3 20 2.1 0))
                        ]
                        ()
            , test "crown + parallel 8T: axial offset 2.9 meshes, 3.1 does not" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh True (placed 0 specCrown24T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T Mat4.identity (vec3 40 0 2.9))
                        , \_ -> expectMesh False (placed 0 specCrown24T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T Mat4.identity (vec3 40 0 3.1))
                        ]
                        ()
            , test "spur pair axial offset 1.9 meshes, 2.1 does not (no crown)" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh True (placed 0 spec8T Mat4.identity (vec3 0 0 0)) (placed 1 spec24T Mat4.identity (vec3 40 0 1.9))
                        , \_ -> expectMesh False (placed 0 spec8T Mat4.identity (vec3 0 0 0)) (placed 1 spec24T Mat4.identity (vec3 40 0 2.1))
                        ]
                        ()
            , test "parallel play: -1.2 meshes, -1.3 and 12T + 40T at (30, 56) (-1.47) do not" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh True (placed 0 spec8T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T Mat4.identity (vec3 18.8 0 0))
                        , \_ -> expectMesh False (placed 0 spec8T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T Mat4.identity (vec3 18.7 0 0))
                        , \_ -> expectMesh False (placed 0 spec12T Mat4.identity (vec3 0 0 0)) (placed 1 spec40T Mat4.identity (vec3 30 56 0))
                        ]
                        ()
            , test "parallel play: +2.0 meshes, +2.1 does not" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh True (placed 0 spec8T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T Mat4.identity (vec3 22 0 0))
                        , \_ -> expectMesh False (placed 0 spec8T Mat4.identity (vec3 0 0 0)) (placed 1 spec8T Mat4.identity (vec3 22.1 0 0))
                        ]
                        ()
            , test "single bevels 32198a and 4143 with the teeth facing away do not mesh" <|
                \_ ->
                    Expect.all
                        [ \_ -> expectMesh False (placed 0 specBevel20a Mat4.identity (vec3 0 0 -27)) (placed 1 specBevel20a axisX (vec3 -27 0 0))
                        , \_ -> expectMesh False (placed 0 specBevel14 Mat4.identity (vec3 0 0 -17)) (placed 1 specBevel14 axisX (vec3 -17 0 0))
                        , \_ -> expectMesh False (placed 0 specBevel20a Mat4.identity (vec3 0 0 27)) (placed 1 specBevel20a axisX (vec3 -27 0 0))
                        ]
                        ()
            ]
        , describe "parallel mesh window (absolute, -1.25 .. +2.02 LDU)"
            [ test "the reported scene: 12T (69778) and 24T 2 studs across, 3 plates up" <|
                \_ ->
                    expectMesh True
                        (placed 0 spec12T Mat4.identity (vec3 -20 -46 -10))
                        (placed 1 spec24T Mat4.identity (vec3 20 -70 -10))
            , test "8T + 8T 1 stud across, 1 plate up (+1.54) meshes" <|
                \_ ->
                    expectMesh True
                        (placed 0 spec8T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec8T Mat4.identity (vec3 20 8 0))
            , test "16T + 40T 2 studs across, 7 plates up (-1.18) meshes" <|
                \_ ->
                    expectMesh True
                        (placed 0 spec16T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec40T Mat4.identity (vec3 40 56 0))
            , test "16T + 40T 3 studs across, 4 plates up (-2.00, binds) does not mesh" <|
                \_ ->
                    expectMesh False
                        (placed 0 spec16T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec40T Mat4.identity (vec3 60 32 0))
            , test "12T + 24T 2 studs across, 2 plates up (-1.92) does not mesh" <|
                \_ ->
                    expectMesh False
                        (placed 0 spec12T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec24T Mat4.identity (vec3 40 16 0))
            , test "12T + 40T 1 stud across, 8 plates up (+2.05) does not mesh" <|
                \_ ->
                    expectMesh False
                        (placed 0 spec12T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec40T Mat4.identity (vec3 20 64 0))
            , test "8T + 16T 4 plates straight up (+2.00) meshes" <|
                \_ ->
                    expectMesh True
                        (placed 0 spec8T Mat4.identity (vec3 0 0 0))
                        (placed 1 spec16T Mat4.identity (vec3 0 32 0))
            ]
        , describe "buildGearGraph — worm"
            [ test "worm drives 12T when axes are perpendicular and centres are 24 LDU apart (1 brick, as wheeler.ldr)" <|
                \_ ->
                    -- Mirrors wheeler.ldr: worm axis along X (identity Z→X via 90° Y rotation),
                    -- 12T axis along Z (identity). Centre distance = 24 LDU along Y
                    -- (pitch sum 25, play -1).
                    let
                        worm =
                            { id = 0
                            , spec = specWorm
                            , color = 16
                            , worldPosition = vec3 0 0 0
                            , worldMatrix = Mat4.makeRotate (pi / 2) (vec3 0 1 0)
                            }

                        gear12T =
                            { id = 1
                            , spec = spec12T
                            , color = 16
                            , worldPosition = vec3 0 24 0
                            , worldMatrix = Mat4.identity
                            }

                        graph =
                            buildGearGraph [ worm, gear12T ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> List.member 1
                        |> Expect.equal True
            , test "worm drives 24T at standard 2-stud (40 LDU) grid distance" <|
                \_ ->
                    let
                        worm =
                            { id = 0
                            , spec = specWorm
                            , color = 16
                            , worldPosition = vec3 0 0 0
                            , worldMatrix = Mat4.makeRotate (pi / 2) (vec3 0 1 0)
                            }

                        gear24T =
                            { id = 1
                            , spec = spec24T
                            , color = 16
                            , worldPosition = vec3 0 40 0
                            , worldMatrix = Mat4.identity
                            }

                        graph =
                            buildGearGraph [ worm, gear24T ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> List.member 1
                        |> Expect.equal True
            , test "worm→12T connection is one-way (self-locking)" <|
                \_ ->
                    let
                        worm =
                            { id = 0
                            , spec = specWorm
                            , color = 16
                            , worldPosition = vec3 0 0 0
                            , worldMatrix = Mat4.makeRotate (pi / 2) (vec3 0 1 0)
                            }

                        gear12T =
                            { id = 1
                            , spec = spec12T
                            , color = 16
                            , worldPosition = vec3 0 24 0
                            , worldMatrix = Mat4.identity
                            }

                        graph =
                            buildGearGraph [ worm, gear12T ]
                    in
                    -- worm → 12T exists, but 12T → worm must not
                    Expect.all
                        [ \g -> Dict.get 0 g.connections |> Maybe.withDefault [] |> List.member 1 |> Expect.equal True
                        , \g -> Dict.get 1 g.connections |> Maybe.withDefault [] |> Expect.equal []
                        ]
                        graph
            , test "worm pairs never form a rotation link, in either document order" <|
                \_ ->
                    let
                        worm id position transform =
                            { id = id
                            , spec = specWorm
                            , color = 16
                            , worldPosition = position
                            , worldMatrix = transform
                            }

                        wormX =
                            worm 0 (vec3 0 0 0) (Mat4.makeRotate (pi / 2) (vec3 0 1 0))

                        wormZ =
                            worm 1 (vec3 0 20 0) Mat4.identity

                        graphFor orderedWorms =
                            buildGearGraph orderedWorms

                        assertNoLink graph =
                            Expect.all
                                [ \g -> Expect.equal Dict.empty g.connections
                                , \g -> Expect.equal Dict.empty g.rigidAxles
                                , \g -> Physics.defaultMotorGearId g |> Expect.equal (Just 0)
                                , \g -> Physics.propagate g 0 1.0 |> Dict.size |> Expect.equal 1
                                ]
                                graph
                    in
                    Expect.all
                        [ \_ -> graphFor [ wormX, wormZ ] |> assertNoLink
                        , \_ ->
                            graphFor
                                [ { wormZ | id = 0 }
                                , { wormX | id = 1 }
                                ]
                                |> assertNoLink
                        ]
                        ()
            ]
        , describe "buildGearGraph — rigidAxles"
            [ test "two gears on the same Z-axis → co-axial coupling" <|
                \_ ->
                    -- Both gears use identity transform → axis is Z.
                    -- They are separated along Z only, so radial offset = 0.
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 0 0 40, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Expect.all
                        [ \g -> Dict.get 0 g.rigidAxles |> Maybe.withDefault [] |> List.member 1 |> Expect.equal True
                        , \g -> Dict.get 1 g.rigidAxles |> Maybe.withDefault [] |> List.member 0 |> Expect.equal True
                        ]
                        graph
            , test "co-axial gears do NOT appear in meshing connections" <|
                \_ ->
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 0 0 40, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.connections
                        |> Maybe.withDefault []
                        |> Expect.equal []
            , test "two gears with parallel but offset axes → not co-axial" <|
                \_ ->
                    -- Shifted 5 LDU radially — axes are parallel but not the same shaft.
                    let
                        g1 =
                            { id = 0, spec = spec8T, color = 16, worldPosition = vec3 0 0 0, worldMatrix = Mat4.identity }

                        g2 =
                            { id = 1, spec = spec16T, color = 16, worldPosition = vec3 5 0 40, worldMatrix = Mat4.identity }

                        graph =
                            buildGearGraph [ g1, g2 ]
                    in
                    Dict.get 0 graph.rigidAxles
                        |> Maybe.withDefault []
                        |> Expect.equal []
            ]
        , describe "buildGearGraph — equivalence with reference"
            [ test "empty model" <|
                \_ -> expectSameGraph []
            , test "meshing chain along X" <|
                \_ ->
                    -- 8T/16T/8T at successive pitch-sum spacings; all identity
                    -- matrices (axis Z), so parallel spur gears mesh.
                    expectSameGraph
                        [ gearInst 0 spec8T Mat4.identity (vec3 0 0 0)
                        , gearInst 1 spec16T Mat4.identity (vec3 30 0 0)
                        , gearInst 2 spec8T Mat4.identity (vec3 60 0 0)
                        ]
            , test "long co-axial stack along Z" <|
                \_ ->
                    -- Gears far apart along a shared Z axle: co-axial at any
                    -- separation. A position-only hash would drop these links.
                    expectSameGraph
                        [ gearInst 0 spec8T Mat4.identity (vec3 0 0 0)
                        , gearInst 1 spec16T Mat4.identity (vec3 0 0 200)
                        , gearInst 2 spec8T Mat4.identity (vec3 0 0 500)
                        ]
            , test "mixed meshing, co-axial and unrelated gears" <|
                \_ ->
                    expectSameGraph
                        [ gearInst 0 spec8T Mat4.identity (vec3 0 0 0)
                        , gearInst 1 spec16T Mat4.identity (vec3 30 0 0)
                        , gearInst 2 spec8T Mat4.identity (vec3 0 0 300)
                        , gearInst 3 spec16T Mat4.identity (vec3 300 0 0)
                        , gearInst 4 spec8T Mat4.identity (vec3 30 0 300)
                        ]
            , test "worm perpendicular to wheel (one-way edge)" <|
                \_ ->
                    expectSameGraph
                        [ gearInst 0 specWorm (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) (vec3 0 0 0)
                        , gearInst 1 spec12T Mat4.identity (vec3 0 24 0)
                        ]
            , test "meshing pair straddling a spatial-hash cell boundary" <|
                \_ ->
                    -- maxPitchRadius = 20 → meshCell = 50. Placing the pair at
                    -- x = 41 and x = 71 puts them in cells 0 and 1 respectively.
                    expectSameGraph
                        [ gearInst 0 spec8T Mat4.identity (vec3 41 0 0)
                        , gearInst 1 spec16T Mat4.identity (vec3 71 0 0)
                        ]
            , test "crossed double bevel pair and a parallel spur" <|
                \_ ->
                    expectSameGraph
                        [ placed 0 specDb12 Mat4.identity (vec3 0 0 30)
                        , placed 1 specDb20 axisX (vec3 20 0 0)
                        , placed 2 spec24T axisX (vec3 20 55 0)
                        , placed 3 specCrown24T Mat4.identity (vec3 200 0 0)
                        , placed 4 spec8T axisX (vec3 230 0 12.5)
                        ]
            , test "long co-axial pair is present in rigidAxles" <|
                \_ ->
                    -- The decisive regression: a naive position hash drops this.
                    let
                        graph =
                            buildGearGraph
                                [ gearInst 0 spec8T Mat4.identity (vec3 0 0 0)
                                , gearInst 1 spec16T Mat4.identity (vec3 0 0 500)
                                ]
                    in
                    Dict.get 0 graph.rigidAxles
                        |> Maybe.withDefault []
                        |> List.member 1
                        |> Expect.equal True
            ]
        ]
