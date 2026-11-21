module Studio.GearSnapTest exposing (suite)

{-| Unit tests for `Studio.GearSnap`:

  - Snapping a 16T gear to another 16T gear at 40 LDU coplanar distance.
  - Snapping an 8T gear to a 16T gear at 30 LDU coplanar distance.
  - Ensuring teeth mesh and axial offset is 0.
  - Cardinal offsets and pitch circle candidates.
  - Release hysteresis.

-}

import Data
import Expect
import Gear.Detect as Detect
import Gear.Phase as Phase
import Gear.Types exposing (GearAxis(..), GearInstance, GearKind(..))
import LDraw.Connectivity as Connectivity
    exposing
        ( ConnectionType(..)
        , SnapCandidate
        , SnapGuide(..)
        )
import LDraw.Document exposing (Placement)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Studio.GearSnap as GearSnap
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Studio.GearSnap"
        [ findGearSpecTests
        , turntableGearSnapTests
        , isGearSnapTests
        , cardinalOffsetsTests
        , snap16TTo16TTests
        , snap8TTo16TTests
        , coplanarAndMeshingTests
        , hysteresisTests
        , perpendicularGearSnapTests
        ]


makePlacement : Int -> String -> Mat4 -> Int -> Placement
makePlacement id part matrix color =
    { id = id
    , part = part
    , fileText = ""
    , color = color
    , matrix = matrix
    , extra = []
    }


findGearSpecTests : Test
findGearSpecTests =
    describe "findGearSpec"
        [ test "identifies 16T gear (4019.dat) with pitch radius 20.0" <|
            \_ ->
                case GearSnap.findGearSpec "4019.dat" of
                    Just spec ->
                        Expect.all
                            [ \_ -> Expect.equal 16 spec.teeth
                            , \_ -> Expect.within (Expect.Absolute 1.0e-4) 20.0 spec.pitchRadius
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected 4019.dat to be recognized as a gear"
        , test "identifies 8T gear (3647.dat) with pitch radius 10.0" <|
            \_ ->
                case GearSnap.findGearSpec "3647.dat" of
                    Just spec ->
                        Expect.all
                            [ \_ -> Expect.equal 8 spec.teeth
                            , \_ -> Expect.within (Expect.Absolute 1.0e-4) 10.0 spec.pitchRadius
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected 3647.dat to be recognized as a gear"
        , test "returns Nothing for standard brick (3001.dat)" <|
            \_ ->
                Expect.equal Nothing (GearSnap.findGearSpec "3001.dat")
        , test "fixed turntable base is not a snap gear" <|
            \_ ->
                Expect.equal Nothing (GearSnap.findGearSpec "99009.dat")
        , test "turntable tops remain rotating snap gears on their local Y axis" <|
            \_ ->
                let
                    specFor part =
                        GearSnap.findGearSpec part
                            |> Maybe.map (\spec -> ( spec.rotates, spec.axis, spec.kind ))
                in
                Expect.equal
                    [ Just ( True, AxisY, TurntableGear ), Just ( True, AxisY, TurntableGear ) ]
                    [ specFor "99010.dat", specFor "18938.dat" ]
        ]


turntableGearSnapTests : Test
turntableGearSnapTests =
    describe "turntable gear snapping"
        [ test "a fixed 99009 is rejected as both dragged gear and partner" <|
            \_ ->
                let
                    target =
                        makePlacement 1 "3647.dat" Mat4.identity 14

                    fixedBase =
                        makePlacement 2 "99009.dat" Mat4.identity 7

                    draggedGear =
                        makePlacement 3 "3647.dat" Mat4.identity 14

                    draggedFixedResult =
                        GearSnap.solveGearSnap
                            (\_ -> True)
                            12
                            Nothing
                            fixedBase
                            (Mat4.makeTranslate3 43 0 0)
                            [ target ]

                    fixedPartnerResult =
                        GearSnap.solveGearSnap
                            (\_ -> True)
                            12
                            Nothing
                            draggedGear
                            (Mat4.makeTranslate3 43 0 0)
                            [ fixedBase ]
                in
                Expect.equal ( Nothing, Nothing ) ( draggedFixedResult, fixedPartnerResult )
        , test "rotatable 99010 and 18938 tops still snap to same-axis partners" <|
            \_ ->
                let
                    canSnap part distance =
                        let
                            target =
                                makePlacement 1 part Mat4.identity 7

                            dragged =
                                makePlacement 2 part Mat4.identity 7
                        in
                        GearSnap.solveGearSnap
                            (\_ -> True)
                            12
                            Nothing
                            dragged
                            (Mat4.makeTranslate3 (distance + 2) 0 0)
                            [ target ]
                            |> Maybe.map .targetPlacementId
                in
                Expect.equal [ Just 1, Just 1 ] [ canSnap "99010.dat" 70, canSnap "18938.dat" 150 ]
        ]


isGearSnapTests : Test
isGearSnapTests =
    describe "isGearSnap"
        [ test "recognizes a gear snap candidate with female axle on both sides" <|
            \_ ->
                let
                    snap =
                        dummyCandidate (TechnicAxleFemale { length = 10 }) (TechnicAxleFemale { length = 10 })
                in
                Expect.equal True (GearSnap.isGearSnap snap)
        , test "rejects standard pin/hole connection snap candidate" <|
            \_ ->
                let
                    snap =
                        dummyCandidate (TechnicAxleFemale { length = 10 }) (TechnicAxleMale { length = 10 })
                in
                Expect.equal False (GearSnap.isGearSnap snap)
        ]


cardinalOffsetsTests : Test
cardinalOffsetsTests =
    describe "cardinalOffsets"
        [ test "returns +X, -X, +Y, -Y at given distance" <|
            \_ ->
                let
                    offsets =
                        GearSnap.cardinalOffsets 40.0

                    expected =
                        [ vec3 40 0 0, vec3 -40 0 0, vec3 0 40 0, vec3 0 -40 0 ]
                in
                Expect.equal expected offsets
        ]


snap16TTo16TTests : Test
snap16TTo16TTests =
    describe "16T to 16T gear snap"
        [ test "snaps a 16T gear dragged near another 16T gear to 40 LDU distance" <|
            \_ ->
                let
                    targetGear =
                        makePlacement 1 "4019.dat" (Mat4.makeTranslate3 -140 -14 -150) 71

                    draggedGear =
                        makePlacement 2 "4019.dat" Mat4.identity 14

                    -- Dragged position: displaced by 11 LDU in Y and 10 LDU in Z (as in bug report)
                    draggedMatrix =
                        Mat4.makeTranslate3 -100 -25 -160

                    result =
                        GearSnap.solveGearSnap
                            (\_ -> True)
                            12.0
                            Nothing
                            draggedGear
                            draggedMatrix
                            [ targetGear ]
                in
                case result of
                    Just snap ->
                        let
                            snappedPos =
                                Mat4.transform snap.snapMatrix (vec3 0 0 0)

                            dist =
                                Vec3.distance (Mat4.transform targetGear.matrix (vec3 0 0 0)) snappedPos

                            axialOffset =
                                abs (Vec3.getZ snappedPos - -150.0)
                        in
                        Expect.all
                            [ \_ -> Expect.within (Expect.Absolute 1.0e-3) 40.0 dist
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) -100.0 (Vec3.getX snappedPos)
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) -14.0 (Vec3.getY snappedPos)
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) -150.0 (Vec3.getZ snappedPos)
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) 0.0 axialOffset
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected 16T gear to snap to partner 16T gear"
        ]


snap8TTo16TTests : Test
snap8TTo16TTests =
    describe "8T to 16T gear snap"
        [ test "snaps an 8T gear dragged near a 16T gear to 30 LDU distance" <|
            \_ ->
                let
                    target16T =
                        makePlacement 1 "4019.dat" (Mat4.makeTranslate3 0 0 0) 71

                    dragged8T =
                        makePlacement 2 "3647.dat" Mat4.identity 14

                    -- Drag near +X cardinal offset (30, 0, 0)
                    draggedMatrix =
                        Mat4.makeTranslate3 32 5 2

                    result =
                        GearSnap.solveGearSnap
                            (\_ -> True)
                            12.0
                            Nothing
                            dragged8T
                            draggedMatrix
                            [ target16T ]
                in
                case result of
                    Just snap ->
                        let
                            snappedPos =
                                Mat4.transform snap.snapMatrix (vec3 0 0 0)

                            dist =
                                Vec3.distance (Mat4.transform target16T.matrix (vec3 0 0 0)) snappedPos

                            axialOffset =
                                abs (Vec3.getZ snappedPos - 0.0)
                        in
                        Expect.all
                            [ \_ -> Expect.within (Expect.Absolute 1.0e-3) 30.0 dist
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) 30.0 (Vec3.getX snappedPos)
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) 0.0 (Vec3.getY snappedPos)
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) 0.0 (Vec3.getZ snappedPos)
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) 0.0 axialOffset
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected 8T gear to snap to 16T gear at 30 LDU"
        ]


coplanarAndMeshingTests : Test
coplanarAndMeshingTests =
    describe "Coplanar alignment and tooth meshing"
        [ test "snapped 16T gears satisfy Detect.meshing and have interlocking teeth" <|
            \_ ->
                let
                    targetGear =
                        makePlacement 1 "4019.dat" (Mat4.makeTranslate3 -140 -14 -150) 71

                    draggedGear =
                        makePlacement 2 "4019.dat" Mat4.identity 14

                    draggedMatrix =
                        Mat4.makeTranslate3 -100 -25 -160

                    maybeSnap =
                        GearSnap.solveGearSnap
                            (\_ -> True)
                            12.0
                            Nothing
                            draggedGear
                            draggedMatrix
                            [ targetGear ]
                in
                case maybeSnap of
                    Just snap ->
                        let
                            targetSpec =
                                GearSnap.findGearSpec targetGear.part |> Maybe.withDefault { partFile = "", teeth = 16, pitchRadius = 20, kind = SpurGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }

                            dragSpec =
                                GearSnap.findGearSpec draggedGear.part |> Maybe.withDefault { partFile = "", teeth = 16, pitchRadius = 20, kind = SpurGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }

                            g1 : GearInstance
                            g1 =
                                { id = targetGear.id
                                , spec = targetSpec
                                , color = targetGear.color
                                , worldPosition = Mat4.transform targetGear.matrix (vec3 0 0 0)
                                , worldMatrix = targetGear.matrix
                                }

                            g2 : GearInstance
                            g2 =
                                { id = draggedGear.id
                                , spec = dragSpec
                                , color = draggedGear.color
                                , worldPosition = Mat4.transform snap.snapMatrix (vec3 0 0 0)
                                , worldMatrix = snap.snapMatrix
                                }

                            facts =
                                Detect.meshFacts g1 g2

                            isMeshing =
                                Detect.meshing g1 g2

                            -- Check remaining tooth correction: should be minimal (< 0.01 rad) because teeth interlocked
                            correction =
                                Phase.meshCorrection { fixed = g1, moving = g2 }
                                    |> Maybe.map .radians
                                    |> Maybe.withDefault 1.0
                        in
                        Expect.all
                            [ \_ -> Expect.equal True facts.axisOk
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) 0.0 facts.axialOffset
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) 40.0 facts.distance
                            , \_ -> Expect.equal True isMeshing
                            , \_ -> Expect.within (Expect.Absolute 0.05) 0.0 (abs correction)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Snap expected"
        ]


hysteresisTests : Test
hysteresisTests =
    describe "Snap threshold and hysteresis"
        [ test "enters snap when within enterThreshold (15 LDU < 24 LDU)" <|
            \_ ->
                let
                    targetGear =
                        makePlacement 1 "4019.dat" Mat4.identity 71

                    draggedGear =
                        makePlacement 2 "4019.dat" Mat4.identity 14

                    -- 40 + 15 = 55 LDU (15 LDU from candidate at 40)
                    draggedMatrix =
                        Mat4.makeTranslate3 55 0 0

                    snap =
                        GearSnap.solveGearSnap (\_ -> True) 12.0 Nothing draggedGear draggedMatrix [ targetGear ]
                in
                case snap of
                    Just s ->
                        Expect.within (Expect.Absolute 1.0e-3) 40.0 (Vec3.getX s.targetWorldPosition)

                    Nothing ->
                        Expect.fail "Expected snap within enter threshold"
        , test "does not enter snap when beyond enterThreshold (40 + 30 = 70 LDU > 24 LDU)" <|
            \_ ->
                let
                    targetGear =
                        makePlacement 1 "4019.dat" Mat4.identity 71

                    draggedGear =
                        makePlacement 2 "4019.dat" Mat4.identity 14

                    draggedMatrix =
                        Mat4.makeTranslate3 70 0 0

                    snap =
                        GearSnap.solveGearSnap (\_ -> True) 12.0 Nothing draggedGear draggedMatrix [ targetGear ]
                in
                Expect.equal Nothing snap
        , test "maintains active snap within release threshold (40 + 28 = 68 LDU <= 36 LDU)" <|
            \_ ->
                let
                    targetGear =
                        makePlacement 1 "4019.dat" Mat4.identity 71

                    draggedGear =
                        makePlacement 2 "4019.dat" Mat4.identity 14

                    -- Active snap from prior frame
                    active =
                        dummyCandidate (TechnicAxleFemale { length = 10 }) (TechnicAxleFemale { length = 10 })

                    activeToTarget =
                        { active | targetPlacementId = targetGear.id }

                    -- 40 + 28 = 68 LDU (28 LDU away: > 24 enter threshold, but <= 36 release threshold)
                    draggedMatrix =
                        Mat4.makeTranslate3 68 0 0

                    snap =
                        GearSnap.solveGearSnap (\_ -> True) 12.0 (Just activeToTarget) draggedGear draggedMatrix [ targetGear ]
                in
                case snap of
                    Just s ->
                        Expect.within (Expect.Absolute 1.0e-3) 40.0 (Vec3.getX s.targetWorldPosition)

                    Nothing ->
                        Expect.fail "Expected hysteresis to maintain snap"
        ]


perpendicularGearSnapTests : Test
perpendicularGearSnapTests =
    describe "perpendicular gear snapping"
        [ test "snaps a tipped 12T double bevel (32270) onto a crossing axle 20 LDU from a 20T double bevel (32269)" <|
            \_ ->
                let
                    targetBevel =
                        makePlacement 1 "32269.dat" Mat4.identity 16

                    -- Dragged 12T double bevel tipped 90 degrees around X (axis along -Y)
                    draggedMatrix =
                        Mat4.mul (Mat4.makeTranslate3 30 0 18) (Mat4.makeRotate (pi / 2) (vec3 1 0 0))

                    draggedBevel =
                        makePlacement 2 "32270.dat" draggedMatrix 16
                in
                case GearSnap.solveGearSnap (\_ -> True) 12.0 Nothing draggedBevel draggedMatrix [ targetBevel ] of
                    Just s ->
                        let
                            facts =
                                Detect.meshFacts (instanceOf targetBevel) (instanceOf { draggedBevel | matrix = s.snapMatrix })
                        in
                        Expect.all
                            [ \_ -> expectNear (vec3 30 0 20) (Mat4.transform s.snapMatrix (vec3 0 0 0))
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) 0 facts.absDot
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) 0 facts.skew
                            , \_ -> Expect.equal True (snapMeshes targetBevel draggedBevel s)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected a crossed snap for 12T and 20T double bevels"
        , test "snaps a tipped 8T spur (3647) onto the 24T crown (3650b) pitch circle, 12.5 above the crown" <|
            \_ ->
                let
                    crownTarget =
                        makePlacement 1 "3650b.dat" Mat4.identity 16

                    draggedMatrix =
                        Mat4.mul (Mat4.makeTranslate3 32 0 10) (Mat4.makeRotate (pi / 2) (vec3 1 0 0))

                    draggedSpur =
                        makePlacement 2 "3647.dat" draggedMatrix 16
                in
                case GearSnap.solveGearSnap (\_ -> True) 12.0 Nothing draggedSpur draggedMatrix [ crownTarget ] of
                    Just s ->
                        Expect.all
                            [ \_ -> expectNear (vec3 30 0 12.5) (Mat4.transform s.snapMatrix (vec3 0 0 0))
                            , \_ -> Expect.equal True (snapMeshes crownTarget draggedSpur s)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected a crossed snap for 8T spur and 24T crown"
        , test "a single bevel (6589) never snaps to a 24T spur, parallel or tipped" <|
            \_ ->
                let
                    spurTarget =
                        makePlacement 1 "3648.dat" Mat4.identity 16

                    tryAt m =
                        GearSnap.solveGearSnap (\_ -> True) 12.0 Nothing (makePlacement 2 "6589.dat" m 16) m [ spurTarget ]
                in
                Expect.equal [ Nothing, Nothing ]
                    [ tryAt (Mat4.makeTranslate3 46 0 0)
                    , tryAt (Mat4.mul (Mat4.makeTranslate3 30 0 17) (Mat4.makeRotate (pi / 2) (vec3 0 1 0)))
                    ]
        , test "a crown snaps next to a 24T spur on parallel axles at 60, in the same plane" <|
            \_ ->
                let
                    spurTarget =
                        makePlacement 1 "3648.dat" Mat4.identity 16

                    crown =
                        makePlacement 2 "3650b.dat" Mat4.identity 16

                    draggedMatrix =
                        Mat4.makeTranslate3 63 0 1
                in
                case GearSnap.solveGearSnap (\_ -> True) 12.0 Nothing crown draggedMatrix [ spurTarget ] of
                    Just s ->
                        Expect.all
                            [ \_ -> expectNear (vec3 60 0 0) (Mat4.transform s.snapMatrix (vec3 0 0 0))
                            , \_ -> Expect.equal True (snapMeshes spurTarget crown s)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected a parallel snap of the crown next to the 24T"
        , test "a vertical 8T snaps to an 18938 turntable at 85 with a vertical axle" <|
            \_ ->
                let
                    turntable =
                        makePlacement 1 "18938.dat" Mat4.identity 16

                    -- 8T axle along world Y
                    draggedMatrix =
                        Mat4.mul (Mat4.makeTranslate3 88 0 2) (Mat4.makeRotate (-pi / 2) (vec3 1 0 0))

                    pinion =
                        makePlacement 2 "3647.dat" draggedMatrix 16
                in
                case GearSnap.solveGearSnap (\_ -> True) 12.0 Nothing pinion draggedMatrix [ turntable ] of
                    Just s ->
                        let
                            axle =
                                Vec3.sub (Mat4.transform s.snapMatrix (vec3 0 0 1)) (Mat4.transform s.snapMatrix (vec3 0 0 0))
                        in
                        Expect.all
                            [ \_ -> expectNear (vec3 85 0 0) (Mat4.transform s.snapMatrix (vec3 0 0 0))
                            , \_ -> Expect.within (Expect.Absolute 1.0e-3) 1 (abs (Vec3.getY axle))
                            , \_ -> Expect.equal True (snapMeshes turntable pinion s)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected the 8T to snap to the turntable"
        , test "a crossed 6589 pair snaps with real collision boxes and the hub boxes do not overlap" <|
            \_ ->
                let
                    target =
                        makePlacement 1 "6589.dat" Mat4.identity 16

                    -- Tipped so its axle runs along X, near the reference
                    -- layout (-17, 0, 17).
                    draggedMatrix =
                        Mat4.mul (Mat4.makeTranslate3 -18 0 15) (Mat4.makeRotate (pi / 2) (vec3 0 1 0))

                    dragged =
                        makePlacement 2 "6589.dat" draggedMatrix 16

                    hub part =
                        Connectivity.knownCatalogPart part
                            |> Maybe.andThen (.boundingBoxes >> List.head)
                            |> Maybe.withDefault { min = vec3 0 0 0, max = vec3 0 0 0 }

                    -- An artificial bystander sitting exactly where the
                    -- target's hub is. `isSnapColliding` never checks the
                    -- dragged gear against the snap target itself, so without
                    -- it the real-box validity would pass trivially; the
                    -- bystander makes the check see a 6589 hub box there.
                    bystander =
                        ( 3, Mat4.identity, hub "6589.dat" )

                    isValid snap =
                        not
                            (Connectivity.isSnapColliding 0.5
                                dragged.id
                                draggedMatrix
                                [ ( dragged.id, draggedMatrix, hub "6589.dat" ) ]
                                [ ( target.id, target.matrix, hub "6589.dat" ), bystander ]
                                snap
                            )

                    oldHub =
                        Connectivity.makeGear 15.0 20.0 "6589.dat"
                            |> .boundingBoxes
                            |> List.head
                            |> Maybe.withDefault { min = vec3 0 0 0, max = vec3 0 0 0 }
                in
                case GearSnap.solveGearSnap isValid 12.0 Nothing dragged draggedMatrix [ target ] of
                    Just s ->
                        Expect.all
                            [ \_ -> expectNear (vec3 -17 0 17) (Mat4.transform s.snapMatrix (vec3 0 0 0))
                            , \_ -> Expect.equal True (snapMeshes target dragged s)
                            , \_ ->
                                Connectivity.checkCollisions 0.5 [ ( 2, s.snapMatrix, oldHub ) ] [ ( 1, Mat4.identity, oldHub ) ]
                                    |> List.isEmpty
                                    |> Expect.equal False
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected the crossed 6589 snap to pass real collision validity"
        , test "every returned snap meshes by Gear.Detect" <|
            \_ ->
                let
                    cases =
                        [ ( "4019.dat", "3647.dat", Mat4.makeTranslate3 33 4 1 )
                        , ( "3648.dat", "69778.dat", Mat4.makeTranslate3 -44 3 0 )
                        , ( "32269.dat", "32269.dat", Mat4.mul (Mat4.makeTranslate3 0 31 -29) (Mat4.makeRotate (pi / 2) (vec3 1 0 0)) )
                        , ( "32270.dat", "32270.dat", Mat4.mul (Mat4.makeTranslate3 21 0 19) (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) )
                        , ( "3650b.dat", "4019.dat", Mat4.mul (Mat4.makeTranslate3 0 -31 21) (Mat4.makeRotate (pi / 2) (vec3 1 0 0)) )
                        , ( "3650b.dat", "3647.dat", Mat4.makeTranslate3 41 0 1 )
                        , ( "32198a.dat", "32198a.dat", Mat4.mul (Mat4.makeTranslate3 26 0 -26) (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) )
                        , ( "4143.dat", "4143.dat", Mat4.mul (Mat4.makeTranslate3 17 0 -17) (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) )
                        ]

                    check ( targetPart, dragPart, m ) =
                        let
                            target =
                                makePlacement 1 targetPart Mat4.identity 16

                            dragged =
                                makePlacement 2 dragPart m 16
                        in
                        case GearSnap.solveGearSnap (\_ -> True) 12.0 Nothing dragged m [ target ] of
                            Just s ->
                                ( dragPart, snapMeshes target dragged s )

                            Nothing ->
                                ( dragPart ++ " did not snap", False )
                in
                Expect.equal
                    (List.map (\( _, d, _ ) -> ( d, True )) cases)
                    (List.map check cases)
        , test "snaps 1-start worm (4716, R=10) to 24T spur (3648, R=30) at 40 LDU (2 studs) grid distance" <|
            \_ ->
                let
                    spurTarget =
                        makePlacement 1 "3648.dat" Mat4.identity 16

                    -- Worm rotated 90 degrees around Y (axis along X instead of Z)
                    draggedMatrix =
                        Mat4.mul (Mat4.makeTranslate3 0 38 0) (Mat4.makeRotate (pi / 2) (vec3 0 1 0))

                    draggedWorm =
                        makePlacement 2 "4716.dat" draggedMatrix 16

                    snap =
                        GearSnap.solveGearSnap (\_ -> True) 12.0 Nothing draggedWorm draggedMatrix [ spurTarget ]
                in
                case snap of
                    Just s ->
                        Expect.within (Expect.Absolute 1.0e-3) 40.0 (Vec3.length s.targetWorldPosition)

                    Nothing ->
                        Expect.fail "Expected perpendicular gear snap for worm and 24T spur at 40 LDU"
        , test "axle snaps into gear center axle hole" <|
            \_ ->
                case ( Connectivity.knownCatalogPart "3705.dat", Connectivity.knownCatalogPart "3647.dat" ) of
                    ( Just axle, Just gear ) ->
                        let
                            gearTargetPoints =
                                List.map (Connectivity.transformPoint Mat4.identity 1) gear.connections

                            hoverMat =
                                Mat4.makeTranslate3 0 0 15

                            snap =
                                Connectivity.solveSnap 16.0 0 hoverMat axle.connections gearTargetPoints
                        in
                        case snap of
                            Just s ->
                                Expect.equal True (List.member s.targetPoint.pointId [ 1, 2, 3 ])

                            Nothing ->
                                Expect.fail "Expected axle to snap into gear axle hole"

                    _ ->
                        Expect.fail "Missing catalog parts"
        , test "axle near 24T gear prioritizes center axle hole over round pin holes" <|
            \_ ->
                case ( Connectivity.knownCatalogPart "3705.dat", Connectivity.knownCatalogPart "3648.dat" ) of
                    ( Just axle, Just gear24T ) ->
                        let
                            gearTargetPoints =
                                List.map (Connectivity.transformPoint Mat4.identity 1) gear24T.connections

                            -- Hover near center front of 24T gear
                            hoverMat =
                                Mat4.makeTranslate3 0 0 14

                            snap =
                                Connectivity.solveSnap 16.0 0 hoverMat axle.connections gearTargetPoints
                        in
                        case snap of
                            Just s ->
                                -- Point IDs 1, 2, 3 are the center axle hole points (center, front, back)
                                Expect.equal True (List.member s.targetPoint.pointId [ 1, 2, 3 ])

                            Nothing ->
                                Expect.fail "Expected axle to snap into 24T gear axle hole"

                    _ ->
                        Expect.fail "Missing catalog parts"
        , test "axle snaps into worm gear axle hole" <|
            \_ ->
                case ( Connectivity.knownCatalogPart "3705.dat", Connectivity.knownCatalogPart "4716.dat" ) of
                    ( Just axle, Just worm ) ->
                        let
                            wormTargetPoints =
                                List.map (Connectivity.transformPoint Mat4.identity 1) worm.connections

                            -- Worm front face is at z = 20, hover at z = 25
                            hoverMat =
                                Mat4.makeTranslate3 0 0 25

                            snap =
                                Connectivity.solveSnap 16.0 0 hoverMat axle.connections wormTargetPoints
                        in
                        case snap of
                            Just s ->
                                Expect.equal True (List.member s.targetPoint.pointId [ 1, 2, 3 ])

                            Nothing ->
                                Expect.fail "Expected axle to snap into worm gear axle hole"

                    _ ->
                        Expect.fail "Missing catalog parts"
        , test "axle snaps into bushing axle hole" <|
            \_ ->
                case ( Connectivity.knownCatalogPart "3705.dat", Connectivity.knownCatalogPart "6590.dat" ) of
                    ( Just axle, Just bushing ) ->
                        let
                            bushingTargetPoints =
                                List.map (Connectivity.transformPoint Mat4.identity 1) bushing.connections

                            hoverMat =
                                Mat4.makeTranslate3 0 0 15

                            snap =
                                Connectivity.solveSnap 16.0 0 hoverMat axle.connections bushingTargetPoints
                        in
                        case snap of
                            Just s ->
                                Expect.equal True (List.member s.targetPoint.pointId [ 1, 2, 3 ])

                            Nothing ->
                                Expect.fail "Expected axle to snap into bushing axle hole"

                    _ ->
                        Expect.fail "Missing catalog parts"
        ]


{-| A placement as a gear instance (spec from `GearSnap.findGearSpec`).
-}
instanceOf : Placement -> GearInstance
instanceOf p =
    { id = p.id
    , spec =
        GearSnap.findGearSpec p.part
            |> Maybe.withDefault { partFile = "", teeth = 1, pitchRadius = 1, kind = SpurGear, axis = AxisZ, motorCapable = True, rotates = True, holes = [] }
    , color = p.color
    , worldPosition = Mat4.transform p.matrix (vec3 0 0 0)
    , worldMatrix = p.matrix
    }


{-| Whether the dragged gear, placed by the snap, meshes with the target.
-}
snapMeshes : Placement -> Placement -> SnapCandidate -> Bool
snapMeshes target dragged snap =
    Detect.meshing (instanceOf target) (instanceOf { dragged | matrix = snap.snapMatrix })


expectNear : Vec3 -> Vec3 -> Expect.Expectation
expectNear expected actual =
    Expect.within (Expect.Absolute 1.0e-3) 0 (Vec3.distance expected actual)


dummyCandidate : ConnectionType -> ConnectionType -> SnapCandidate
dummyCandidate draggedConn targetConn =
    { draggedPoint =
        { id = 1
        , connType = draggedConn
        , localPosition = vec3 0 0 0
        , normal = vec3 0 0 1
        , tangent = vec3 1 0 0
        }
    , targetPoint =
        { pointId = 1
        , placementId = 1
        , connType = targetConn
        , worldPosition = vec3 0 0 0
        , worldNormal = vec3 0 0 1
        , worldTangent = vec3 1 0 0
        }
    , targetPlacementId = 1
    , targetWorldPosition = vec3 0 0 0
    , targetWorldNormal = vec3 0 0 1
    , snapMatrix = Mat4.identity
    , distance = 0
    , guide = AxleAxisGuide (vec3 0 0 0) (vec3 0 0 1) False
    }
