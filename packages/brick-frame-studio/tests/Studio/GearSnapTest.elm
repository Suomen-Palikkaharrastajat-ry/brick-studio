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
import Gear.Types exposing (GearInstance)
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
        , isGearSnapTests
        , cardinalOffsetsTests
        , snap16TTo16TTests
        , snap8TTo16TTests
        , coplanarAndMeshingTests
        , hysteresisTests
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
                                GearSnap.findGearSpec targetGear.part |> Maybe.withDefault { partFile = "", teeth = 16, pitchRadius = 20 }

                            dragSpec =
                                GearSnap.findGearSpec draggedGear.part |> Maybe.withDefault { partFile = "", teeth = 16, pitchRadius = 20 }

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
