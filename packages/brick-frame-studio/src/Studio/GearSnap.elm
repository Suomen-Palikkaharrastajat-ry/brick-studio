module Studio.GearSnap exposing
    ( candidateMeshPositions
    , cardinalOffsets
    , findGearSpec
    , isGearSnap
    , solveGearSnap
    )

{-| Gear mesh snapping: magnetic alignment of meshing gears along their pitch circles.

When a gear is dragged near an existing partner gear in the document, this module
detects valid meshing positions at distance `D = R_drag + R_target`, perfectly
coplanar with the partner gear's axle plane, and sets the tooth rotation angle
so the gear teeth interlock.

-}

import Data
import Gear.Phase as Phase
import Gear.Types exposing (GearAxis(..), GearInstance, GearSpec)
import LDraw.Connectivity as Connectivity
    exposing
        ( ConnectionPoint
        , ConnectionType(..)
        , SnapCandidate
        , SnapGuide(..)
        , WorldConnectionPoint
        )
import LDraw.Document exposing (Placement)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


{-| Check whether an LDraw part filename is a rotatable gear in `Data.gearParts`.
Fixed parts such as the 99009 turntable base are not mesh-snap partners.
-}
findGearSpec : String -> Maybe GearSpec
findGearSpec file =
    Data.gearParts
        |> List.filter (\spec -> spec.partFile == file && spec.rotates)
        |> List.head


{-| Whether a snap candidate represents a gear mesh snap.

Gear mesh snaps are identified by both dragged and target connection types being
`TechnicAxleFemale`, which cannot interface in physical connection snapping.

-}
isGearSnap : SnapCandidate -> Bool
isGearSnap snap =
    case ( snap.draggedPoint.connType, snap.targetPoint.connType ) of
        ( TechnicAxleFemale _, TechnicAxleFemale _ ) ->
            True

        _ ->
            False


{-| Cardinal offsets in the gear disc plane (local XY) at distance `dist`.
-}
cardinalOffsets : Float -> List Vec3
cardinalOffsets dist =
    [ vec3 dist 0 0
    , vec3 -dist 0 0
    , vec3 0 dist 0
    , vec3 0 -dist 0
    ]


{-| Extract the unit axle axis from a gear's configured local axis and world matrix.
-}
gearAxis : GearSpec -> Mat4 -> Vec3
gearAxis spec mat =
    let
        origin =
            Mat4.transform mat (vec3 0 0 0)

        localAxis =
            localGearAxis spec

        raw =
            Vec3.sub (Mat4.transform mat localAxis) origin

        len =
            Vec3.length raw
    in
    if len < 1.0e-6 then
        localAxis

    else
        Vec3.scale (1 / len) raw


localGearAxis : GearSpec -> Vec3
localGearAxis spec =
    case spec.axis of
        AxisY ->
            vec3 0 1 0

        AxisZ ->
            vec3 0 0 1


type alias CandidatePoint =
    { offset : Vec3
    , worldPos : Vec3
    , distance : Float
    }


{-| Generate candidate mesh positions around a partner gear for a dragged gear position.
Returns cardinal candidates within threshold, or falling back to the closest point
on the pitch circle.
-}
candidateMeshPositions : GearAxis -> Float -> Float -> Mat4 -> Vec3 -> List CandidatePoint
candidateMeshPositions axis meshDistance threshold targetMatrix draggedWorldPos =
    let
        invTarget =
            Connectivity.invertRigid targetMatrix

        draggedLocal =
            Mat4.transform invTarget draggedWorldPos

        planeCoordinates =
            case axis of
                AxisY ->
                    ( Vec3.getX draggedLocal, Vec3.getZ draggedLocal )

                AxisZ ->
                    ( Vec3.getX draggedLocal, Vec3.getY draggedLocal )

        ( x, y ) =
            planeCoordinates

        r =
            sqrt (x * x + y * y)

        cardinalCandidates =
            cardinalOffsetsForAxis axis meshDistance
                |> List.map
                    (\offset ->
                        let
                            worldPos =
                                Mat4.transform targetMatrix offset

                            dist =
                                Vec3.distance draggedWorldPos worldPos
                        in
                        { offset = offset
                        , worldPos = worldPos
                        , distance = dist
                        }
                    )
                |> List.filter (\c -> c.distance <= threshold)
                |> List.sortBy .distance
    in
    case cardinalCandidates of
        best :: rest ->
            best :: rest

        [] ->
            if r > 1.0e-4 then
                let
                    circleOffset =
                        case axis of
                            AxisY ->
                                vec3 (meshDistance * x / r) 0 (meshDistance * y / r)

                            AxisZ ->
                                vec3 (meshDistance * x / r) (meshDistance * y / r) 0

                    worldPos =
                        Mat4.transform targetMatrix circleOffset

                    dist =
                        Vec3.distance draggedWorldPos worldPos
                in
                if dist <= threshold then
                    [ { offset = circleOffset
                      , worldPos = worldPos
                      , distance = dist
                      }
                    ]

                else
                    []

            else
                []


cardinalOffsetsForAxis : GearAxis -> Float -> List Vec3
cardinalOffsetsForAxis axis distance =
    case axis of
        AxisY ->
            [ vec3 distance 0 0
            , vec3 -distance 0 0
            , vec3 0 0 distance
            , vec3 0 0 -distance
            ]

        AxisZ ->
            cardinalOffsets distance


{-| Whether two gears can form a perpendicular mesh (bevel, crown, or worm).
-}
isPerpendicularCandidate : GearSpec -> GearSpec -> Bool
isPerpendicularCandidate s1 s2 =
    case ( s1.kind, s2.kind ) of
        ( Gear.Types.BevelGear, _ ) ->
            True

        ( _, Gear.Types.BevelGear ) ->
            True

        ( Gear.Types.CrownGear, _ ) ->
            True

        ( _, Gear.Types.CrownGear ) ->
            True

        ( Gear.Types.WormGear, _ ) ->
            True

        ( _, Gear.Types.WormGear ) ->
            True

        _ ->
            False


{-| Find the best gear mesh snap candidate for the currently dragged gear among
existing placements in the document.
-}
solveGearSnap :
    (SnapCandidate -> Bool)
    -> Float
    -> Maybe SnapCandidate
    -> Placement
    -> Mat4
    -> List Placement
    -> Maybe SnapCandidate
solveGearSnap isValid snapDistance activeSnap draggedPlacement draggedSurfaceMatrix otherPlacements =
    case findGearSpec draggedPlacement.part of
        Nothing ->
            Nothing

        Just dragSpec ->
            let
                partnerGears =
                    otherPlacements
                        |> List.filterMap
                            (\p ->
                                findGearSpec p.part
                                    |> Maybe.map (\spec -> ( p, spec ))
                            )

                draggedWorldPos =
                    Mat4.transform draggedSurfaceMatrix (vec3 0 0 0)

                draggedAxle =
                    gearAxis dragSpec draggedSurfaceMatrix

                evaluatePartner ( targetP, targetSpec ) =
                    let
                        targetAxle =
                            gearAxis targetSpec targetP.matrix

                        absDot =
                            abs (Vec3.dot draggedAxle targetAxle)
                    in
                    if absDot >= 0.5 then
                        let
                            meshDistance =
                                dragSpec.pitchRadius + targetSpec.pitchRadius

                            isActive =
                                case activeSnap of
                                    Just a ->
                                        a.targetPlacementId == targetP.id && isGearSnap a

                                    Nothing ->
                                        False

                            enterThreshold =
                                max 24.0 (snapDistance * 1.5)

                            releaseThreshold =
                                max 32.0 (enterThreshold * 1.5)

                            threshold =
                                if isActive then
                                    releaseThreshold

                                else
                                    enterThreshold

                            candidates =
                                candidateMeshPositions targetSpec.axis meshDistance threshold targetP.matrix draggedWorldPos

                            isOccupied pos =
                                List.any
                                    (\p ->
                                        p.id
                                            /= targetP.id
                                            && Vec3.distance (Mat4.transform p.matrix (vec3 0 0 0)) pos
                                            < 5.0
                                    )
                                    otherPlacements

                            buildCandidate cand =
                                if isOccupied cand.worldPos then
                                    Nothing

                                else
                                    let
                                        baseSnapMatrix =
                                            Mat4.mul targetP.matrix (Mat4.makeTranslate cand.offset)

                                        partnerInst =
                                            { id = targetP.id
                                            , spec = targetSpec
                                            , color = targetP.color
                                            , worldPosition = Mat4.transform targetP.matrix (vec3 0 0 0)
                                            , worldMatrix = targetP.matrix
                                            }

                                        movingInst =
                                            { id = draggedPlacement.id
                                            , spec = dragSpec
                                            , color = draggedPlacement.color
                                            , worldPosition = cand.worldPos
                                            , worldMatrix = baseSnapMatrix
                                            }

                                        snapMatrix =
                                            case Phase.meshCorrection { fixed = partnerInst, moving = movingInst } of
                                                Just correction ->
                                                    Mat4.mul baseSnapMatrix (Mat4.makeRotate correction.radians (localGearAxis dragSpec))

                                                Nothing ->
                                                    baseSnapMatrix

                                        candidate =
                                            { draggedPoint =
                                                { id = 1
                                                , connType = TechnicAxleFemale { length = 10 }
                                                , localPosition = vec3 0 0 0
                                                , normal = vec3 0 0 1
                                                , tangent = vec3 1 0 0
                                                }
                                            , targetPoint =
                                                { pointId = 1
                                                , placementId = targetP.id
                                                , connType = TechnicAxleFemale { length = 10 }
                                                , worldPosition = cand.worldPos
                                                , worldNormal = targetAxle
                                                , worldTangent = Mat4.transform targetP.matrix (vec3 1 0 0)
                                                }
                                            , targetPlacementId = targetP.id
                                            , targetWorldPosition = cand.worldPos
                                            , targetWorldNormal = targetAxle
                                            , snapMatrix = snapMatrix
                                            , distance = cand.distance
                                            , guide = AxleAxisGuide cand.worldPos targetAxle False
                                            }
                                    in
                                    if isValid candidate then
                                        Just candidate

                                    else
                                        Nothing
                        in
                        List.filterMap buildCandidate candidates

                    else if absDot <= 0.35 && isPerpendicularCandidate dragSpec targetSpec then
                        let
                            targetCenter =
                                Mat4.transform targetP.matrix (vec3 0 0 0)

                            meshDistance =
                                dragSpec.pitchRadius + targetSpec.pitchRadius

                            isActive =
                                case activeSnap of
                                    Just a ->
                                        a.targetPlacementId == targetP.id && isGearSnap a

                                    Nothing ->
                                        False

                            enterThreshold =
                                max 24.0 (snapDistance * 1.5)

                            releaseThreshold =
                                max 32.0 (enterThreshold * 1.5)

                            threshold =
                                if isActive then
                                    releaseThreshold

                                else
                                    enterThreshold

                            cross =
                                Vec3.cross targetAxle draggedAxle

                            crossLen =
                                Vec3.length cross
                        in
                        if crossLen < 0.01 then
                            []

                        else
                            let
                                normal =
                                    Vec3.scale (1.0 / crossLen) cross

                                targetAxleCandidate =
                                    let
                                        c =
                                            Vec3.cross targetAxle normal
                                    in
                                    if Vec3.dot draggedAxle c >= 0 then
                                        c

                                    else
                                        Vec3.scale -1.0 c

                                cosTheta =
                                    clamp -1.0 1.0 (Vec3.dot draggedAxle targetAxleCandidate)

                                sinTheta =
                                    Vec3.dot normal (Vec3.cross draggedAxle targetAxleCandidate)

                                theta =
                                    atan2 sinTheta cosTheta

                                candidateWorldPos s =
                                    Vec3.add targetCenter (Vec3.scale (s * meshDistance) normal)

                                cand1Pos =
                                    candidateWorldPos 1.0

                                cand2Pos =
                                    candidateWorldPos -1.0

                                candPoints =
                                    [ { worldPos = cand1Pos, distance = Vec3.distance draggedWorldPos cand1Pos }
                                    , { worldPos = cand2Pos, distance = Vec3.distance draggedWorldPos cand2Pos }
                                    ]
                                        |> List.filter (\c -> c.distance <= threshold)
                                        |> List.sortBy .distance

                                isOccupied pos =
                                    List.any
                                        (\p ->
                                            p.id
                                                /= targetP.id
                                                && Vec3.distance (Mat4.transform p.matrix (vec3 0 0 0)) pos
                                                < 5.0
                                        )
                                        otherPlacements

                                buildCandidate cand =
                                    if isOccupied cand.worldPos then
                                        Nothing

                                    else
                                        let
                                            delta =
                                                Vec3.sub cand.worldPos draggedWorldPos

                                            translatedMat =
                                                Mat4.mul (Mat4.makeTranslate delta) draggedSurfaceMatrix

                                            alignedMat =
                                                if abs theta > 1.0e-4 then
                                                    Mat4.mul (Mat4.makeTranslate cand.worldPos)
                                                        (Mat4.mul (Mat4.makeRotate theta normal)
                                                            (Mat4.mul (Mat4.makeTranslate (Vec3.scale -1.0 cand.worldPos)) translatedMat)
                                                        )

                                                else
                                                    translatedMat

                                            partnerInst =
                                                { id = targetP.id
                                                , spec = targetSpec
                                                , color = targetP.color
                                                , worldPosition = targetCenter
                                                , worldMatrix = targetP.matrix
                                                }

                                            movingInst =
                                                { id = draggedPlacement.id
                                                , spec = dragSpec
                                                , color = draggedPlacement.color
                                                , worldPosition = cand.worldPos
                                                , worldMatrix = alignedMat
                                                }

                                            snapMatrix =
                                                case Phase.meshCorrection { fixed = partnerInst, moving = movingInst } of
                                                    Just correction ->
                                                        Mat4.mul alignedMat (Mat4.makeRotate correction.radians (localGearAxis dragSpec))

                                                    Nothing ->
                                                        alignedMat

                                            candidate =
                                                { draggedPoint =
                                                    { id = 1
                                                    , connType = TechnicAxleFemale { length = 10 }
                                                    , localPosition = vec3 0 0 0
                                                    , normal = vec3 0 0 1
                                                    , tangent = vec3 1 0 0
                                                    }
                                                , targetPoint =
                                                    { pointId = 1
                                                    , placementId = targetP.id
                                                    , connType = TechnicAxleFemale { length = 10 }
                                                    , worldPosition = cand.worldPos
                                                    , worldNormal = targetAxleCandidate
                                                    , worldTangent = normal
                                                    }
                                                , targetPlacementId = targetP.id
                                                , targetWorldPosition = cand.worldPos
                                                , targetWorldNormal = targetAxleCandidate
                                                , snapMatrix = snapMatrix
                                                , distance = cand.distance
                                                , guide = AxleAxisGuide cand.worldPos targetAxleCandidate False
                                                }
                                        in
                                        if isValid candidate then
                                            Just candidate

                                        else
                                            Nothing
                            in
                            List.filterMap buildCandidate candPoints

                    else
                        []
            in
            partnerGears
                |> List.concatMap evaluatePartner
                |> List.sortBy .distance
                |> List.head
