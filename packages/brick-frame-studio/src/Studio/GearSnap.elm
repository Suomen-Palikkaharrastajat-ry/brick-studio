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
import Gear.Types exposing (GearInstance, GearSpec)
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


{-| Check whether an LDraw part filename is a known gear in `Data.gearParts`.
-}
findGearSpec : String -> Maybe GearSpec
findGearSpec file =
    Data.gearParts
        |> List.filter (\spec -> spec.partFile == file)
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


{-| Extract the unit axle axis (local +Z in world space) from a 4x4 transform matrix.
-}
gearAxis : Mat4 -> Vec3
gearAxis mat =
    let
        origin =
            Mat4.transform mat (vec3 0 0 0)

        alongZ =
            Mat4.transform mat (vec3 0 0 1)

        raw =
            Vec3.sub alongZ origin

        len =
            Vec3.length raw
    in
    if len < 1.0e-6 then
        vec3 0 0 1

    else
        Vec3.scale (1 / len) raw


type alias CandidatePoint =
    { offset : Vec3
    , worldPos : Vec3
    , distance : Float
    }


{-| Generate candidate mesh positions around a partner gear for a dragged gear position.
Returns cardinal candidates within threshold, or falling back to the closest point
on the pitch circle.
-}
candidateMeshPositions : Float -> Float -> Mat4 -> Vec3 -> List CandidatePoint
candidateMeshPositions meshDistance threshold targetMatrix draggedWorldPos =
    let
        invTarget =
            Connectivity.invertRigid targetMatrix

        draggedLocal =
            Mat4.transform invTarget draggedWorldPos

        x =
            Vec3.getX draggedLocal

        y =
            Vec3.getY draggedLocal

        r =
            sqrt (x * x + y * y)

        cardinalCandidates =
            cardinalOffsets meshDistance
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
                    gearAxis draggedSurfaceMatrix

                evaluatePartner ( targetP, targetSpec ) =
                    let
                        targetAxle =
                            gearAxis targetP.matrix

                        absDot =
                            abs (Vec3.dot draggedAxle targetAxle)
                    in
                    if absDot < 0.5 then
                        []

                    else
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
                                candidateMeshPositions meshDistance threshold targetP.matrix draggedWorldPos

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
                                                    Mat4.mul baseSnapMatrix (Mat4.makeRotate correction.radians (vec3 0 0 1))

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
            in
            partnerGears
                |> List.concatMap evaluatePartner
                |> List.sortBy .distance
                |> List.head
