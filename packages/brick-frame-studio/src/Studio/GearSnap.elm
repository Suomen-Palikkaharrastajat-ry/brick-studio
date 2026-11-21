module Studio.GearSnap exposing
    ( candidateMeshPositions
    , cardinalOffsets
    , findGearSpec
    , isGearSnap
    , solveGearSnap
    )

{-| Gear mesh snapping: magnetic alignment of meshing gears along their pitch circles.

When a gear is dragged near an existing partner gear in the document, this module
detects valid meshing positions (at distance `D = R_drag + R_target` in the
partner's plane for parallel axles, on crossing axles at the parts' reference
offsets for bevel and crown gears, see `Gear.Detect.crossedLayouts`, or on the
common normal for worms) and sets the tooth rotation angle so the gear teeth
interlock. Every snap satisfies `Gear.Detect.meshing`.

-}

import Data
import Gear.Detect as Detect
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


{-| Rotation (about the gear's own origin) that maps the dragged gear's local
axle onto the target's local axle, so a parallel snap copies the target's
orientation but keeps the dragged gear's axle along the target's axle. The
identity when both use the same local axis.
-}
alignLocalAxis : GearSpec -> GearSpec -> Mat4
alignLocalAxis dragSpec targetSpec =
    case ( dragSpec.axis, targetSpec.axis ) of
        ( AxisZ, AxisY ) ->
            -- (0, 0, 1) onto (0, 1, 0)
            Mat4.makeRotate (-pi / 2) (vec3 1 0 0)

        ( AxisY, AxisZ ) ->
            -- (0, 1, 0) onto (0, 0, 1)
            Mat4.makeRotate (pi / 2) (vec3 1 0 0)

        _ ->
            Mat4.identity


{-| The world directions of a gear's local in-plane axes (the two local axes
perpendicular to its axle), both signs.
-}
planeDirections : GearSpec -> Mat4 -> List Vec3
planeDirections spec mat =
    let
        dir v =
            Vec3.sub (Mat4.transform mat v) (Mat4.transform mat (vec3 0 0 0))
                |> Vec3.normalize

        ( e1, e2 ) =
            case spec.axis of
                AxisY ->
                    ( dir (vec3 1 0 0), dir (vec3 0 0 1) )

                AxisZ ->
                    ( dir (vec3 1 0 0), dir (vec3 0 1 0) )
    in
    [ e1, Vec3.negate e1, e2, Vec3.negate e2 ]


{-| Smallest rotation taking unit vector `from` onto unit vector `to`, with its
angle. For opposite vectors it turns by pi about `fallbackAxis` (which must be
perpendicular to both).
-}
rotationBetween : Vec3 -> Vec3 -> Vec3 -> ( Float, Mat4 )
rotationBetween from to fallbackAxis =
    let
        k =
            Vec3.cross from to

        sinT =
            Vec3.length k

        cosT =
            clamp -1 1 (Vec3.dot from to)
    in
    if sinT < 1.0e-6 then
        if cosT > 0 then
            ( 0, Mat4.identity )

        else
            ( pi, Mat4.makeRotate pi fallbackAxis )

    else
        let
            angle =
                atan2 sinT cosT
        in
        ( angle, Mat4.makeRotate angle (Vec3.scale (1 / sinT) k) )


{-| Find the best gear mesh snap candidate for the currently dragged gear among
existing placements in the document.

Three kinds of snap are tried against each partner gear:

  - **parallel** (axles within 60° of each other): the dragged gear is put in
    the partner's plane at the sum of the pitch radii, with its own axle along
    the partner's axle;
  - **crossed** (axles near right angles, and the two parts have reference
    layouts in `Gear.Detect.crossedLayouts`): the crossing point lies on the
    partner's axle at the partner's offset, the dragged gear sits at its own
    offset from it, and its axle is turned to cross the partner's axle with its
    teeth toward the crossing;
  - **worm** (axles near right angles, one gear a worm): the dragged gear is put
    on the common normal of the two axles at the sum of the pitch radii.

Every candidate must satisfy `Gear.Detect.meshing` with the partner (the rule
the simulation uses), so a snapped pair is always connected. The tooth phase is
aligned with `Gear.Phase.meshCorrection`.

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

                dragged =
                    { spec = dragSpec
                    , matrix = draggedSurfaceMatrix
                    , position = Mat4.transform draggedSurfaceMatrix (vec3 0 0 0)
                    , axle = gearAxis dragSpec draggedSurfaceMatrix
                    }

                enterThreshold =
                    max 24.0 (snapDistance * 1.5)

                releaseThreshold =
                    max 32.0 (enterThreshold * 1.5)

                evaluatePartner ( targetP, targetSpec ) =
                    let
                        target =
                            { spec = targetSpec
                            , matrix = targetP.matrix
                            , position = Mat4.transform targetP.matrix (vec3 0 0 0)
                            , axle = gearAxis targetSpec targetP.matrix
                            }

                        absDot =
                            abs (Vec3.dot dragged.axle target.axle)

                        isActive =
                            case activeSnap of
                                Just a ->
                                    a.targetPlacementId == targetP.id && isGearSnap a

                                Nothing ->
                                    False

                        threshold =
                            if isActive then
                                releaseThreshold

                            else
                                enterThreshold

                        isOccupied pos =
                            List.any
                                (\p ->
                                    p.id
                                        /= targetP.id
                                        && Vec3.distance (Mat4.transform p.matrix (vec3 0 0 0)) pos
                                        < 5.0
                                )
                                otherPlacements

                        partnerInst =
                            { id = targetP.id
                            , spec = targetSpec
                            , color = targetP.color
                            , worldPosition = target.position
                            , worldMatrix = targetP.matrix
                            }

                        -- Align the tooth phase, check the mesh rule and the
                        -- host's validity, and build the snap candidate.
                        finish worldPos baseMatrix normal tangent =
                            if isOccupied worldPos then
                                Nothing

                            else
                                let
                                    movingInst =
                                        { id = draggedPlacement.id
                                        , spec = dragSpec
                                        , color = draggedPlacement.color
                                        , worldPosition = worldPos
                                        , worldMatrix = baseMatrix
                                        }

                                    snapMatrix =
                                        case Phase.meshCorrection { fixed = partnerInst, moving = movingInst } of
                                            Just correction ->
                                                Mat4.mul baseMatrix (Mat4.makeRotate correction.radians (localGearAxis dragSpec))

                                            Nothing ->
                                                baseMatrix

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
                                            , worldPosition = worldPos
                                            , worldNormal = normal
                                            , worldTangent = tangent
                                            }
                                        , targetPlacementId = targetP.id
                                        , targetWorldPosition = worldPos
                                        , targetWorldNormal = normal
                                        , snapMatrix = snapMatrix
                                        , distance = Vec3.distance dragged.position worldPos
                                        , guide = AxleAxisGuide worldPos normal False
                                        }
                                in
                                if
                                    Detect.meshing partnerInst { movingInst | worldMatrix = snapMatrix }
                                        && isValid candidate
                                then
                                    Just candidate

                                else
                                    Nothing

                        layouts =
                            Detect.crossedLayouts dragSpec targetSpec
                    in
                    if absDot >= 0.5 then
                        parallelCandidates threshold finish dragged target

                    else if absDot <= 0.35 && not (List.isEmpty layouts) then
                        crossedCandidates threshold finish layouts dragged target

                    else if absDot <= 0.35 && (Detect.isWorm dragSpec || Detect.isWorm targetSpec) then
                        wormCandidates threshold finish dragged target

                    else
                        []
            in
            partnerGears
                |> List.concatMap evaluatePartner
                |> List.sortBy .distance
                |> List.head


{-| A gear taking part in a snap: its spec, world matrix, centre and unit axle.
-}
type alias SnapGear =
    { spec : GearSpec
    , matrix : Mat4
    , position : Vec3
    , axle : Vec3
    }


{-| Builds a snap candidate from the dragged world position, the dragged gear's
matrix before tooth alignment, and the guide normal and tangent; `Nothing` when
the result does not mesh or is not valid.
-}
type alias Finish =
    Vec3 -> Mat4 -> Vec3 -> Vec3 -> Maybe SnapCandidate


parallelCandidates : Float -> Finish -> SnapGear -> SnapGear -> List SnapCandidate
parallelCandidates threshold finish dragged target =
    let
        meshDistance =
            dragged.spec.pitchRadius + target.spec.pitchRadius

        align =
            alignLocalAxis dragged.spec target.spec
    in
    candidateMeshPositions target.spec.axis meshDistance threshold target.matrix dragged.position
        |> List.filterMap
            (\cand ->
                finish cand.worldPos
                    (Mat4.mul target.matrix (Mat4.mul (Mat4.makeTranslate cand.offset) align))
                    target.axle
                    (Mat4.transform target.matrix (vec3 1 0 0))
            )


{-| Crossed (bevel / crown) snaps. For each reference layout the crossing point
`O` lies on the target's axle at the target's offset. The dragged centre goes
at the dragged gear's offset from `O` along a direction `u` in the target's
plane (a cardinal direction of the target when one is within reach, otherwise
the direction toward the dragged gear), and the dragged axle is turned onto
`∓u` so that `O` lies on the dragged gear's tooth side. Only the closest
candidate is kept, ties going to the smaller rotation of the dragged gear.
-}
crossedCandidates :
    Float
    -> Finish
    -> List { firstOffset : Float, secondOffset : Float }
    -> SnapGear
    -> SnapGear
    -> List SnapCandidate
crossedCandidates threshold finish layouts dragged target =
    let
        directionsFor crossing dragOffset =
            let
                centreAlong u =
                    Vec3.add crossing (Vec3.scale (abs dragOffset) u)

                cardinal =
                    planeDirections target.spec target.matrix
                        |> List.filter (\u -> Vec3.distance (centreAlong u) dragged.position <= threshold)
            in
            if List.isEmpty cardinal then
                let
                    w0 =
                        Vec3.sub dragged.position crossing

                    w =
                        Vec3.sub w0 (Vec3.scale (Vec3.dot w0 target.axle) target.axle)
                in
                if Vec3.length w > 1.0e-4 then
                    [ Vec3.normalize w ]

                else
                    []

            else
                cardinal

        build layout =
            let
                crossing =
                    Vec3.add target.position (Vec3.scale layout.secondOffset target.axle)
            in
            directionsFor crossing layout.firstOffset
                |> List.filterMap
                    (\u ->
                        let
                            centre =
                                Vec3.add crossing (Vec3.scale (abs layout.firstOffset) u)

                            newAxle =
                                if layout.firstOffset >= 0 then
                                    Vec3.negate u

                                else
                                    u

                            ( angle, rotation ) =
                                rotationBetween dragged.axle newAxle target.axle

                            baseMatrix =
                                Mat4.mul (Mat4.makeTranslate centre)
                                    (Mat4.mul rotation
                                        (Mat4.mul (Mat4.makeTranslate (Vec3.negate dragged.position)) dragged.matrix)
                                    )
                        in
                        if Vec3.distance centre dragged.position <= threshold then
                            finish centre baseMatrix newAxle target.axle
                                |> Maybe.map (\c -> ( ( c.distance, angle ), c ))

                        else
                            Nothing
                    )
    in
    List.concatMap build layouts
        |> List.sortBy Tuple.first
        |> List.head
        |> Maybe.map (Tuple.second >> List.singleton)
        |> Maybe.withDefault []


{-| Worm snaps: the dragged gear goes on the common normal of the two axles at
the sum of the pitch radii, its axle turned to be exactly perpendicular.
-}
wormCandidates : Float -> Finish -> SnapGear -> SnapGear -> List SnapCandidate
wormCandidates threshold finish dragged target =
    let
        meshDistance =
            dragged.spec.pitchRadius + target.spec.pitchRadius

        cross =
            Vec3.cross target.axle dragged.axle

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
                        Vec3.cross target.axle normal
                in
                if Vec3.dot dragged.axle c >= 0 then
                    c

                else
                    Vec3.scale -1.0 c

            cosTheta =
                clamp -1.0 1.0 (Vec3.dot dragged.axle targetAxleCandidate)

            sinTheta =
                Vec3.dot normal (Vec3.cross dragged.axle targetAxleCandidate)

            theta =
                atan2 sinTheta cosTheta

            candidateWorldPos s =
                Vec3.add target.position (Vec3.scale (s * meshDistance) normal)

            buildCandidate pos =
                let
                    delta =
                        Vec3.sub pos dragged.position

                    translatedMat =
                        Mat4.mul (Mat4.makeTranslate delta) dragged.matrix

                    alignedMat =
                        if abs theta > 1.0e-4 then
                            Mat4.mul (Mat4.makeTranslate pos)
                                (Mat4.mul (Mat4.makeRotate theta normal)
                                    (Mat4.mul (Mat4.makeTranslate (Vec3.scale -1.0 pos)) translatedMat)
                                )

                        else
                            translatedMat
                in
                finish pos alignedMat targetAxleCandidate normal
        in
        [ candidateWorldPos 1.0, candidateWorldPos -1.0 ]
            |> List.filter (\pos -> Vec3.distance dragged.position pos <= threshold)
            |> List.sortBy (Vec3.distance dragged.position)
            |> List.filterMap buildCandidate
