module LDraw.Connectivity exposing
    ( BoundingBox3D
    , CollisionMode(..)
    , ConnectionPoint
    , ConnectionType(..)
    , OBB
    , PartConnectivity
    , SnapCandidate
    , SnapGuide(..)
    , SnapPriority(..)
    , WorldConnectionPoint
    , alignedContactCount
    , checkCollisions
    , defaultSnapDistance
    , effectiveEnvelope
    , extractConnections
    , extractPartConnectivity
    , invertRigid
    , isCompatible
    , isSnapColliding
    , isStudded
    , isUpright
    , knownCatalogPart
    , makeAxle
    , makeGear
    , makeGearPinHoles
    , makeSlope
    , makeTire
    , makeWheelRim
    , setGuideColliding
    , snapEnterDistance
    , snapReleaseDistance
    , solveSnap
    , solveSnapCandidates
    , solveSnapCandidatesWithPriority
    , solveSnapWithHysteresis
    , solveSnapWithHysteresisAndPredicate
    , solveSnapWithHysteresisAndPredicateWithPriority
    , solveSnapWithPredicate
    , solveSnapWithPredicateWithPriority
    , studOffset
    , testOBB
    , toOBB
    , transformDirection
    , transformPoint
    )

{-| Procedural connection point detection, magnetic snapping, and 3D SAT
bounding volume collision detection for LDraw models.

Supports studs, anti-studs, Technic pins and axles, clips, bars, and ball
joints.

-}

import Dict exposing (Dict)
import LDraw.Types exposing (LDrawLine(..), SubFileData)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set


{-| Standard connection types used across the Lego and LDraw ecosystem.
-}
type ConnectionType
    = Stud
    | AntiStud
    | TechnicPinMale { length : Float, friction : Bool }
    | TechnicPinFemale { length : Float }
    | TechnicAxleMale { length : Float }
    | TechnicAxleFemale { length : Float }
    | ClipMale { width : Float }
    | BarFemale { diameter : Float }
    | BallMale
    | BallSocketFemale
    | WheelRim { diameter : Float }
    | WheelTire { diameter : Float }


{-| A local connection site on a part.
`normal` points outward along the connection engagement axis.
`tangent` provides the reference orientation for rotation snapping.
-}
type alias ConnectionPoint =
    { id : Int
    , connType : ConnectionType
    , localPosition : Vec3
    , normal : Vec3
    , tangent : Vec3
    }


{-| An axis-aligned 3D bounding box in local part space.
-}
type alias BoundingBox3D =
    { min : Vec3
    , max : Vec3
    }


{-| Connectivity and collision descriptor for a part.
-}
type alias PartConnectivity =
    { part : String
    , connections : List ConnectionPoint
    , boundingBoxes : List BoundingBox3D
    }


{-| A connection point transformed into world coordinates by its placement matrix.
-}
type alias WorldConnectionPoint =
    { placementId : Int
    , pointId : Int
    , connType : ConnectionType
    , worldPosition : Vec3
    , worldNormal : Vec3
    , worldTangent : Vec3
    }


{-| An Oriented Bounding Box in 3D for fast Separating Axis Theorem (SAT) tests.
-}
type alias OBB =
    { center : Vec3
    , halfExtents : Vec3
    , axisX : Vec3
    , axisY : Vec3
    , axisZ : Vec3
    }


{-| Collision resolution behaviour in the editor.
-}
type CollisionMode
    = CollisionStrict
    | CollisionSoft
    | CollisionOff


{-| How candidate snaps are ranked when more than one connection is reachable.
-}
type SnapPriority
    = MaximizeContacts
    | ClosestPoint


{-| Visual guide displayed in the WebGL overlay when snapping.
-}
type SnapGuide
    = StudRingGuide Vec3 Vec3 Bool
    | AxleAxisGuide Vec3 Vec3 Bool
    | ClipLockGuide Vec3 Vec3 Bool


{-| A solved magnetic snap alignment.
-}
type alias SnapCandidate =
    { draggedPoint : ConnectionPoint
    , targetPoint : WorldConnectionPoint
    , targetPlacementId : Int
    , targetWorldPosition : Vec3
    , targetWorldNormal : Vec3
    , snapMatrix : Mat4
    , distance : Float
    , guide : SnapGuide
    }


{-| Default magnetic snap activation threshold in LDU.
-}
defaultSnapDistance : Float
defaultSnapDistance =
    12.0


{-| Entry threshold for magnetic snapping in LDU.
-}
snapEnterDistance : Float
snapEnterDistance =
    12.0


{-| Release threshold for magnetic snapping hysteresis in LDU.
-}
snapReleaseDistance : Float
snapReleaseDistance =
    18.0



-- ── Compatibility ────────────────────────────────────────────────────────────


{-| Whether two connection types can physically interface.
-}
isCompatible : ConnectionType -> ConnectionType -> Bool
isCompatible a b =
    canConnect a b || canConnect b a


canConnect : ConnectionType -> ConnectionType -> Bool
canConnect a b =
    case ( a, b ) of
        ( Stud, AntiStud ) ->
            True

        ( TechnicPinMale _, TechnicPinFemale _ ) ->
            True

        ( TechnicAxleMale _, TechnicAxleFemale _ ) ->
            True

        ( TechnicAxleMale _, TechnicPinFemale _ ) ->
            True

        ( ClipMale _, BarFemale _ ) ->
            True

        ( BallMale, BallSocketFemale ) ->
            True

        ( WheelRim r, WheelTire t ) ->
            abs (r.diameter - t.diameter) <= 2.5

        _ ->
            False


isAxleMale : ConnectionType -> Bool
isAxleMale conn =
    case conn of
        TechnicAxleMale _ ->
            True

        _ ->
            False


isAxleFemale : ConnectionType -> Bool
isAxleFemale conn =
    case conn of
        TechnicAxleFemale _ ->
            True

        _ ->
            False


{-| Priority bias for connection candidate evaluation.
Negative bias favors higher priority matches (e.g. Stud-AntiStud, or AxleMale-AxleFemale over AxleMale-PinFemale).
-}
connectionPairBias : ConnectionType -> ConnectionType -> Float
connectionPairBias dragged target =
    if dragged == AntiStud && target == Stud then
        -0.5

    else if
        (isAxleMale dragged && isAxleFemale target)
            || (isAxleFemale dragged && isAxleMale target)
    then
        -3.0

    else
        case ( dragged, target ) of
            ( WheelRim _, WheelTire _ ) ->
                -4.0

            ( WheelTire _, WheelRim _ ) ->
                -4.0

            _ ->
                0.0



-- ── Vector & Coordinate Math ──────────────────────────────────────────────────


{-| Transform a direction vector by a 4x4 matrix and normalize it.
-}
transformDirection : Mat4 -> Vec3 -> Vec3
transformDirection matrix dir =
    let
        p0 =
            Mat4.transform matrix (vec3 0 0 0)

        p1 =
            Mat4.transform matrix dir

        d =
            Vec3.sub p1 p0

        len =
            Vec3.length d
    in
    if len < 1.0e-6 then
        dir

    else
        Vec3.scale (1 / len) d


{-| Transform a local connection point into world space.
-}
transformPoint : Mat4 -> Int -> ConnectionPoint -> WorldConnectionPoint
transformPoint matrix placementId point =
    { placementId = placementId
    , pointId = point.id
    , connType = point.connType
    , worldPosition = Mat4.transform matrix point.localPosition
    , worldNormal = transformDirection matrix point.normal
    , worldTangent = transformDirection matrix point.tangent
    }


{-| Invert a rigid 4x4 matrix (pure rotation + translation).
-}
invertRigid : Mat4 -> Mat4
invertRigid matrix =
    let
        m =
            Mat4.toRecord matrix

        tx =
            -(m.m11 * m.m14 + m.m21 * m.m24 + m.m31 * m.m34)

        ty =
            -(m.m12 * m.m14 + m.m22 * m.m24 + m.m32 * m.m34)

        tz =
            -(m.m13 * m.m14 + m.m23 * m.m24 + m.m33 * m.m34)
    in
    Mat4.fromRecord
        { m11 = m.m11
        , m12 = m.m21
        , m13 = m.m31
        , m14 = tx
        , m21 = m.m12
        , m22 = m.m22
        , m23 = m.m32
        , m24 = ty
        , m31 = m.m13
        , m32 = m.m23
        , m33 = m.m33
        , m34 = tz
        , m41 = 0
        , m42 = 0
        , m43 = 0
        , m44 = 1
        }


{-| Update the collision state on a snap guide.
-}
setGuideColliding : Bool -> SnapGuide -> SnapGuide
setGuideColliding col guide =
    case guide of
        StudRingGuide pos norm _ ->
            StudRingGuide pos norm col

        AxleAxisGuide pos ax _ ->
            AxleAxisGuide pos ax col

        ClipLockGuide pos norm _ ->
            ClipLockGuide pos norm col


{-| Build a right-handed orthonormal basis from a normal and a tangent hint.
-}
orthonormalBasis : Vec3 -> Vec3 -> { normal : Vec3, tangent : Vec3, bitangent : Vec3 }
orthonormalBasis normalRaw tangentHint =
    let
        nLen =
            Vec3.length normalRaw

        n =
            if nLen < 1.0e-6 then
                vec3 0 -1 0

            else
                Vec3.scale (1 / nLen) normalRaw

        dotNT =
            Vec3.dot tangentHint n

        proj =
            Vec3.sub tangentHint (Vec3.scale dotNT n)

        projLen =
            Vec3.length proj

        u =
            if projLen < 1.0e-6 then
                let
                    alt =
                        if abs (Vec3.getX n) < 0.9 then
                            vec3 1 0 0

                        else
                            vec3 0 1 0

                    altDot =
                        Vec3.dot alt n

                    altProj =
                        Vec3.sub alt (Vec3.scale altDot n)

                    altProjLen =
                        Vec3.length altProj
                in
                if altProjLen < 1.0e-6 then
                    vec3 1 0 0

                else
                    Vec3.scale (1 / altProjLen) altProj

            else
                Vec3.scale (1 / projLen) proj

        v =
            Vec3.cross n u
    in
    { normal = n, tangent = u, bitangent = v }



-- ── Snap Solver ──────────────────────────────────────────────────────────────


{-| Number of dragged connector sites that would engage this target placement
after a candidate transform. Ranking by this before pointer distance makes a
group snap prefer a full aligned contact grid over a closer one- or two-point
partial overlap.
-}
alignedContactCount : Mat4 -> Int -> List ConnectionPoint -> List WorldConnectionPoint -> Int
alignedContactCount snapMatrix targetPlacementId draggedPoints targetPoints =
    let
        targetsOnPlacement =
            List.filter (.placementId >> (==) targetPlacementId) targetPoints

        matchesTarget dragged =
            let
                draggedPosition =
                    Mat4.transform snapMatrix dragged.localPosition

                draggedNormal =
                    transformDirection snapMatrix dragged.normal
            in
            List.any
                (\target ->
                    -- the distance test first: it rejects nearly every target cheaply
                    if Vec3.distance draggedPosition target.worldPosition > 0.5 || not (isCompatible dragged.connType target.connType) then
                        False

                    else
                        let
                            desiredNormal =
                                targetDesiredNormal dragged.connType draggedNormal target.worldNormal
                        in
                        not (isDirectional dragged.connType)
                            || Vec3.dot draggedNormal desiredNormal
                            >= -0.2
                )
                targetsOnPlacement
    in
    List.filter matchesTarget draggedPoints |> List.length


{-| Find and score all compatible magnetic snap candidates within `maxDist`,
preferring candidates which engage more connector sites, then by score.
-}
solveSnapCandidates :
    Float
    -> Int
    -> Mat4
    -> List ConnectionPoint
    -> List WorldConnectionPoint
    -> List SnapCandidate
solveSnapCandidates maxDist rotationIndex currentMatrix draggedPoints targetPoints =
    solveSnapCandidatesWithPriority MaximizeContacts maxDist rotationIndex currentMatrix draggedPoints targetPoints


{-| Candidate generator with an explicit ranking strategy. `ClosestPoint` is
used for Shift-drag, which allows a user to choose a nearby staggered contact;
the default strategy prefers the largest aligned connector grid.
-}
solveSnapCandidatesWithPriority :
    SnapPriority
    -> Float
    -> Int
    -> Mat4
    -> List ConnectionPoint
    -> List WorldConnectionPoint
    -> List SnapCandidate
solveSnapCandidatesWithPriority priority maxDist rotationIndex currentMatrix draggedPoints targetPoints =
    let
        accessibleTargetPoints =
            List.filter
                (\tp ->
                    not (tp.connType == AntiStud && Vec3.getY tp.worldPosition >= -0.01 && Vec3.getY tp.worldNormal > 0.7)
                )
                targetPoints

        evalPair ( draggedLocal, draggedWorldPos ) targetWorld =
            if not (isCompatible draggedLocal.connType targetWorld.connType) then
                Nothing

            else
                let
                    dist =
                        Vec3.distance draggedWorldPos targetWorld.worldPosition
                in
                if dist > maxDist then
                    Nothing

                else
                    let
                        draggedWorldNormal =
                            transformDirection currentMatrix draggedLocal.normal

                        desiredNormal =
                            targetDesiredNormal draggedLocal.connType draggedWorldNormal targetWorld.worldNormal

                        cosAngle =
                            Vec3.dot draggedWorldNormal desiredNormal
                    in
                    if isDirectional draggedLocal.connType && cosAngle < -0.2 then
                        Nothing

                    else
                        let
                            snapMat =
                                computeSnapMatrix currentMatrix rotationIndex draggedLocal targetWorld desiredNormal

                            originY =
                                Vec3.getY (Mat4.transform snapMat (vec3 0 0 0))

                            draggedY =
                                Vec3.getY (Mat4.transform snapMat draggedLocal.localPosition)

                            isPenetratingGround =
                                if abs (Vec3.getY targetWorld.worldNormal) > 0.7 && Vec3.getY targetWorld.worldPosition <= 0.5 then
                                    originY
                                        > 0.5
                                        || draggedY
                                        > 0.5
                                        || List.any (\pt -> Vec3.getY (Mat4.transform snapMat pt.localPosition) > 0.5) draggedPoints

                                else
                                    False
                        in
                        if isPenetratingGround then
                            Nothing

                        else
                            let
                                bias =
                                    connectionPairBias draggedLocal.connType targetWorld.connType

                                guide =
                                    makeGuide draggedLocal.connType targetWorld.worldPosition targetWorld.worldNormal False

                                contactCount =
                                    alignedContactCount snapMat targetWorld.placementId draggedPoints accessibleTargetPoints

                                contactBonus =
                                    case priority of
                                        MaximizeContacts ->
                                            toFloat (contactCount - 1) * 2.5

                                        ClosestPoint ->
                                            0.0

                                score =
                                    dist + (1.0 - cosAngle) * 5.0 + bias - contactBonus
                            in
                            Just
                                ( contactCount
                                , score
                                , { draggedPoint = draggedLocal
                                  , targetPoint = targetWorld
                                  , targetPlacementId = targetWorld.placementId
                                  , targetWorldPosition = targetWorld.worldPosition
                                  , targetWorldNormal = targetWorld.worldNormal
                                  , snapMatrix = snapMat
                                  , distance = dist
                                  , guide = guide
                                  }
                                )

        -- each dragged point's world position once, not once per target
        candidates =
            draggedPoints
                |> List.concatMap
                    (\d ->
                        List.filterMap (evalPair ( d, Mat4.transform currentMatrix d.localPosition )) accessibleTargetPoints
                    )

        rankedCandidates =
            List.sortBy (\( _, score, _ ) -> score) candidates
    in
    List.map (\( _, _, candidate ) -> candidate) rankedCandidates


{-| Solve the best magnetic snap transformation satisfying a validation predicate.
Returns the highest-scoring candidate for which `isValid candidate` is True.
-}
solveSnapWithPredicate :
    (SnapCandidate -> Bool)
    -> Float
    -> Int
    -> Mat4
    -> List ConnectionPoint
    -> List WorldConnectionPoint
    -> Maybe SnapCandidate
solveSnapWithPredicate isValid maxDist rotationIndex currentMatrix draggedPoints targetPoints =
    solveSnapWithPredicateWithPriority MaximizeContacts isValid maxDist rotationIndex currentMatrix draggedPoints targetPoints


{-| Solve the highest-priority magnetic snap candidate satisfying `isValid`.
-}
solveSnapWithPredicateWithPriority :
    SnapPriority
    -> (SnapCandidate -> Bool)
    -> Float
    -> Int
    -> Mat4
    -> List ConnectionPoint
    -> List WorldConnectionPoint
    -> Maybe SnapCandidate
solveSnapWithPredicateWithPriority priority isValid maxDist rotationIndex currentMatrix draggedPoints targetPoints =
    findFirst isValid (solveSnapCandidatesWithPriority priority maxDist rotationIndex currentMatrix draggedPoints targetPoints)


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst predicate items =
    case items of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                findFirst predicate rest


{-| Solve the best magnetic snap transformation between dragged connection points
and candidate target points in the scene.
-}
solveSnap :
    Float
    -> Int
    -> Mat4
    -> List ConnectionPoint
    -> List WorldConnectionPoint
    -> Maybe SnapCandidate
solveSnap maxDist rotationIndex currentMatrix draggedPoints targetPoints =
    solveSnapWithPredicate (\_ -> True) maxDist rotationIndex currentMatrix draggedPoints targetPoints


{-| Solve snap with magnetic hysteresis latching and a validation predicate:
If an active snap candidate is provided and satisfies `isValid`, the connection remains latched
as long as the dragged connection point stays within `releaseDist` and its orientation remains valid.
If a candidate within `enterDist` engages more connector sites, it replaces the active candidate even
inside the release radius. Otherwise, when distance exceeds `releaseDist`, orientation is incompatible,
`isValid` fails, or no active snap is present, candidate search evaluates candidates within `enterDist`
and picks the first that satisfies `isValid`.
-}
solveSnapWithHysteresisAndPredicate :
    (SnapCandidate -> Bool)
    -> Float
    -> Float
    -> Maybe SnapCandidate
    -> Int
    -> Mat4
    -> List ConnectionPoint
    -> List WorldConnectionPoint
    -> Maybe SnapCandidate
solveSnapWithHysteresisAndPredicate isValid enterDist releaseDist maybeActive rotationIndex currentMatrix draggedPoints targetPoints =
    solveSnapWithHysteresisAndPredicateWithPriority MaximizeContacts isValid enterDist releaseDist maybeActive rotationIndex currentMatrix draggedPoints targetPoints


{-| Solve with magnetic hysteresis and an explicit candidate ranking strategy.
The active candidate remains latched while valid and within `releaseDist`; when
searching, candidates follow `priority`.
-}
solveSnapWithHysteresisAndPredicateWithPriority :
    SnapPriority
    -> (SnapCandidate -> Bool)
    -> Float
    -> Float
    -> Maybe SnapCandidate
    -> Int
    -> Mat4
    -> List ConnectionPoint
    -> List WorldConnectionPoint
    -> Maybe SnapCandidate
solveSnapWithHysteresisAndPredicateWithPriority priority isValid enterDist releaseDist maybeActive rotationIndex currentMatrix draggedPoints targetPoints =
    case maybeActive of
        Just active ->
            let
                draggedPos =
                    Mat4.transform currentMatrix active.draggedPoint.localPosition

                dist =
                    Vec3.distance draggedPos active.targetPoint.worldPosition
            in
            if dist <= releaseDist then
                let
                    draggedWorldNormal =
                        transformDirection currentMatrix active.draggedPoint.normal

                    desiredNormal =
                        targetDesiredNormal active.draggedPoint.connType draggedWorldNormal active.targetPoint.worldNormal

                    cosAngle =
                        Vec3.dot draggedWorldNormal desiredNormal
                in
                if isDirectional active.draggedPoint.connType && cosAngle < -0.2 then
                    solveSnapWithPredicateWithPriority priority isValid enterDist rotationIndex currentMatrix draggedPoints targetPoints

                else
                    let
                        snapMat =
                            computeSnapMatrix currentMatrix rotationIndex active.draggedPoint active.targetPoint desiredNormal

                        guide =
                            makeGuide active.draggedPoint.connType active.targetPoint.worldPosition active.targetPoint.worldNormal False

                        candidate =
                            { active
                                | snapMatrix = snapMat
                                , distance = dist
                                , guide = guide
                            }
                    in
                    if isValid candidate then
                        let
                            bias =
                                connectionPairBias active.draggedPoint.connType active.targetPoint.connType

                            activeContactCount =
                                alignedContactCount snapMat active.targetPlacementId draggedPoints targetPoints

                            activeContactBonus =
                                case priority of
                                    MaximizeContacts ->
                                        toFloat (activeContactCount - 1) * 2.5

                                    ClosestPoint ->
                                        0.0

                            activeScore =
                                dist + (1.0 - cosAngle) * 5.0 + bias - activeContactBonus

                            maybeAlternative =
                                solveSnapWithPredicateWithPriority priority isValid enterDist rotationIndex currentMatrix draggedPoints targetPoints
                        in
                        case maybeAlternative of
                            Just alternative ->
                                let
                                    altDraggedWorldNormal =
                                        transformDirection currentMatrix alternative.draggedPoint.normal

                                    altDesiredNormal =
                                        targetDesiredNormal alternative.draggedPoint.connType altDraggedWorldNormal alternative.targetPoint.worldNormal

                                    altCosAngle =
                                        Vec3.dot altDraggedWorldNormal altDesiredNormal

                                    altBias =
                                        connectionPairBias alternative.draggedPoint.connType alternative.targetPoint.connType

                                    altContactCount =
                                        alignedContactCount alternative.snapMatrix alternative.targetPlacementId draggedPoints targetPoints

                                    altContactBonus =
                                        case priority of
                                            MaximizeContacts ->
                                                toFloat (altContactCount - 1) * 2.5

                                            ClosestPoint ->
                                                0.0

                                    alternativeScore =
                                        alternative.distance + (1.0 - altCosAngle) * 5.0 + altBias - altContactBonus
                                in
                                if alternativeScore < activeScore - 5.0 then
                                    Just alternative

                                else
                                    Just candidate

                            Nothing ->
                                Just candidate

                    else
                        solveSnapWithPredicateWithPriority priority isValid enterDist rotationIndex currentMatrix draggedPoints targetPoints

            else
                solveSnapWithPredicateWithPriority priority isValid enterDist rotationIndex currentMatrix draggedPoints targetPoints

        Nothing ->
            solveSnapWithPredicateWithPriority priority isValid enterDist rotationIndex currentMatrix draggedPoints targetPoints


{-| Solve snap with magnetic hysteresis latching:
If an active snap candidate is provided, the connection remains latched as long as
the dragged connection point stays within `releaseDist` (default 18.0 LDU) of the target.
If the distance exceeds `releaseDist` or no active snap is present, candidates are
searched within `enterDist` (default 12.0 LDU).
-}
solveSnapWithHysteresis :
    Float
    -> Float
    -> Maybe SnapCandidate
    -> Int
    -> Mat4
    -> List ConnectionPoint
    -> List WorldConnectionPoint
    -> Maybe SnapCandidate
solveSnapWithHysteresis enterDist releaseDist maybeActive rotationIndex currentMatrix draggedPoints targetPoints =
    solveSnapWithHysteresisAndPredicate (\_ -> True) enterDist releaseDist maybeActive rotationIndex currentMatrix draggedPoints targetPoints


isDirectional : ConnectionType -> Bool
isDirectional conn =
    case conn of
        Stud ->
            True

        AntiStud ->
            True

        ClipMale _ ->
            True

        BarFemale _ ->
            True

        BallMale ->
            True

        BallSocketFemale ->
            True

        _ ->
            False


isBidirectionalAxis : ConnectionType -> Bool
isBidirectionalAxis conn =
    case conn of
        TechnicAxleMale _ ->
            True

        TechnicAxleFemale _ ->
            True

        TechnicPinMale _ ->
            True

        TechnicPinFemale _ ->
            True

        WheelRim _ ->
            True

        WheelTire _ ->
            True

        _ ->
            False


targetDesiredNormal : ConnectionType -> Vec3 -> Vec3 -> Vec3
targetDesiredNormal conn draggedWorldNormal targetWorldNormal =
    if isBidirectionalAxis conn then
        if Vec3.dot draggedWorldNormal targetWorldNormal < 0 then
            Vec3.negate targetWorldNormal

        else
            targetWorldNormal

    else
        Vec3.negate targetWorldNormal


rotateShortestArc : Vec3 -> Vec3 -> Vec3 -> Vec3
rotateShortestArc n1 n2 x =
    let
        c =
            Vec3.dot n1 n2
    in
    if c > 0.999999 then
        x

    else if c < -0.999999 then
        -- 180 degree flip around an orthogonal axis
        let
            axis =
                if abs (Vec3.getX n1) < 0.9 then
                    Vec3.normalize (Vec3.cross n1 (vec3 1 0 0))

                else
                    Vec3.normalize (Vec3.cross n1 (vec3 0 1 0))
        in
        Vec3.add (Vec3.scale (2.0 * Vec3.dot axis x) axis) (Vec3.negate x)

    else
        let
            v =
                Vec3.cross n1 n2

            factor =
                1.0 / (1.0 + c)
        in
        Vec3.add x (Vec3.add (Vec3.cross v x) (Vec3.scale factor (Vec3.cross v (Vec3.cross v x))))


computeSnapMatrix : Mat4 -> Int -> ConnectionPoint -> WorldConnectionPoint -> Vec3 -> Mat4
computeSnapMatrix currentMatrix rotationIndex draggedLocal targetWorld desiredNormal =
    let
        basisD =
            orthonormalBasis draggedLocal.normal draggedLocal.tangent

        draggedWorldNormal =
            transformDirection currentMatrix draggedLocal.normal

        draggedWorldTangent =
            transformDirection currentMatrix draggedLocal.tangent

        refTangent =
            rotateShortestArc draggedWorldNormal desiredNormal draggedWorldTangent

        basisT0 =
            orthonormalBasis desiredNormal refTangent

        step =
            modBy 4 (modBy 4 rotationIndex + 4)

        ( cosT, sinT ) =
            case step of
                0 ->
                    ( 1.0, 0.0 )

                1 ->
                    ( 0.0, 1.0 )

                2 ->
                    ( -1.0, 0.0 )

                _ ->
                    ( 0.0, -1.0 )

        uT =
            Vec3.add (Vec3.scale cosT basisT0.tangent) (Vec3.scale sinT basisT0.bitangent)

        vT =
            Vec3.cross desiredNormal uT

        -- Transform column j: R(e_j) = dot(e_j, Ud)*uT + dot(e_j, Vd)*vT + dot(e_j, Nd)*desiredNormal
        uDx =
            Vec3.getX basisD.tangent

        uDy =
            Vec3.getY basisD.tangent

        uDz =
            Vec3.getZ basisD.tangent

        vDx =
            Vec3.getX basisD.bitangent

        vDy =
            Vec3.getY basisD.bitangent

        vDz =
            Vec3.getZ basisD.bitangent

        nDx =
            Vec3.getX basisD.normal

        nDy =
            Vec3.getY basisD.normal

        nDz =
            Vec3.getZ basisD.normal

        col1 =
            Vec3.add (Vec3.scale uDx uT) (Vec3.add (Vec3.scale vDx vT) (Vec3.scale nDx desiredNormal))

        col2 =
            Vec3.add (Vec3.scale uDy uT) (Vec3.add (Vec3.scale vDy vT) (Vec3.scale nDy desiredNormal))

        col3 =
            Vec3.add (Vec3.scale uDz uT) (Vec3.add (Vec3.scale vDz vT) (Vec3.scale nDz desiredNormal))

        -- Rotate dragged local position by R:
        pLocal =
            draggedLocal.localPosition

        pLx =
            Vec3.getX pLocal

        pLy =
            Vec3.getY pLocal

        pLz =
            Vec3.getZ pLocal

        rotP =
            Vec3.add (Vec3.scale pLx col1) (Vec3.add (Vec3.scale pLy col2) (Vec3.scale pLz col3))

        trans =
            Vec3.sub targetWorld.worldPosition rotP
    in
    Mat4.fromRecord
        { m11 = Vec3.getX col1
        , m21 = Vec3.getY col1
        , m31 = Vec3.getZ col1
        , m41 = 0
        , m12 = Vec3.getX col2
        , m22 = Vec3.getY col2
        , m32 = Vec3.getZ col2
        , m42 = 0
        , m13 = Vec3.getX col3
        , m23 = Vec3.getY col3
        , m33 = Vec3.getZ col3
        , m43 = 0
        , m14 = Vec3.getX trans
        , m24 = Vec3.getY trans
        , m34 = Vec3.getZ trans
        , m44 = 1
        }


makeGuide : ConnectionType -> Vec3 -> Vec3 -> Bool -> SnapGuide
makeGuide conn pos normal isColliding =
    case conn of
        Stud ->
            StudRingGuide pos normal isColliding

        AntiStud ->
            StudRingGuide pos normal isColliding

        TechnicAxleMale _ ->
            AxleAxisGuide pos normal isColliding

        TechnicAxleFemale _ ->
            AxleAxisGuide pos normal isColliding

        TechnicPinMale _ ->
            AxleAxisGuide pos normal isColliding

        TechnicPinFemale _ ->
            AxleAxisGuide pos normal isColliding

        WheelRim _ ->
            AxleAxisGuide pos normal isColliding

        WheelTire _ ->
            AxleAxisGuide pos normal isColliding

        _ ->
            ClipLockGuide pos normal isColliding



-- ── Collision Detection (SAT) ────────────────────────────────────────────────


{-| Build an Oriented Bounding Box from a local bounding box and a transform matrix.
-}
toOBB : BoundingBox3D -> Mat4 -> OBB
toOBB box matrix =
    let
        centerLocal =
            Vec3.scale 0.5 (Vec3.add box.min box.max)

        halfLocal =
            Vec3.scale 0.5 (Vec3.sub box.max box.min)

        worldCenter =
            Mat4.transform matrix centerLocal

        axisX =
            transformDirection matrix (vec3 1 0 0)

        axisY =
            transformDirection matrix (vec3 0 1 0)

        axisZ =
            transformDirection matrix (vec3 0 0 1)

        scaleX =
            Vec3.length (Vec3.sub (Mat4.transform matrix (vec3 1 0 0)) (Mat4.transform matrix (vec3 0 0 0)))

        scaleY =
            Vec3.length (Vec3.sub (Mat4.transform matrix (vec3 0 1 0)) (Mat4.transform matrix (vec3 0 0 0)))

        scaleZ =
            Vec3.length (Vec3.sub (Mat4.transform matrix (vec3 0 0 1)) (Mat4.transform matrix (vec3 0 0 0)))

        halfExtents =
            vec3
                (abs (Vec3.getX halfLocal) * scaleX)
                (abs (Vec3.getY halfLocal) * scaleY)
                (abs (Vec3.getZ halfLocal) * scaleZ)
    in
    { center = worldCenter
    , halfExtents = halfExtents
    , axisX = axisX
    , axisY = axisY
    , axisZ = axisZ
    }


{-| Effective collision envelope that excludes external stud protrusions (height = 4 LDU),
preventing false collisions when studs enter compatible cavities or side-stud attachments.
-}
effectiveEnvelope : BoundingBox3D -> List ConnectionPoint -> BoundingBox3D
effectiveEnvelope box connections =
    let
        studs =
            List.filterMap
                (\c ->
                    case c.connType of
                        Stud ->
                            Just ( c.localPosition, c.normal )

                        _ ->
                            Nothing
                )
                connections

        ( newMinX, newMaxX ) =
            trimDimension Vec3.getX (Vec3.getX box.min) (Vec3.getX box.max) studs

        ( newMinY, newMaxY ) =
            trimDimension Vec3.getY (Vec3.getY box.min) (Vec3.getY box.max) studs

        ( newMinZ, newMaxZ ) =
            trimDimension Vec3.getZ (Vec3.getZ box.min) (Vec3.getZ box.max) studs
    in
    { min = vec3 newMinX newMinY newMinZ
    , max = vec3 newMaxX newMaxY newMaxZ
    }


trimDimension : (Vec3 -> Float) -> Float -> Float -> List ( Vec3, Vec3 ) -> ( Float, Float )
trimDimension getCoord minVal maxVal studs =
    let
        trimMin studBase faceVal =
            if faceVal < studBase - 0.1 && (studBase - faceVal) <= 4.5 then
                studBase

            else
                faceVal

        trimMax studBase faceVal =
            if faceVal > studBase + 0.1 && (faceVal - studBase) <= 4.5 then
                studBase

            else
                faceVal

        minStuds =
            studs
                |> List.filterMap
                    (\( p, n ) ->
                        if getCoord n < -0.7 then
                            Just (getCoord p)

                        else
                            Nothing
                    )

        maxStuds =
            studs
                |> List.filterMap
                    (\( p, n ) ->
                        if getCoord n > 0.7 then
                            Just (getCoord p)

                        else
                            Nothing
                    )

        newMin =
            case minStuds of
                [] ->
                    minVal

                bases ->
                    trimMin (List.foldl min 1.0e9 bases) minVal

        newMax =
            case maxStuds of
                [] ->
                    maxVal

                bases ->
                    trimMax (List.foldl max -1.0e9 bases) maxVal
    in
    ( newMin, newMax )


{-| 3D Separating Axis Theorem (SAT) test between two OBBs with a skin tolerance.
Returns True when the two boxes intersect by more than `tolerance`.
-}
testOBB : Float -> OBB -> OBB -> Bool
testOBB tolerance a b =
    let
        d =
            Vec3.sub b.center a.center

        axesA =
            [ a.axisX, a.axisY, a.axisZ ]

        axesB =
            [ b.axisX, b.axisY, b.axisZ ]

        crossAxes =
            List.concatMap
                (\ax ->
                    List.filterMap
                        (\bx ->
                            let
                                c =
                                    Vec3.cross ax bx

                                len =
                                    Vec3.length c
                            in
                            if len < 1.0e-5 then
                                Nothing

                            else
                                Just (Vec3.scale (1 / len) c)
                        )
                        axesB
                )
                axesA

        allAxes =
            axesA ++ axesB ++ crossAxes

        overlapsAlong axis =
            let
                rA =
                    abs (Vec3.dot a.axisX axis)
                        * Vec3.getX a.halfExtents
                        + abs (Vec3.dot a.axisY axis)
                        * Vec3.getY a.halfExtents
                        + abs (Vec3.dot a.axisZ axis)
                        * Vec3.getZ a.halfExtents

                rB =
                    abs (Vec3.dot b.axisX axis)
                        * Vec3.getX b.halfExtents
                        + abs (Vec3.dot b.axisY axis)
                        * Vec3.getY b.halfExtents
                        + abs (Vec3.dot b.axisZ axis)
                        * Vec3.getZ b.halfExtents

                dist =
                    abs (Vec3.dot d axis)
            in
            (rA + rB) - dist > tolerance
    in
    List.all overlapsAlong allAxes


{-| Check collisions between dragged placements and static scene placements.
Returns list of colliding placement ID pairs (draggedId, otherId).
-}
checkCollisions :
    Float
    -> List ( Int, Mat4, BoundingBox3D )
    -> List ( Int, Mat4, BoundingBox3D )
    -> List ( Int, Int )
checkCollisions tolerance draggedPlacements scenePlacements =
    let
        draggedOBBs =
            List.map (\( id, mat, box ) -> ( id, toOBB box mat )) draggedPlacements

        sceneOBBs =
            List.map (\( id, mat, box ) -> ( id, toOBB box mat )) scenePlacements
    in
    draggedOBBs
        |> List.concatMap
            (\( dragId, obbA ) ->
                sceneOBBs
                    |> List.filterMap
                        (\( sceneId, obbB ) ->
                            if dragId /= sceneId && testOBB tolerance obbA obbB then
                                Just ( dragId, sceneId )

                            else
                                Nothing
                        )
            )


{-| Check whether applying a snap candidate causes the dragged placements to collide with scene placements.
The lead part snapping to a target placement excludes `snap.targetPlacementId` so it does not collide
with the target placement it wraps around or connects to, while still checking collisions against all
other scene placements and checking follower parts against all scene placements.
-}
isSnapColliding :
    Float
    -> Int
    -> Mat4
    -> List ( Int, Mat4, BoundingBox3D )
    -> List ( Int, Mat4, BoundingBox3D )
    -> SnapCandidate
    -> Bool
isSnapColliding tolerance leadId leadOrigMatrix draggedOrigPlacements scenePlacements snap =
    let
        snapDeltaMat =
            Mat4.mul snap.snapMatrix (invertRigid leadOrigMatrix)

        draggedBoxes =
            draggedOrigPlacements
                |> List.map (\( id, origMat, box ) -> ( id, Mat4.mul snapDeltaMat origMat, box ))

        ( leadBoxes, otherBoxes ) =
            List.partition (\( id, _, _ ) -> id == leadId) draggedBoxes

        activeSceneBoxes =
            List.filter (\( id, _, _ ) -> id /= snap.targetPlacementId) scenePlacements

        collidingPairs =
            checkCollisions tolerance leadBoxes activeSceneBoxes
                ++ checkCollisions tolerance otherBoxes scenePlacements
    in
    not (List.isEmpty collidingPairs)



-- ── Connection Extraction ────────────────────────────────────────────────────


{-| Extract connection points directly from parsed LDraw lines and subfile references.
-}
extractConnections : List LDrawLine -> List ConnectionPoint
extractConnections lines =
    let
        extractSubfile idx line acc =
            case line of
                SubFileRef sub ->
                    case classifySubfile sub of
                        Just ( connType, norm, tang ) ->
                            let
                                pt =
                                    { id = idx
                                    , connType = connType
                                    , localPosition = Mat4.transform sub.transform (primitiveAnchor connType)
                                    , normal = transformDirection sub.transform norm
                                    , tangent = transformDirection sub.transform tang
                                    }
                            in
                            pt :: acc

                        Nothing ->
                            acc

                _ ->
                    acc
    in
    List.indexedMap (\i l -> ( i, l )) lines
        |> List.foldl (\( i, l ) acc -> extractSubfile i l acc) []
        |> List.reverse


classifySubfile : SubFileData -> Maybe ( ConnectionType, Vec3, Vec3 )
classifySubfile sub =
    let
        file =
            String.toLower sub.file
    in
    if isStudFile file then
        Just ( Stud, vec3 0 -1 0, vec3 1 0 0 )

    else if isAntiStudFile file then
        -- The tube primitives run from their base (Y = 0, against the part's
        -- ceiling) to their open end at Y = -4: the opening faces local -Y
        -- (see primitiveAnchor). Parts flip them with a negative Y scale, so the
        -- opening ends up facing down out of the part's underside.
        Just ( AntiStud, vec3 0 -1 0, vec3 1 0 0 )

    else if isAxleHoleFile file then
        Just ( TechnicAxleFemale { length = 20.0 }, vec3 0 0 1, vec3 1 0 0 )

    else if isAxleMaleFile file then
        Just ( TechnicAxleMale { length = 20.0 }, vec3 0 0 1, vec3 1 0 0 )

    else if isPinHoleFile file then
        Just ( TechnicPinFemale { length = 20.0 }, vec3 0 0 1, vec3 1 0 0 )

    else if isFrictionPinMaleFile file then
        Just ( TechnicPinMale { length = 20.0, friction = True }, vec3 0 0 1, vec3 1 0 0 )

    else if isFrictionlessPinMaleFile file then
        Just ( TechnicPinMale { length = 20.0, friction = False }, vec3 0 0 1, vec3 1 0 0 )

    else if String.contains "clip" file then
        Just ( ClipMale { width = 4.0 }, vec3 0 0 1, vec3 1 0 0 )

    else if String.contains "bar" file then
        Just ( BarFemale { diameter = 4.0 }, vec3 0 1 0, vec3 1 0 0 )

    else if String.contains "joint8ball" file || String.contains "towball" file then
        Just ( BallMale, vec3 0 -1 0, vec3 1 0 0 )

    else if String.contains "joint8socket" file then
        Just ( BallSocketFemale, vec3 0 1 0, vec3 1 0 0 )

    else
        Nothing


{-| Where a connection primitive's connection point sits in its own space. A stud
is anchored at its base (the origin). An anti-stud tube (stud3, stud4, …) is
anchored at its open end, local Y = -4, where a stud enters it: in a part that
is the underside face (a plate's Y = 8, a brick's Y = 24), as the catalog has it.
-}
primitiveAnchor : ConnectionType -> Vec3
primitiveAnchor connType =
    case connType of
        AntiStud ->
            vec3 0 -4 0

        _ ->
            vec3 0 0 0


isStudFile : String -> Bool
isStudFile f =
    f
        == "stud.dat"
        || f
        == "stud2.dat"
        || f
        == "stud2a.dat"
        || f
        == "stud10.dat"
        || f
        == "stud15.dat"
        || f
        == "studp01.dat"
        || String.startsWith "stud-logo" f


isAntiStudFile : String -> Bool
isAntiStudFile f =
    String.startsWith "stud4" f
        || String.startsWith "stud3" f
        || f
        == "tube.dat"
        || f
        == "studtube.dat"
        || f
        == "studtube2.dat"


isAxleHoleFile : String -> Bool
isAxleHoleFile f =
    String.startsWith "axlehole" f
        || String.startsWith "axlehol" f
        || String.startsWith "axl2ho" f
        || String.startsWith "axl3ho" f
        || String.startsWith "axl4ho" f
        || String.startsWith "axl5ho" f


isAxleMaleFile : String -> Bool
isAxleMaleFile f =
    f
        == "axle.dat"
        || String.startsWith "axleend" f
        || f
        == "axles.dat"


isPinHoleFile : String -> Bool
isPinHoleFile f =
    String.contains "pinhole" f
        || f
        == "peghole.dat"
        || f
        == "beamhole.dat"
        || f
        == "connhole.dat"
        || String.startsWith "wpinhol" f


isFrictionPinMaleFile : String -> Bool
isFrictionPinMaleFile f =
    String.contains "confric" f


isFrictionlessPinMaleFile : String -> Bool
isFrictionlessPinMaleFile f =
    f
        == "connect.dat"
        || f
        == "connect1.dat"
        || f
        == "connect2.dat"
        || f
        == "connect8.dat"
        || f
        == "pin.dat"
        || String.startsWith "wpin" f


isPinMaleFile : String -> Bool
isPinMaleFile f =
    isFrictionPinMaleFile f || isFrictionlessPinMaleFile f



-- ── Standard Catalog & Recursive Extraction ──────────────────────────────────


{-| Get connectivity for standard Lego parts by name or recursive line expansion.
-}
extractPartConnectivity : String -> (String -> Maybe (List LDrawLine)) -> PartConnectivity
extractPartConnectivity partName lineResolver =
    case knownCatalogPart partName of
        Just pc ->
            pc

        Nothing ->
            let
                lines =
                    lineResolver partName |> Maybe.withDefault []

                direct =
                    extractConnections lines

                expanded =
                    if List.isEmpty direct then
                        extractRecursive lineResolver 0 Mat4.identity lines []

                    else
                        direct

                box =
                    inferBounds expanded
            in
            { part = partName
            , connections = expanded
            , boundingBoxes = [ box ]
            }


extractRecursive : (String -> Maybe (List LDrawLine)) -> Int -> Mat4 -> List LDrawLine -> List ConnectionPoint -> List ConnectionPoint
extractRecursive lineResolver depth currentMat lines acc =
    if depth > 4 then
        acc

    else
        List.foldl
            (\line currentAcc ->
                case line of
                    SubFileRef sub ->
                        let
                            comb =
                                Mat4.mul currentMat sub.transform
                        in
                        case classifySubfile sub of
                            Just ( connType, norm, tang ) ->
                                { id = List.length currentAcc + 1
                                , connType = connType
                                , localPosition = Mat4.transform comb (primitiveAnchor connType)
                                , normal = transformDirection comb norm
                                , tangent = transformDirection comb tang
                                }
                                    :: currentAcc

                            Nothing ->
                                case knownCatalogPart (String.toLower sub.file) of
                                    Just catalogPart ->
                                        List.foldl
                                            (\point points ->
                                                { point
                                                    | id = List.length points + 1
                                                    , localPosition = Mat4.transform comb point.localPosition
                                                    , normal = transformDirection comb point.normal
                                                    , tangent = transformDirection comb point.tangent
                                                }
                                                    :: points
                                            )
                                            currentAcc
                                            catalogPart.connections

                                    Nothing ->
                                        case lineResolver (String.toLower sub.file) of
                                            Just subLines ->
                                                extractRecursive lineResolver (depth + 1) comb subLines currentAcc

                                            Nothing ->
                                                currentAcc

                    _ ->
                        currentAcc
            )
            acc
            lines


inferBounds : List ConnectionPoint -> BoundingBox3D
inferBounds points =
    case points of
        [] ->
            { min = vec3 -10 -4 -10, max = vec3 10 24 10 }

        first :: rest ->
            let
                -- How far a point's connector sticks out of the body along its normal.
                protrusion pt =
                    if pt.connType == Stud then
                        4

                    else
                        0

                -- How far the body reaches behind a vertical point: to the nearest
                -- opposite face (a plate's anti-studs 8 LDU below its studs), else
                -- a brick's 24 LDU.
                bodyDepth pt =
                    points
                        |> List.filterMap
                            (\other ->
                                let
                                    behind =
                                        Vec3.dot (Vec3.sub other.localPosition pt.localPosition) (Vec3.negate pt.normal)
                                in
                                if Vec3.dot other.normal pt.normal < -0.7 && behind > 0.5 then
                                    Just behind

                                else
                                    Nothing
                            )
                        |> List.minimum
                        |> Maybe.withDefault 24

                pointExtents pt =
                    let
                        p =
                            pt.localPosition

                        n =
                            pt.normal

                        padX =
                            if abs (Vec3.getX n) > 0.7 then
                                if Vec3.getX n < 0 then
                                    ( Vec3.getX p - 4, Vec3.getX p )

                                else
                                    ( Vec3.getX p, Vec3.getX p + 4 )

                            else
                                ( Vec3.getX p - 10, Vec3.getX p + 10 )

                        padY =
                            if abs (Vec3.getY n) > 0.7 then
                                if Vec3.getY n < 0 then
                                    ( Vec3.getY p - protrusion pt, Vec3.getY p + bodyDepth pt )

                                else
                                    ( Vec3.getY p - bodyDepth pt, Vec3.getY p + protrusion pt )

                            else
                                ( Vec3.getY p - 10, Vec3.getY p + 10 )

                        padZ =
                            if abs (Vec3.getZ n) > 0.7 then
                                if Vec3.getZ n < 0 then
                                    ( Vec3.getZ p - 4, Vec3.getZ p )

                                else
                                    ( Vec3.getZ p, Vec3.getZ p + 4 )

                            else
                                ( Vec3.getZ p - 10, Vec3.getZ p + 10 )
                    in
                    { minX = Tuple.first padX
                    , maxX = Tuple.second padX
                    , minY = Tuple.first padY
                    , maxY = Tuple.second padY
                    , minZ = Tuple.first padZ
                    , maxZ = Tuple.second padZ
                    }

                combine e1 e2 =
                    { minX = min e1.minX e2.minX
                    , maxX = max e1.maxX e2.maxX
                    , minY = min e1.minY e2.minY
                    , maxY = max e1.maxY e2.maxY
                    , minZ = min e1.minZ e2.minZ
                    , maxZ = max e1.maxZ e2.maxZ
                    }

                extents =
                    List.foldl (\pt acc -> combine (pointExtents pt) acc) (pointExtents first) rest
            in
            { min = vec3 extents.minX extents.minY extents.minZ
            , max = vec3 extents.maxX extents.maxY extents.maxZ
            }


{-| Whether a part has any stud (top stud) connection points. Studded parts
sit on the stud lattice; Technic parts (gears, beams, axles, pins, bushes) have
none.
-}
isStudded : PartConnectivity -> Bool
isStudded conn =
    List.any isStudPoint conn.connections


{-| Whether a part is upright: true when it has at least one up-facing stud
(normal Y < −0.7 in LDraw) or down-facing anti-stud (normal Y > 0.7) after `matrix`.
Used by build health and restud to skip stud-lattice rules for tipped parts.
-}
isUpright : PartConnectivity -> Mat4 -> Bool
isUpright conn matrix =
    List.any
        (\point ->
            let
                worldNormY =
                    Vec3.getY (transformDirection matrix point.normal)
            in
            case point.connType of
                Stud ->
                    worldNormY < -0.7

                AntiStud ->
                    worldNormY > 0.7

                _ ->
                    False
        )
        conn.connections


isStudPoint : ConnectionPoint -> Bool
isStudPoint point =
    case point.connType of
        Stud ->
            True

        _ ->
            False


{-| The X and Z offset a studded part's origin needs to sit on the stud lattice,
where studs are at 10 mod 20 (LDU) on both axes:

  - `10` on an axis along which the part, turned by `matrix`, has an odd number
    of studs (a 1×1 or a 1×3 must sit with its origin at ±10, a 2×4 at 0 mod 20);
  - `0` otherwise, and always for a part without stud points (Technic parts keep
    their own 20 LDU pitch, see `isStudded`).

Only the rotation of `matrix` counts: its translation is ignored. A part's stud
columns are the distinct rotated stud X (or Z) positions, so a 2 × 1 turned 90°
has its offset swapped.

    studOffset (extractPartConnectivity "3005.dat" (\_ -> Nothing)) Mat4.identity
        == { x = 10, z = 10 }

-}
studOffset : PartConnectivity -> Mat4 -> { x : Float, z : Float }
studOffset conn matrix =
    let
        origin =
            Mat4.transform matrix (vec3 0 0 0)

        antiStuds =
            conn.connections
                |> List.filter
                    (\point ->
                        point.connType == AntiStud && Vec3.getY (transformDirection matrix point.normal) > 0.7
                    )
                |> List.map (\point -> Vec3.sub (Mat4.transform matrix point.localPosition) origin)

        studs =
            conn.connections
                |> List.filter
                    (\point ->
                        point.connType == Stud && Vec3.getY (transformDirection matrix point.normal) < -0.7
                    )
                |> List.map (\point -> Vec3.sub (Mat4.transform matrix point.localPosition) origin)

        relevantPoints =
            if not (List.isEmpty antiStuds) then
                antiStuds

            else
                studs

        offsetAlong get =
            if List.isEmpty relevantPoints then
                0

            else
                let
                    hasOddMultiple =
                        relevantPoints
                            |> List.any (\v -> modBy 20 (abs (round (get v))) == 10)
                in
                if hasOddMultiple then
                    0

                else
                    10
    in
    { x = offsetAlong Vec3.getX, z = offsetAlong Vec3.getZ }


{-| Pre-defined connection layouts for common standard Lego parts.
-}
knownCatalogPart : String -> Maybe PartConnectivity
knownCatalogPart name =
    let
        clean =
            String.toLower (String.replace ".dat" "" (String.replace "parts/" "" name))
    in
    case clean of
        "60592" ->
            -- Window 1 x 2 x 2 without sill: 2 x 1 studs on top, 2 bricks (48 LDU) tall,
            -- with the underside anti-studs that seat it on the studs below
            Just (makeBrick 2 1 48 "60592.dat")

        "3001" ->
            -- Brick 2 x 4
            Just (makeBrick 4 2 24 "3001.dat")

        "3002" ->
            -- Brick 2 x 3
            Just (makeBrick 3 2 24 "3002.dat")

        "3003" ->
            -- Brick 2 x 2
            Just (makeBrick 2 2 24 "3003.dat")

        "3003b" ->
            Just (makeBrick 2 2 24 "3003b.dat")

        "3004" ->
            -- Brick 1 x 2
            Just (makeBrick 2 1 24 "3004.dat")

        "3004b" ->
            Just (makeBrick 2 1 24 "3004b.dat")

        "3005" ->
            -- Brick 1 x 1
            Just (makeBrick 1 1 24 "3005.dat")

        "3005b" ->
            Just (makeBrick 1 1 24 "3005b.dat")

        "3006" ->
            -- Brick 2 x 5
            Just (makeBrick 5 2 24 "3006.dat")

        "3007" ->
            -- Brick 2 x 8
            Just (makeBrick 8 2 24 "3007.dat")

        "3008" ->
            -- Brick 1 x 8
            Just (makeBrick 8 1 24 "3008.dat")

        "3009" ->
            -- Brick 1 x 6
            Just (makeBrick 6 1 24 "3009.dat")

        "3010" ->
            -- Brick 1 x 4
            Just (makeBrick 4 1 24 "3010.dat")

        "3622" ->
            -- Brick 1 x 3
            Just (makeBrick 3 1 24 "3622.dat")

        "3020" ->
            -- Plate 2 x 4
            Just (makeBrick 4 2 8 "3020.dat")

        "3021" ->
            -- Plate 2 x 3
            Just (makeBrick 3 2 8 "3021.dat")

        "3022" ->
            -- Plate 2 x 2
            Just (makeBrick 2 2 8 "3022.dat")

        "3023" ->
            -- Plate 1 x 2
            Just (makeBrick 2 1 8 "3023.dat")

        "3023b" ->
            Just (makeBrick 2 1 8 "3023b.dat")

        "3024" ->
            -- Plate 1 x 1
            Just (makeBrick 1 1 8 "3024.dat")

        "3034" ->
            -- Plate 2 x 8
            Just (makeBrick 8 2 8 "3034.dat")

        "3460" ->
            -- Plate 1 x 8
            Just (makeBrick 8 1 8 "3460.dat")

        "3666" ->
            -- Plate 1 x 6
            Just (makeBrick 6 1 8 "3666.dat")

        "3710" ->
            -- Plate 1 x 4
            Just (makeBrick 4 1 8 "3710.dat")

        "3623" ->
            -- Plate 1 x 3
            Just (makeBrick 3 1 8 "3623.dat")

        "3795" ->
            -- Plate 2 x 6
            Just (makeBrick 6 2 8 "3795.dat")

        "3794" ->
            Just (makeJumperPlate1x2 "3794.dat")

        "3794a" ->
            Just (makeJumperPlate1x2 "3794a.dat")

        "3794b" ->
            Just (makeJumperPlate1x2 "3794b.dat")

        "15573" ->
            Just (makeJumperPlate1x2 "15573.dat")

        "87580" ->
            Just (makeJumperPlate2x2 "87580.dat")

        "3031" ->
            -- Plate 4 x 4
            Just (makeBrick 4 4 8 "3031.dat")

        "3958" ->
            -- Plate 6 x 6
            Just (makeBrick 6 6 8 "3958.dat")

        "41539" ->
            -- Plate 8 x 8
            Just (makeBrick 8 8 8 "41539.dat")

        "4073" ->
            -- Plate 1 x 1 Round
            Just (makeBrick 1 1 8 "4073.dat")

        "6141" ->
            -- Plate 1 x 1 Round
            Just (makeBrick 1 1 8 "6141.dat")

        "85861" ->
            -- Plate 1 x 1 Round with open stud
            Just (makeBrick 1 1 8 "85861.dat")

        "3062" ->
            -- Brick 1 x 1 Round
            Just (makeBrick 1 1 24 "3062.dat")

        "3062a" ->
            -- Brick 1 x 1 Round
            Just (makeBrick 1 1 24 "3062a.dat")

        "3062b" ->
            -- Brick 1 x 1 Round
            Just (makeBrick 1 1 24 "3062b.dat")

        "3700" ->
            Just (makeTechnicBrick 2 "3700.dat")

        "3701" ->
            Just (makeTechnicBrick 4 "3701.dat")

        "3702" ->
            Just (makeTechnicBrick 8 "3702.dat")

        "3703" ->
            Just (makeTechnicBrick 16 "3703.dat")

        "3894" ->
            Just (makeTechnicBrick 6 "3894.dat")

        "6541" ->
            Just
                { part = "6541.dat"
                , connections =
                    [ studAt 1 (vec3 0 0 0)
                    , antiStudAt 100 (vec3 0 24 0)
                    , { id = 200
                      , connType = TechnicPinFemale { length = 20.0 }
                      , localPosition = vec3 0 10 0
                      , normal = vec3 0 0 1
                      , tangent = vec3 1 0 0
                      }
                    , { id = 201
                      , connType = TechnicPinFemale { length = 20.0 }
                      , localPosition = vec3 0 10 10
                      , normal = vec3 0 0 1
                      , tangent = vec3 1 0 0
                      }
                    , { id = 202
                      , connType = TechnicPinFemale { length = 20.0 }
                      , localPosition = vec3 0 10 -10
                      , normal = vec3 0 0 -1
                      , tangent = vec3 1 0 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -10 -4 -10, max = vec3 10 24 10 } ]
                }

        "3709b" ->
            let
                base =
                    makeBrick 4 2 8 "3709b.dat"

                pinHoles =
                    [ -20.0, 0.0, 20.0 ]
                        |> List.indexedMap
                            (\i x ->
                                let
                                    baseId =
                                        200 + i * 3
                                in
                                [ { id = baseId
                                  , connType = TechnicPinFemale { length = 8.0 }
                                  , localPosition = vec3 x 4 0
                                  , normal = vec3 0 1 0
                                  , tangent = vec3 1 0 0
                                  }
                                , { id = baseId + 1
                                  , connType = TechnicPinFemale { length = 8.0 }
                                  , localPosition = vec3 x 0 0
                                  , normal = vec3 0 -1 0
                                  , tangent = vec3 1 0 0
                                  }
                                , { id = baseId + 2
                                  , connType = TechnicPinFemale { length = 8.0 }
                                  , localPosition = vec3 x 8 0
                                  , normal = vec3 0 1 0
                                  , tangent = vec3 1 0 0
                                  }
                                ]
                            )
                        |> List.concat
            in
            Just { base | connections = base.connections ++ pinHoles }

        "32523" ->
            Just (makeLiftarm 3 "32523.dat")

        "32316" ->
            Just (makeLiftarm 5 "32316.dat")

        "32524" ->
            Just (makeLiftarm 7 "32524.dat")

        "32449" ->
            Just (makeLiftarm 9 "32449.dat")

        "40490" ->
            Just (makeLiftarm 9 "40490.dat")

        "32525" ->
            Just (makeLiftarm 11 "32525.dat")

        "32009" ->
            Just (makeLiftarm 15 "32009.dat")

        "32278" ->
            Just (makeLiftarm 15 "32278.dat")

        "3647" ->
            Just (makeGear 10.0 20.0 "3647.dat")

        "10928" ->
            Just (makeGear 10.0 20.0 "10928.dat")

        "11955" ->
            Just (makeGear 10.0 20.0 "11955.dat")

        "4019" ->
            Just (makeGear 20.0 20.0 "4019.dat")

        "69778" ->
            Just (makeGear 15.0 20.0 "69778.dat")

        "94925" ->
            Just (makeGear 20.0 20.0 "94925.dat")

        "32498" ->
            Just (makeGear 45.0 20.0 "32498.dat")

        "32072" ->
            Just (makeGear 20.0 20.0 "32072.dat")

        "3648" ->
            let
                base =
                    makeGear 30.0 20.0 "3648.dat"

                pinHoles =
                    makeGearPinHoles 10
                        [ ( 10, 10 )
                        , ( -10, 10 )
                        , ( -10, -10 )
                        , ( 10, -10 )
                        ]
            in
            Just { base | connections = base.connections ++ pinHoles }

        "3648b" ->
            let
                base =
                    makeGear 30.0 20.0 "3648b.dat"

                pinHoles =
                    makeGearPinHoles 10
                        [ ( 10, 10 )
                        , ( -10, 10 )
                        , ( -10, -10 )
                        , ( 10, -10 )
                        ]
            in
            Just { base | connections = base.connections ++ pinHoles }

        "3649" ->
            let
                base =
                    makeGear 50.0 20.0 "3649.dat"

                extraHoles =
                    [ ( 0, 20 )
                    , ( 0, -20 )
                    , ( 20, 0 )
                    , ( -20, 0 )
                    ]
                        |> List.indexedMap
                            (\i ( x, y ) ->
                                makeAxleHolePoints (10 + i * 3) 10.0 (vec3 x y 0)
                            )
                        |> List.concat

                pinHoles =
                    makeGearPinHoles 30
                        [ ( 10, 10 )
                        , ( -10, 10 )
                        , ( -10, -10 )
                        , ( 10, -10 )
                        , ( 10, 30 )
                        , ( -10, 30 )
                        , ( 10, -30 )
                        , ( -10, -30 )
                        , ( 30, 10 )
                        , ( -30, 10 )
                        , ( 30, -10 )
                        , ( -30, -10 )
                        ]
            in
            Just { base | connections = base.connections ++ extraHoles ++ pinHoles }

        "32198" ->
            Just (makeGear 25.0 20.0 "32198.dat")

        "32198a" ->
            Just (makeGear 25.0 20.0 "32198a.dat")

        "32269" ->
            Just (makeGear 25.0 20.0 "32269.dat")

        "6589" ->
            -- Body z -3..7 with the teeth at z 0..7 (they face +z).
            Just (makeGearSpan 15.0 { zMin = -3.0, zMax = 7.0 } { zMin = 0.0, zMax = 7.0 } "6589.dat")

        "32270" ->
            Just (makeGear 15.0 20.0 "32270.dat")

        "4143" ->
            -- Body z -4..3 with the teeth at z -4..0 (they face -z).
            Just (makeGearSpan 17.5 { zMin = -4.0, zMax = 3.0 } { zMin = -4.0, zMax = 0.0 } "4143.dat")

        "3650b" ->
            Just (makeGear 30.0 20.0 "3650b.dat")

        "3650c" ->
            Just (makeGear 30.0 20.0 "3650c.dat")

        "3650" ->
            Just (makeGear 30.0 20.0 "3650.dat")

        "18938" ->
            let
                holes =
                    [ 40.0, -40.0 ]
                        |> List.indexedMap
                            (\i x ->
                                let
                                    baseId =
                                        1 + i * 3
                                in
                                [ { id = baseId
                                  , connType = TechnicPinFemale { length = 20.0 }
                                  , localPosition = vec3 x -10 0
                                  , normal = vec3 0 -1 0
                                  , tangent = vec3 0 0 1
                                  }
                                , { id = baseId + 1
                                  , connType = TechnicPinFemale { length = 20.0 }
                                  , localPosition = vec3 x -20 0
                                  , normal = vec3 0 -1 0
                                  , tangent = vec3 0 0 1
                                  }
                                , { id = baseId + 2
                                  , connType = TechnicPinFemale { length = 20.0 }
                                  , localPosition = vec3 x 0 0
                                  , normal = vec3 0 1 0
                                  , tangent = vec3 0 0 1
                                  }
                                ]
                            )
                        |> List.concat
            in
            Just
                { part = "18938.dat"
                , connections = holes
                , boundingBoxes = [ { min = vec3 -77 -20 -77, max = vec3 77 0 77 } ]
                }

        "4716" ->
            Just (makeGear 10.0 40.0 "4716.dat")

        "32905" ->
            Just (makeGear 10.0 40.0 "32905.dat")

        "3704" ->
            Just (makeAxle 2 "3704.dat")

        "4519" ->
            Just (makeAxle 3 "4519.dat")

        "3705" ->
            Just (makeAxle 4 "3705.dat")

        "32073" ->
            Just (makeAxle 5 "32073.dat")

        "3706" ->
            Just (makeAxle 6 "3706.dat")

        "44294" ->
            Just (makeAxle 7 "44294.dat")

        "3707" ->
            Just (makeAxle 8 "3707.dat")

        "60485" ->
            Just (makeAxle 9 "60485.dat")

        "3737" ->
            Just (makeAxle 10 "3737.dat")

        "3708" ->
            Just (makeAxle 12 "3708.dat")

        "32062" ->
            Just (makeAxle 2 "32062.dat")

        "6587" ->
            -- Technic Axle 3 with Stud
            Just
                { part = "6587.dat"
                , connections =
                    [ { id = 1
                      , connType = TechnicAxleMale { length = 60.0 }
                      , localPosition = vec3 -20 0 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 2
                      , connType = TechnicAxleMale { length = 60.0 }
                      , localPosition = vec3 0 0 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 3
                      , connType = TechnicAxleMale { length = 60.0 }
                      , localPosition = vec3 20 0 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 4
                      , connType = Stud
                      , localPosition = vec3 30 0 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -30 -4 -4, max = vec3 34 4 4 } ]
                }

        "3713" ->
            Just (makeGear 9.0 20.0 "3713.dat")

        "6590" ->
            Just (makeGear 9.0 20.0 "6590.dat")

        "4265c" ->
            Just (makeGear 9.0 10.0 "4265c.dat")

        "32123" ->
            Just (makeGear 9.0 10.0 "4265c.dat")

        "32123a" ->
            Just (makeGear 9.0 10.0 "4265c.dat")

        "2780" ->
            -- Technic Pin with Friction
            Just (makeTechnicPin 10.0 True "2780.dat")

        "3673" ->
            -- Technic Pin without Friction (2L)
            Just (makeTechnicPin 10.0 False "3673.dat")

        "6558" ->
            -- Technic Pin 3L with Friction Ridges
            Just (makeTechnicPin 15.0 True "6558.dat")

        "32556" ->
            -- Technic Pin 3L without Friction
            Just (makeTechnicPin 15.0 False "32556.dat")

        "32556a" ->
            Just (makeTechnicPin 15.0 False "32556a.dat")

        "43093" ->
            -- Technic Axle 1L with Pin with Friction
            Just
                { part = "43093.dat"
                , connections =
                    [ { id = 1
                      , connType = TechnicPinMale { length = 20.0, friction = True }
                      , localPosition = vec3 -10 0 0
                      , normal = vec3 -1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 2
                      , connType = TechnicAxleMale { length = 20.0 }
                      , localPosition = vec3 10 0 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -20 -8 -8, max = vec3 20 8 8 } ]
                }

        "3749" ->
            -- Technic Axle 1L with Pin without Friction
            Just
                { part = "3749.dat"
                , connections =
                    [ { id = 1
                      , connType = TechnicPinMale { length = 20.0, friction = False }
                      , localPosition = vec3 -10 0 0
                      , normal = vec3 -1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 2
                      , connType = TechnicAxleMale { length = 20.0 }
                      , localPosition = vec3 10 0 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -20 -8 -8, max = vec3 20 8 8 } ]
                }

        "11214" ->
            -- Technic Axle Pin Long with Friction with 2L Pin
            Just
                { part = "11214.dat"
                , connections =
                    [ { id = 1
                      , connType = TechnicPinMale { length = 40.0, friction = True }
                      , localPosition = vec3 -20 0 0
                      , normal = vec3 -1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 2
                      , connType = TechnicAxleMale { length = 20.0 }
                      , localPosition = vec3 20 0 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -40 -8 -8, max = vec3 30 8 8 } ]
                }

        "4274" ->
            -- Technic Pin 1/2 with Stud
            Just
                { part = "4274.dat"
                , connections =
                    [ { id = 1
                      , connType = TechnicPinMale { length = 10.0, friction = True }
                      , localPosition = vec3 -5 0 0
                      , normal = vec3 -1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 2
                      , connType = Stud
                      , localPosition = vec3 0 0 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -10 -8 -8, max = vec3 4 8 8 } ]
                }

        "89678" ->
            -- Technic Pin 1/2 with Friction
            Just
                { part = "89678.dat"
                , connections =
                    [ { id = 1
                      , connType = TechnicPinMale { length = 10.0, friction = True }
                      , localPosition = vec3 -5 0 0
                      , normal = vec3 -1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 2
                      , connType = Stud
                      , localPosition = vec3 0 0 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -10 -8 -8, max = vec3 4 8 8 } ]
                }

        "2711" ->
            -- Technic Rotor 2 Blade with 2 Studs
            Just
                { part = "2711.dat"
                , connections =
                    [ studAt 1 (vec3 20 0 0)
                    , studAt 2 (vec3 -20 0 0)
                    , antiStudAt 100 (vec3 20 8 0)
                    , antiStudAt 101 (vec3 -20 8 0)
                    , { id = 200
                      , connType = TechnicAxleFemale { length = 8.0 }
                      , localPosition = vec3 0 4 0
                      , normal = vec3 0 1 0
                      , tangent = vec3 1 0 0
                      }
                    , { id = 201
                      , connType = TechnicAxleFemale { length = 8.0 }
                      , localPosition = vec3 0 0 0
                      , normal = vec3 0 -1 0
                      , tangent = vec3 1 0 0
                      }
                    , { id = 202
                      , connType = TechnicAxleFemale { length = 8.0 }
                      , localPosition = vec3 0 8 0
                      , normal = vec3 0 1 0
                      , tangent = vec3 1 0 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -50 -4 -10, max = vec3 50 8 10 } ]
                }

        "32125" ->
            -- Technic Rotor 3 Blade Small
            Just
                { part = "32125.dat"
                , connections =
                    [ studAt 1 (vec3 0 0 30)
                    , studAt 2 (vec3 0 0 40)
                    , studAt 3 (vec3 25.98 0 -15)
                    , studAt 4 (vec3 34.64 0 -20)
                    , studAt 5 (vec3 -25.98 0 -15)
                    , studAt 6 (vec3 -34.64 0 -20)
                    , antiStudAt 100 (vec3 0 8 30)
                    , antiStudAt 101 (vec3 0 8 40)
                    , antiStudAt 102 (vec3 25.98 8 -15)
                    , antiStudAt 103 (vec3 34.64 8 -20)
                    , antiStudAt 104 (vec3 -25.98 8 -15)
                    , antiStudAt 105 (vec3 -34.64 8 -20)
                    , { id = 200
                      , connType = TechnicAxleFemale { length = 8.0 }
                      , localPosition = vec3 0 4 0
                      , normal = vec3 0 1 0
                      , tangent = vec3 1 0 0
                      }
                    , { id = 201
                      , connType = TechnicAxleFemale { length = 8.0 }
                      , localPosition = vec3 0 0 0
                      , normal = vec3 0 -1 0
                      , tangent = vec3 1 0 0
                      }
                    , { id = 202
                      , connType = TechnicAxleFemale { length = 8.0 }
                      , localPosition = vec3 0 8 0
                      , normal = vec3 0 1 0
                      , tangent = vec3 1 0 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -50 -4 -50, max = vec3 50 8 50 } ]
                }

        "99009" ->
            -- Technic Turntable 28 Tooth Bottom
            let
                pinHoles =
                    makeGearPinHoles 1
                        [ ( 20, 20 )
                        , ( -20, 20 )
                        ]
            in
            Just
                { part = "99009.dat"
                , connections = pinHoles
                , boundingBoxes =
                    [ { min = vec3 -35 -4 -35, max = vec3 35 24 35 } ]
                }

        "99010" ->
            -- Technic Turntable 28 Tooth Top
            let
                pinHoles =
                    makeGearPinHoles 1
                        [ ( 20, -20 )
                        , ( -20, -20 )
                        ]
            in
            Just
                { part = "99010.dat"
                , connections = pinHoles
                , boundingBoxes =
                    [ { min = vec3 -35 -24 -35, max = vec3 35 4 35 } ]
                }

        "4085" ->
            Just (makeClipPlate "4085.dat")

        "4085c" ->
            Just (makeClipPlate "4085c.dat")

        "30374" ->
            -- Bar 4L
            Just (makeBar 4 "30374.dat")

        "87087" ->
            -- Brick 1 x 1 with Stud on 1 Side (SNOT): the side stud faces -Z
            Just (makeBrick 1 1 24 "87087.dat" |> withSideStuds [ ( vec3 0 10 -10, vec3 0 0 -1 ) ])

        "4070" ->
            -- Brick 1 x 1 with Headlight: the side stud sits recessed, its base at Z = -6,
            -- and the rear face has an anti-stud cavity at Z = 10
            let
                base =
                    makeBrick 1 1 24 "4070.dat"
                        |> withSideStuds [ ( vec3 0 10 -6, vec3 0 0 -1 ) ]

                rearAntiStud =
                    { id = 101
                    , connType = AntiStud
                    , localPosition = vec3 0 10 10
                    , normal = vec3 0 0 1
                    , tangent = vec3 0 1 0
                    }
            in
            Just { base | connections = rearAntiStud :: base.connections }

        "4733" ->
            -- Brick 1 x 1 with Studs on 4 Sides (Travis brick)
            Just
                (makeBrick 1 1 24 "4733.dat"
                    |> withSideStuds
                        [ ( vec3 0 10 -10, vec3 0 0 -1 )
                        , ( vec3 10 10 0, vec3 1 0 0 )
                        , ( vec3 0 10 10, vec3 0 0 1 )
                        , ( vec3 -10 10 0, vec3 -1 0 0 )
                        ]
                )

        "26604" ->
            -- Brick 1 x 1 with Studs on Two Adjacent Sides
            Just
                (makeBrick 1 1 24 "26604.dat"
                    |> withSideStuds [ ( vec3 0 10 -10, vec3 0 0 -1 ), ( vec3 10 10 0, vec3 1 0 0 ) ]
                )

        "11211" ->
            -- Brick 1 x 2 with Two Studs on One Side
            Just
                (makeBrick 2 1 24 "11211.dat"
                    |> withSideStuds [ ( vec3 -10 10 -10, vec3 0 0 -1 ), ( vec3 10 10 -10, vec3 0 0 -1 ) ]
                )

        "30414" ->
            -- Brick 1 x 4 with Studs on Side
            Just
                (makeBrick 4 1 24 "30414.dat"
                    |> withSideStuds (List.map (\x -> ( vec3 x 10 -10, vec3 0 0 -1 )) [ -30, -10, 10, 30 ])
                )

        "32952" ->
            -- Brick 1 x 1 x 1 2/3 with Studs on 1 Side: two side studs, one above the other
            Just
                (makeBrick 1 1 40 "32952.dat"
                    |> withSideStuds [ ( vec3 0 10 -10, vec3 0 0 -1 ), ( vec3 0 30 -10, vec3 0 0 -1 ) ]
                )

        "22885" ->
            -- Brick 1 x 2 x 1 2/3 with Studs on 1 Side: a 2 x 2 grid of side studs
            Just
                (makeBrick 2 1 40 "22885.dat"
                    |> withSideStuds
                        (List.concatMap (\x -> [ ( vec3 x 10 -10, vec3 0 0 -1 ), ( vec3 x 30 -10, vec3 0 0 -1 ) ]) [ -10, 10 ])
                )

        "36840" ->
            -- Bracket 1 x 1 - 1 x 1 Up: 1 x 1 plate with upward vertical plate
            Just
                (makeBracket 1
                    "36840.dat"
                    { minY = -12, maxY = 8 }
                    [ ( vec3 0 -2 -14, vec3 0 0 -1 ) ]
                )

        "36841" ->
            -- Bracket 1 x 1 - 1 x 1 Down: 1 x 1 plate with downward vertical plate
            Just
                (makeBracket 1
                    "36841.dat"
                    { minY = -4, maxY = 20 }
                    [ ( vec3 0 10 -14, vec3 0 0 -1 ) ]
                )

        "99781" ->
            -- Bracket 1 x 2 - 1 x 2 Down: a 1 x 2 plate with a vertical plate hanging
            -- below its front edge (Z -14 to -10), whose studs face -Z
            Just
                (makeBracket 2
                    "99781.dat"
                    { minY = -4, maxY = 20 }
                    [ ( vec3 -10 10 -14, vec3 0 0 -1 ), ( vec3 10 10 -14, vec3 0 0 -1 ) ]
                )

        "99780" ->
            -- Bracket 1 x 2 - 1 x 2 Up: the vertical plate rises above the front edge
            Just
                (makeBracket 2
                    "99780.dat"
                    { minY = -12, maxY = 8 }
                    [ ( vec3 -10 -2 -14, vec3 0 0 -1 ), ( vec3 10 -2 -14, vec3 0 0 -1 ) ]
                )

        "99207" ->
            -- Bracket 1 x 2 - 2 x 2 Up: a taller vertical plate with two rows of studs
            Just
                (makeBracket 2
                    "99207.dat"
                    { minY = -32, maxY = 8 }
                    (List.concatMap (\y -> [ ( vec3 -10 y -14, vec3 0 0 -1 ), ( vec3 10 y -14, vec3 0 0 -1 ) ]) [ -2, -22 ])
                )

        "44728" ->
            -- Bracket 1 x 2 - 2 x 2 Down: 1 x 2 plate with taller downward vertical plate
            Just
                (makeBracket 2
                    "44728.dat"
                    { minY = -4, maxY = 40 }
                    (List.concatMap (\y -> [ ( vec3 -10 y -14, vec3 0 0 -1 ), ( vec3 10 y -14, vec3 0 0 -1 ) ]) [ 10, 30 ])
                )

        "99206" ->
            -- Plate 2 x 2 x 2/3 with Two Studs On Side and Two Raised: the raised
            -- front row at Y = 0, the rear row at Y = 8, the side studs facing -Z, and
            -- a 2 x 2 bottom (Z -10 to 30) 16 LDU down
            Just
                { part = "99206.dat"
                , connections =
                    [ studAt 1 (vec3 -10 0 0)
                    , studAt 2 (vec3 10 0 0)
                    , studAt 3 (vec3 -10 8 20)
                    , studAt 4 (vec3 10 8 20)
                    ]
                        ++ List.indexedMap (\i ( x, z ) -> antiStudAt (100 + i) (vec3 x 16 z))
                            [ ( -10, 0 ), ( 10, 0 ), ( -10, 20 ), ( 10, 20 ) ]
                        ++ sideStudPoints [ ( vec3 -10 6 -10, vec3 0 0 -1 ), ( vec3 10 6 -10, vec3 0 0 -1 ) ]
                , boundingBoxes = [ { min = vec3 -20 -4 -10, max = vec3 20 16 30 } ]
                }

        "3070" ->
            Just (makeTile 1 1 "3070.dat")

        "3070a" ->
            Just (makeTile 1 1 "3070a.dat")

        "3070b" ->
            -- Tile 1 x 1 with Groove
            Just (makeTile 1 1 "3070b.dat")

        "98138" ->
            -- Tile 1 x 1 Round
            Just (makeTile 1 1 "98138.dat")

        "25269" ->
            -- Tile 1 x 1 Round Quarter
            Just (makeTile 1 1 "25269.dat")

        "24246" ->
            -- Tile 1 x 1 with Rounded End
            Just (makeTile 1 1 "24246.dat")

        "39739" ->
            -- Tile 1 x 1 Heart
            Just (makeTile 1 1 "39739.dat")

        "3069" ->
            Just (makeTile 2 1 "3069.dat")

        "3069a" ->
            Just (makeTile 2 1 "3069a.dat")

        "3069b" ->
            -- Tile 1 x 2 with Groove
            Just (makeTile 2 1 "3069b.dat")

        "2412" ->
            Just (makeTile 2 1 "2412.dat")

        "2412a" ->
            Just (makeTile 2 1 "2412a.dat")

        "2412b" ->
            -- Tile 1 x 2 Grille with Groove
            Just (makeTile 2 1 "2412b.dat")

        "3068" ->
            Just (makeTile 2 2 "3068.dat")

        "3068a" ->
            Just (makeTile 2 2 "3068a.dat")

        "3068b" ->
            -- Tile 2 x 2 with Groove
            Just (makeTile 2 2 "3068b.dat")

        "2431" ->
            -- Tile 1 x 4 with Groove
            Just (makeTile 4 1 "2431.dat")

        "6636" ->
            -- Tile 1 x 6
            Just (makeTile 6 1 "6636.dat")

        "87079" ->
            -- Tile 2 x 4 with Groove
            Just (makeTile 4 2 "87079.dat")

        "3040" ->
            -- Slope Brick 45 2 x 1
            Just (makeSlope 1 2 24 "3040.dat")

        "3040a" ->
            Just (makeSlope 1 2 24 "3040a.dat")

        "3040b" ->
            Just (makeSlope 1 2 24 "3040b.dat")

        "3039" ->
            -- Slope Brick 45 2 x 2
            Just (makeSlope 2 2 24 "3039.dat")

        "3038" ->
            -- Slope Brick 45 2 x 3
            Just (makeSlope 3 2 24 "3038.dat")

        "3037" ->
            -- Slope Brick 45 2 x 4
            Just (makeSlope 4 2 24 "3037.dat")

        "3298" ->
            -- Slope Brick 33 3 x 2
            Just (makeSlope 2 3 24 "3298.dat")

        "54200" ->
            -- Slope Brick 31 1 x 1 x 2/3 (cheese slope)
            Just
                { part = "54200.dat"
                , connections = [ antiStudAt 100 (vec3 0 0 0) ]
                , boundingBoxes = [ { min = vec3 -10 -16 -10, max = vec3 10 0 10 } ]
                }

        "14417" ->
            -- Plate 1 x 2 with Ball
            let
                base =
                    makeBrick 2 1 8 "14417.dat"

                ball =
                    { id = 50
                    , connType = BallMale
                    , localPosition = vec3 0 4 -20
                    , normal = vec3 0 0 -1
                    , tangent = vec3 1 0 0
                    }
            in
            Just { base | connections = ball :: base.connections }

        "14418" ->
            -- Plate 1 x 2 with Ball Socket
            let
                base =
                    makeBrick 2 1 8 "14418.dat"

                socket =
                    { id = 50
                    , connType = BallSocketFemale
                    , localPosition = vec3 0 4 -20
                    , normal = vec3 0 0 -1
                    , tangent = vec3 1 0 0
                    }
            in
            Just { base | connections = socket :: base.connections }

        "4624" ->
            Just (makeWheelRim False 10.0 16.0 16.0 "4624.dat")

        "3641" ->
            Just (makeTire 8.0 18.0 16.0 16.0 "3641.dat")

        "6014b" ->
            Just (makeWheelRim False 13.5 24.0 27.0 (clean ++ ".dat"))

        "6014" ->
            Just (makeWheelRim False 13.5 24.0 27.0 (clean ++ ".dat"))

        "6015" ->
            Just (makeTire 12.5 24.8 28.0 27.0 "6015.dat")

        "56890" ->
            Just (makeTire 12.5 30.0 28.0 27.0 "56890.dat")

        "42610" ->
            Just (makeWheelRim False 14.0 20.0 27.0 "42610.dat")

        "50951" ->
            Just (makeTire 12.0 19.0 16.0 27.0 (clean ++ ".dat"))

        "50945" ->
            Just (makeTire 12.0 19.0 16.0 27.0 (clean ++ ".dat"))

        "55982" ->
            Just (makeWheelRim True 21.0 34.0 42.0 "55982.dat")

        "55981" ->
            Just (makeWheelRim False 21.0 34.0 42.0 "55981.dat")

        "56891" ->
            Just (makeTire 20.0 37.0 35.0 42.0 "56891.dat")

        "56145" ->
            let
                baseRim =
                    makeWheelRim True 38.0 44.0 75.0 "56145.dat"
            in
            Just
                { baseRim
                    | connections =
                        baseRim.connections
                            ++ makeGearPinHoles 200
                                [ ( 0, 20 )
                                , ( 0, -20 )
                                , ( 17.32, 10 )
                                , ( -17.32, 10 )
                                , ( 17.32, -10 )
                                , ( -17.32, -10 )
                                ]
                }

        "44309" ->
            Just (makeTire 34.0 54.0 56.0 75.0 "44309.dat")

        "4600" ->
            let
                base =
                    makeBrick 2 2 8 "4600.dat"

                wheelPins =
                    [ { id = 50
                      , connType = TechnicPinMale { length = 12.0, friction = False }
                      , localPosition = vec3 28 5 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 51
                      , connType = TechnicPinMale { length = 12.0, friction = False }
                      , localPosition = vec3 -28 5 0
                      , normal = vec3 -1 0 0
                      , tangent = vec3 0 1 0
                      }
                    ]
            in
            Just
                { part = "4600.dat"
                , connections = base.connections ++ wheelPins
                , boundingBoxes = [ { min = vec3 -20 -4 -20, max = vec3 20 8 20 } ]
                }

        "4488" ->
            Just
                { part = "4488.dat"
                , connections =
                    [ studAt 1 (vec3 -10 0 -10)
                    , studAt 2 (vec3 10 0 -10)
                    , studAt 3 (vec3 -10 0 10)
                    , studAt 4 (vec3 10 0 10)
                    , antiStudAt 10 (vec3 -10 8 -10)
                    , antiStudAt 11 (vec3 -10 8 10)
                    , antiStudAt 12 (vec3 10 16 -10)
                    , antiStudAt 13 (vec3 10 16 10)
                    , { id = 50
                      , connType = TechnicPinMale { length = 12.0, friction = False }
                      , localPosition = vec3 28 5 0
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    ]
                , boundingBoxes = [ { min = vec3 -20 -4 -20, max = vec3 20 16 20 } ]
                }

        "24326" ->
            Just
                { part = "24326.dat"
                , connections =
                    [ studAt 1 (vec3 -10 0 -30)
                    , studAt 2 (vec3 10 0 -30)
                    , studAt 3 (vec3 -10 0 30)
                    , studAt 4 (vec3 10 0 30)
                    , studAt 5 (vec3 -10 8 -10)
                    , studAt 6 (vec3 10 8 -10)
                    , studAt 7 (vec3 -10 8 10)
                    , studAt 8 (vec3 10 8 10)
                    , antiStudAt 10 (vec3 -10 16 -30)
                    , antiStudAt 11 (vec3 10 16 -30)
                    , antiStudAt 12 (vec3 -10 16 30)
                    , antiStudAt 13 (vec3 10 16 30)
                    , antiStudAt 14 (vec3 -10 16 -10)
                    , antiStudAt 15 (vec3 10 16 -10)
                    , antiStudAt 16 (vec3 -10 16 10)
                    , antiStudAt 17 (vec3 10 16 10)
                    , { id = 50
                      , connType = TechnicPinMale { length = 12.0, friction = False }
                      , localPosition = vec3 38 5 20
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 51
                      , connType = TechnicPinMale { length = 12.0, friction = False }
                      , localPosition = vec3 38 5 -20
                      , normal = vec3 1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 52
                      , connType = TechnicPinMale { length = 12.0, friction = False }
                      , localPosition = vec3 -38 5 20
                      , normal = vec3 -1 0 0
                      , tangent = vec3 0 1 0
                      }
                    , { id = 53
                      , connType = TechnicPinMale { length = 12.0, friction = False }
                      , localPosition = vec3 -38 5 -20
                      , normal = vec3 -1 0 0
                      , tangent = vec3 0 1 0
                      }
                    ]
                , boundingBoxes = [ { min = vec3 -30 -4 -40, max = vec3 30 16 40 } ]
                }

        "1846" ->
            Just
                { part = "1846.dat"
                , connections =
                    [ studAt 1 (vec3 -10 0 0)
                    , studAt 2 (vec3 10 0 0)
                    , studAt 3 (vec3 -30 24 0)
                    , studAt 4 (vec3 30 24 0)
                    , antiStudAt 10 (vec3 -10 32 0)
                    , antiStudAt 11 (vec3 0 32 0)
                    , antiStudAt 12 (vec3 10 32 0)
                    , antiStudAt 13 (vec3 -30 40 0)
                    , antiStudAt 14 (vec3 30 40 0)
                    , antiStudAt 15 (vec3 -30 40 -40)
                    , antiStudAt 16 (vec3 30 40 -40)
                    ]
                , boundingBoxes = [ { min = vec3 -40 -4 -10, max = vec3 40 32 10 } ]
                }

        "50745" ->
            Just
                { part = "50745.dat"
                , connections =
                    [ studAt 1 (vec3 -30 0 10)
                    , studAt 2 (vec3 -10 0 10)
                    , studAt 3 (vec3 10 0 10)
                    , studAt 4 (vec3 30 0 10)
                    , antiStudAt 10 (vec3 -30 24 10)
                    , antiStudAt 11 (vec3 -10 24 10)
                    , antiStudAt 12 (vec3 10 24 10)
                    , antiStudAt 13 (vec3 30 24 10)
                    ]
                , boundingBoxes = [ { min = vec3 -40 -4 0, max = vec3 40 24 20 } ]
                }

        "2352" ->
            Just (makeSlope 4 2 72 "2352.dat")

        "3823" ->
            Just
                { part = "3823.dat"
                , connections =
                    [ studAt 1 (vec3 -30 0 0)
                    , studAt 2 (vec3 30 0 0)
                    , antiStudAt 10 (vec3 -30 48 0)
                    , antiStudAt 11 (vec3 30 48 0)
                    , antiStudAt 12 (vec3 -30 48 -20)
                    , antiStudAt 13 (vec3 -10 48 -20)
                    , antiStudAt 14 (vec3 10 48 -20)
                    , antiStudAt 15 (vec3 30 48 -20)
                    ]
                , boundingBoxes = [ { min = vec3 -40 -4 -30, max = vec3 40 48 10 } ]
                }

        "3829c01" ->
            Just
                { part = "3829c01.dat"
                , connections =
                    [ antiStudAt 1 (vec3 -10 8 0)
                    , antiStudAt 2 (vec3 10 8 0)
                    , antiStudAt 3 (vec3 0 8 0)
                    ]
                , boundingBoxes = [ { min = vec3 -20 -38 -10, max = vec3 20 8 10 } ]
                }

        "3829" ->
            Just
                { part = "3829c01.dat"
                , connections =
                    [ antiStudAt 1 (vec3 -10 8 0)
                    , antiStudAt 2 (vec3 10 8 0)
                    , antiStudAt 3 (vec3 0 8 0)
                    ]
                , boundingBoxes = [ { min = vec3 -20 -38 -10, max = vec3 20 8 10 } ]
                }

        -- ── Microscale, slopes and inverted slopes ─────────────────────
        -- Measured from the LDraw part files (kulits tree). New entries keep a
        -- single envelope box first (it sets drag collisions and where a freshly
        -- added part rests); only 15070's tooth and 4022's beam add a second box.
        "3941" ->
            -- Brick 2 x 2 round (the axle hole is left out on purpose: a Technic
            -- point would turn off Health's stud rules)
            Just (makeBrick 2 2 24 "3941.dat")

        "4032b" ->
            -- Plate 2 x 2 round (axle hole left out, as for 3941)
            Just (makeBrick 2 2 8 "4032b.dat")

        "3811" ->
            -- Baseplate 32 x 32: 1024 studs, no anti-studs
            Just baseplate32x32

        "3867" ->
            -- Baseplate 16 x 16: 256 studs, no anti-studs
            Just baseplate16x16

        "63864" ->
            -- Tile 1 x 3
            Just (makeTile 3 1 "63864.dat")

        "60481a" ->
            -- Slope 65 2 x 1 x 2
            Just (makeSlope 1 2 48 "60481a.dat")

        "35464" ->
            -- Slope 45 1 x 1 double: origin at the bottom (like 54200), no studs
            Just (makePart "35464.dat" [] [ vec3 0 0 0 ] [ boxOf -10 -16 -10 10 0 10 ])

        "85984" ->
            -- Slope 31 1 x 2 x 2/3 (cheese slope): origin at the bottom
            Just (makePart "85984.dat" [] [ vec3 -10 0 0, vec3 10 0 0 ] [ boxOf -20 -16 -10 20 0 10 ])

        "11477" ->
            -- Slope curved 2 x 1: origin at the bottom; only the bottom-level
            -- anti-stud is modelled (the notch at (0, −8, 10) is left out)
            Just (makePart "11477.dat" [] [ vec3 0 0 -10 ] [ boxOf -10 -16 -20 10 0 20 ])

        "15068" ->
            -- Slope curved 2 x 2 x 2/3: origin at the bottom, notch at Y −8 left out
            Just (makePart "15068.dat" [] [ vec3 -10 0 -10, vec3 10 0 -10 ] [ boxOf -20 -16 -20 20 0 20 ])

        "93273" ->
            -- Slope curved 4 x 1 double: origin at the bottom, the ends sit on studs,
            -- the middle notch at Y −8 is left out
            Just (makePart "93273.dat" [] [ vec3 0 0 -30, vec3 0 0 30 ] [ boxOf -10 -16 -40 10 0 40 ])

        "50950" ->
            -- Slope curved 3 x 1: origin at the top, no studs; notch at (0, 16, 20) left out
            Just (makePart "50950.dat" [] [ vec3 0 24 -20, vec3 0 24 0 ] [ boxOf -10 0 -30 10 24 30 ])

        "61678" ->
            -- Slope curved 4 x 1: origin at the top, no studs; notches at Y 16 / 8 left out
            Just (makePart "61678.dat" [] [ vec3 0 24 -30, vec3 0 24 -10 ] [ boxOf -10 0 -40 10 24 40 ])

        "3665b" ->
            -- Slope 45 2 x 1 inverted
            Just (makeInvertedSlope 1 2 "3665b.dat")

        "3660b" ->
            -- Slope 45 2 x 2 inverted
            Just (makeInvertedSlope 2 2 "3660b.dat")

        "3747b" ->
            -- Slope 33 3 x 2 inverted
            Just (makeInvertedSlope 2 3 "3747b.dat")

        "24201" ->
            -- Slope curved 2 x 1 inverted: two hollow studs, the front one a plate
            -- lower; the underside has no flat face, so no anti-studs
            Just (makePart "24201.dat" [ vec3 0 0 0, vec3 0 8 20 ] [] [ boxOf -10 -4 -10 10 16 30 ])

        "98283" ->
            -- Brick 1 x 2 with masonry profile
            Just (makeBrick 2 1 24 "98283.dat")

        "2877" ->
            -- Brick 1 x 2 with grille
            Just (makeBrick 2 1 24 "2877.dat")

        "30136" ->
            -- Brick 1 x 2 log
            Just (makeBrick 2 1 24 "30136.dat")

        "4490" ->
            -- Arch 1 x 3: anti-studs only under the legs; the opening (|x| < 10)
            -- is inside the envelope box
            Just
                (makePart "4490.dat"
                    [ vec3 -20 0 0, vec3 0 0 0, vec3 20 0 0 ]
                    [ vec3 -20 24 0, vec3 20 24 0 ]
                    [ boxOf -30 -4 -10 30 24 10 ]
                )

        "3659" ->
            -- Arch 1 x 4: anti-studs only under the legs (opening |x| < 20)
            Just
                (makePart "3659.dat"
                    [ vec3 -30 0 0, vec3 -10 0 0, vec3 10 0 0, vec3 30 0 0 ]
                    [ vec3 -30 24 0, vec3 30 24 0 ]
                    [ boxOf -40 -4 -10 40 24 10 ]
                )

        "87552" ->
            -- Panel 1 x 2 x 2 with side supports (front pocket enclosed in the box)
            Just (makeBrick 2 1 48 "87552.dat")

        "4865b" ->
            -- Panel 1 x 2 x 1: no studs, wall z 6..10 Y 0..16 over a plate base
            Just (makePart "4865b.dat" [] [ vec3 -10 24 0, vec3 10 24 0 ] [ boxOf -20 0 -10 20 24 10 ])

        "3633" ->
            -- Fence 1 x 4 x 1 lattice: no studs, lattice z −10..−4 over a plate base
            Just
                (makePart "3633.dat"
                    []
                    [ vec3 -30 24 0, vec3 -10 24 0, vec3 10 24 0, vec3 30 24 0 ]
                    [ boxOf -40 0 -10 40 24 10 ]
                )

        "4589" ->
            -- Cone 1 x 1 (axle hole not modelled)
            Just (makeBrick 1 1 24 "4589.dat")

        "3942c" ->
            -- Cone 2 x 2 x 2: one centre top stud, no centre anti-stud
            Just
                (makePart "3942c.dat"
                    [ vec3 0 0 0 ]
                    [ vec3 -10 48 -10, vec3 10 48 -10, vec3 -10 48 10, vec3 10 48 10 ]
                    [ boxOf -20 -4 -20 20 48 20 ]
                )

        "4740" ->
            -- Dish 2 x 2 inverted: one centre top stud
            Just
                (makePart "4740.dat"
                    [ vec3 0 0 0 ]
                    [ vec3 -10 8 -10, vec3 10 8 -10, vec3 -10 8 10, vec3 10 8 10 ]
                    [ boxOf -20 -4 -20 20 8 20 ]
                )

        "43888" ->
            -- Brick 1 x 1 x 6 round pillar
            Just (makeBrick 1 1 144 "43888.dat")

        "87994" ->
            -- Bar 3L
            Just (makeBar 3 "87994.dat")

        "3957a" ->
            -- Antenna 4H: one anti-stud at the bottom, mast up to Y −88
            Just (makePart "3957a.dat" [] [ vec3 0 8 0 ] [ boxOf -8 -88 -8 8 8 8 ])

        "61252" ->
            -- Plate 1 x 1 with horizontal clip: the held bar runs along X, centred
            -- (0, 2, −20); the clip ring stays outside the box (like makeClipPlate)
            Just (withConnections [ clipAt 20 (vec3 0 2 -20) (vec3 1 0 0) ] (makeBrick 1 1 8 "61252.dat"))

        "15070" ->
            -- Plate 1 x 1 with tooth: the tooth is a second box, so a fresh ground
            -- add (first box only) sinks it 16 LDU until the mesh is built
            let
                plate =
                    makeBrick 1 1 8 "15070.dat"
            in
            Just { plate | boundingBoxes = plate.boundingBoxes ++ [ boxOf -10 0 -18 10 24 -10 ] }

        "33291" ->
            -- Plate 1 x 1 round with flower petals (petals r12 Y 0..3 left out of the box)
            Just (makeBrick 1 1 8 "33291.dat")

        "32607" ->
            -- Plant plate 1 x 1 with leaves (leaves inside the box)
            Just (makePart "32607.dat" [ vec3 0 0 0 ] [ vec3 0 8 0 ] [ boxOf -10 -4 -26 28 8 10 ])

        "2423" ->
            -- Plant leaves 4 x 3: six leaf studs, a stud tube under the centre and
            -- under the far leaf tip; the 3-LDU leaves are boxed to Y 8
            Just
                (makePart "2423.dat"
                    [ vec3 0 0 0, vec3 -20 0 -20, vec3 20 0 -20, vec3 -20 0 -40, vec3 20 0 -40, vec3 0 0 -60 ]
                    [ vec3 0 8 0, vec3 0 8 -60 ]
                    [ boxOf -26 -4 -68 26 8 10 ]
                )

        "24866" ->
            -- Plate 1 x 1 round with 5 petals
            Just (makeBrick 1 1 8 "24866.dat")

        -- ── Every other visible curated part ───────────────────────────
        -- Plates, tiles and baseplates
        "3032" ->
            Just (makeBrick 6 4 8 "3032.dat")

        "3035" ->
            Just (makeBrick 8 4 8 "3035.dat")

        "3036" ->
            Just (makeBrick 8 6 8 "3036.dat")

        "3030" ->
            Just (makeBrick 10 4 8 "3030.dat")

        "3029" ->
            Just (makeBrick 12 4 8 "3029.dat")

        "3028" ->
            Just (makeBrick 12 6 8 "3028.dat")

        "3027" ->
            Just (makeBrick 16 6 8 "3027.dat")

        "4477" ->
            Just (makeBrick 10 1 8 "4477.dat")

        "3023bp00" ->
            Just (makeBrick 2 1 8 "3023bp00.dat")

        "4162" ->
            Just (makeTile 8 1 "4162.dat")

        "26603" ->
            Just (makeTile 3 2 "26603.dat")

        "14769" ->
            Just (makeTile 2 2 "14769.dat")

        "3069bp64" ->
            Just (makeTile 2 1 "3069bp64.dat")

        "14719" ->
            -- Tile 2 x 2 corner: two boxes for the L shape
            Just
                (makePart "14719.dat"
                    []
                    [ vec3 0 8 0, vec3 20 8 0, vec3 0 8 20 ]
                    [ boxOf -10 0 -10 30 8 10, boxOf -10 0 10 10 8 30 ]
                )

        "18674" ->
            -- Plate 2 x 2 round with a centre stud
            Just
                (makePart "18674.dat"
                    [ vec3 0 0 0 ]
                    [ vec3 -10 8 -10, vec3 10 8 -10, vec3 -10 8 10, vec3 10 8 10 ]
                    [ boxOf -20 -4 -20 20 8 20 ]
                )

        -- Bricks, slopes, arches, round parts and Technic basics
        "3001b" ->
            Just (makeBrick 4 2 24 "3001b.dat")

        "3245c" ->
            Just (makeBrick 2 1 48 "3245c.dat")

        "60593" ->
            Just (makeBrick 2 1 72 "60593.dat")

        "59900" ->
            -- Cone 1 x 1 with top groove (axle holes not modelled)
            Just (makeBrick 1 1 24 "59900.dat")

        "4286" ->
            Just (makeSlope 1 3 24 "4286.dat")

        "4460b" ->
            Just (makeSlope 1 2 72 "4460b.dat")

        "3678b" ->
            Just (makeSlope 2 2 48 "3678b.dat")

        "3044b" ->
            -- Slope 45 2 x 1 double: no studs
            Just (makePart "3044b.dat" [] [ vec3 0 24 -10, vec3 0 24 10 ] [ boxOf -10 0 -20 10 24 20 ])

        "3048b" ->
            -- Slope 45 1 x 2 triple: no studs
            Just (makePart "3048b.dat" [] [ vec3 -10 24 0, vec3 10 24 0 ] [ boxOf -20 0 -10 20 24 10 ])

        "3045" ->
            -- Slope 45 2 x 2 double convex: origin at a corner stud, the 2 x 2 centred at (10, −10)
            Just
                (makePart "3045.dat"
                    [ vec3 0 0 0 ]
                    cornerSlopeAntiStuds
                    [ boxOf -10 -4 -30 30 24 10 ]
                )

        "3046" ->
            -- Slope 45 2 x 2 double concave: three studs, origin at a corner stud
            Just
                (makePart "3046.dat"
                    [ vec3 0 0 0, vec3 20 0 0, vec3 0 0 -20 ]
                    cornerSlopeAntiStuds
                    [ boxOf -10 -4 -30 30 24 10 ]
                )

        "30413" ->
            -- Panel 1 x 4 x 1: no studs
            Just
                (makePart "30413.dat"
                    []
                    [ vec3 -30 24 0, vec3 -10 24 0, vec3 10 24 0, vec3 30 24 0 ]
                    [ boxOf -40 0 -10 40 24 10 ]
                )

        "3455" ->
            -- Arch 1 x 6: anti-studs only under the legs
            Just
                (makePart "3455.dat"
                    (List.map (\x -> vec3 x 0 0) (centeredPitch 6))
                    [ vec3 -50 24 0, vec3 50 24 0 ]
                    [ boxOf -60 -4 -10 60 24 10 ]
                )

        "6182" ->
            -- Arch 1 x 4 x 2: anti-studs only under the legs
            Just
                (makePart "6182.dat"
                    (List.map (\x -> vec3 x 0 0) (centeredPitch 4))
                    [ vec3 -30 48 0, vec3 30 48 0 ]
                    [ boxOf -40 -4 -10 40 48 10 ]
                )

        "2489" ->
            -- Barrel 2 x 2 x 1.67: open top, anti-studs underneath
            Just
                (makePart "2489.dat"
                    []
                    [ vec3 -10 40 -10, vec3 10 40 -10, vec3 -10 40 10, vec3 10 40 10 ]
                    [ boxOf -20 -4 -20 20 40 20 ]
                )

        "35442" ->
            -- Plate gear 6 tooth 2 x 2
            Just
                (makePart "35442.dat"
                    grid2x2Studs
                    (grid2x2At 8)
                    [ boxOf -33.4 -4 -37.5 33.4 8 37.5 ]
                )

        "51739" ->
            -- Wing 2 x 4
            Just
                (makePart "51739.dat"
                    grid2x2Studs
                    (grid2x2At 8)
                    [ boxOf -38.5 -4 -20 38.5 8 20 ]
                )

        "61485" ->
            -- Turntable flat base 4 x 4: 16 anti-studs on the 4 x 4 grid
            Just
                (makePart "61485.dat"
                    []
                    (List.concatMap (\x -> List.map (\z -> vec3 x 8 z) (centeredPitch 4)) (centeredPitch 4))
                    [ boxOf -40 -4 -40 40 8 40 ]
                )

        "3680" ->
            -- Turntable 2 x 2 base
            Just (makePart "3680.dat" [] (grid2x2At 8) [ boxOf -20 0 -20 20 8 20 ])

        "4459" ->
            Just (makeTechnicPin 10 True "4459.dat")

        "87083" ->
            Just (makeAxle 4 "87083.dat")

        "55013" ->
            Just (makeAxle 8 "55013.dat")

        -- Clips (the held bar's centre, normal along the bar; clip rings stay outside the boxes)
        "4081b" ->
            Just (withConnections [ clipAt 20 (vec3 0 2 -20) (vec3 1 0 0) ] (makeBrick 1 1 8 "4081b.dat"))

        "63868" ->
            Just (withConnections [ clipAt 20 (vec3 30 2 0) (vec3 0 0 1) ] (makeBrick 2 1 8 "63868.dat"))

        "2555" ->
            Just (withConnections [ clipAt 20 (vec3 0 -6 0) (vec3 0 0 1) ] (makeTile 1 1 "2555.dat"))

        "15712" ->
            Just (withConnections [ clipAt 20 (vec3 0 -6 0) (vec3 0 0 1) ] (makeTile 1 1 "15712.dat"))

        "2335" ->
            -- Flag 2 x 2: two clips on a vertical bar along Y
            Just
                (withConnections
                    [ clipAt 20 (vec3 0 4 0) (vec3 0 1 0), clipAt 21 (vec3 0 36 0) (vec3 0 1 0) ]
                    (makePart "2335.dat" [] [] [ boxOf -2 0 10 2 40 50 ])
                )

        "30258" ->
            -- Road sign 2 x 2 with clip (clip point measured approximate, ±0.5 LDU)
            Just
                (withConnections
                    [ clipAt 20 (vec3 0 0 0) (vec3 0 1 0) ]
                    (makePart "30258.dat" [] [] [ boxOf -20 -20 -9 20 20 -6 ])
                )

        -- Inferred or simplified layouts (to verify in Studio)
        "6005" ->
            -- Arch 1 x 3 x 2 with curved top: stud on the low end, the anti-stud
            -- under its ledge at Y 16 is a guess
            Just
                (makePart "6005.dat"
                    [ vec3 0 8 0 ]
                    [ vec3 0 48 -40, vec3 0 16 0 ]
                    [ boxOf -10 0 -50 10 48 -10, boxOf -10 4 -10 10 16 10 ]
                )

        "2654a" ->
            -- Dish 2 x 2 with rim: a domed underside with one centre recess (inferred anti-stud)
            Just (makePart "2654a.dat" grid2x2Studs [ vec3 0 8 0 ] [ boxOf -20 -4 -20 20 8 20 ])

        "32028" ->
            -- Plate 1 x 2 with door rail: the rail cannot snap a door
            let
                plate =
                    makeBrick 2 1 8 "32028.dat"
            in
            Just { plate | boundingBoxes = [ boxOf -20 -4 -19 20 8 10 ] }

        "92013" ->
            -- Brick 2 x 2 with ball socket: brick body only, socket omitted
            Just (makeBrick 2 2 24 "92013.dat")

        "4022" ->
            -- Train buffer beam: deck box first (its max Y carries the anti-studs),
            -- beam second; a fresh ground add sinks the beam until the mesh is built
            Just
                (makePart "4022.dat"
                    []
                    (grid2x2At 8)
                    [ boxOf -61 0 -53 61 8 20, boxOf -65 8 -54 65 40 -20 ]
                )

        "4315" ->
            -- Hinge plate 1 x 4 with car roof holder: plate only, holder omitted
            Just (makeBrick 4 1 8 "4315.dat")

        "44570" ->
            -- Hinge plate 3 x 4 locking (fingers omitted; the z +10 anti-stud row is inferred)
            let
                positions =
                    List.concatMap (\x -> List.map (\z -> ( x, z )) [ 10, -10, -30 ]) (centeredPitch 4)
            in
            Just
                (makePart "44570.dat"
                    (List.map (\( x, z ) -> vec3 x 0 z) positions)
                    (List.map (\( x, z ) -> vec3 x 8 z) positions)
                    [ boxOf -40 -4 -40 40 8 20 ]
                )

        "32209" ->
            -- Axle 5.5 with stop
            Just
                { part = "32209.dat"
                , connections =
                    List.indexedMap
                        (\idx x -> technicAt (idx + 1) (TechnicAxleMale { length = 107.5 }) (vec3 x 0 0) (vec3 1 0 0))
                        [ -45, -25, -5, 15, 35 ]
                , boundingBoxes = [ boxOf -55 -8 -8 52.5 8 8 ]
                }

        "32235" ->
            -- Axle flexible 19 (straight)
            Just (makeAxle 19 "32235.dat")

        "32054" ->
            -- Pin long with stop bush
            Just (makeTechnicPin 15 True "32054.dat")

        "15100" ->
            -- Pin with pin hole
            Just
                { part = "15100.dat"
                , connections =
                    [ technicAt 1 (TechnicPinMale { length = 20, friction = True }) (vec3 20 0 0) (vec3 1 0 0)
                    , technicAt 2 (TechnicPinFemale { length = 20 }) (vec3 0 0 0) (vec3 0 1 0)
                    ]
                , boundingBoxes = [ boxOf -9 -10 -9 30 10 9 ]
                }

        "18654" ->
            -- Technic beam 1 (a single pin hole along Y)
            Just
                { part = "18654.dat"
                , connections = [ technicAt 1 (TechnicPinFemale { length = 20 }) (vec3 0 0 0) (vec3 0 1 0) ]
                , boundingBoxes = [ boxOf -9 -10 -9 9 10 9 ]
                }

        "15459" ->
            -- Technic suspension arm 1 x 5: pin holes along X
            Just
                { part = "15459.dat"
                , connections =
                    List.indexedMap
                        (\idx z -> technicAt (idx + 1) (TechnicPinFemale { length = 20 }) (vec3 0 0 z) (vec3 1 0 0))
                        [ 0, -20, -40, -60 ]
                , boundingBoxes = [ boxOf -10 -9 -90 10 9 9 ]
                }

        "32526" ->
            -- Technic beam 3 x 5 bent 90
            Just
                { part = "32526.dat"
                , connections =
                    List.indexedMap
                        (\idx position -> technicAt (idx + 1) (TechnicPinFemale { length = 20 }) position (vec3 0 1 0))
                        [ vec3 0 0 0, vec3 0 0 20, vec3 0 0 40, vec3 0 0 60, vec3 0 0 80, vec3 20 0 80, vec3 40 0 80 ]
                , boundingBoxes = [ boxOf -9 -10 -9 9 10 89, boxOf -9 -10 71 49 10 89 ]
                }

        "32527" ->
            -- Panel fairing #5: two pin holes along Z
            Just
                { part = "32527.dat"
                , connections = fairingPinHoles
                , boundingBoxes = [ boxOf -36.5 -59 -20 9 -4 79.7 ]
                }

        "32528" ->
            -- Panel fairing #6 (mirror of 32527)
            Just
                { part = "32528.dat"
                , connections = fairingPinHoles
                , boundingBoxes = [ boxOf -9 -59 -20 36.5 -4 79.7 ]
                }

        "4185a" ->
            -- Technic wedge belt wheel: an axle hole along Z
            Just
                { part = "4185a.dat"
                , connections = makeAxleHolePoints 1 5 (vec3 0 0 0)
                , boundingBoxes = [ boxOf -30 -30 -5 30 30 5 ]
                }

        "6628" ->
            -- Pin towball with friction (towball omitted)
            Just
                { part = "6628.dat"
                , connections = [ technicAt 1 (TechnicPinMale { length = 20, friction = True }) (vec3 10 0 0) (vec3 1 0 0) ]
                , boundingBoxes = [ boxOf -18 -8 -8 20 8 8 ]
                }

        "2736" ->
            -- Axle towball (towball omitted)
            Just
                { part = "2736.dat"
                , connections = [ technicAt 1 (TechnicAxleMale { length = 17.5 }) (vec3 8.75 0 0) (vec3 1 0 0) ]
                , boundingBoxes = [ boxOf -20 -8 -8 17.5 8 8 ]
                }

        "53585" ->
            -- Technic ball joint with axle hole along Y
            Just
                { part = "53585.dat"
                , connections =
                    [ technicAt 1 (TechnicAxleFemale { length = 15 }) (vec3 0 0 0) (vec3 0 1 0)
                    , technicAt 2 (TechnicAxleFemale { length = 15 }) (vec3 0 7.5 0) (vec3 0 1 0)
                    , technicAt 3 (TechnicAxleFemale { length = 15 }) (vec3 0 -7.5 0) (vec3 0 -1 0)
                    ]
                , boundingBoxes = [ boxOf -12.8 -9.6 -12.8 12.8 9.6 12.8 ]
                }

        "60176" ->
            -- Constraction connector 3 x 2 with ball socket: axle hole along X (socket omitted)
            Just
                { part = "60176.dat"
                , connections = [ technicAt 1 (TechnicAxleFemale { length = 40 }) (vec3 0 0 0) (vec3 1 0 0) ]
                , boundingBoxes = [ boxOf -20 -10 -29.2 20 10 30 ]
                }

        -- Hinge and turntable halves (rigid: the joint is not modelled; box = plate body only)
        "2429" ->
            Just
                (makePart "2429.dat"
                    [ vec3 -30 0 10, vec3 -10 0 10 ]
                    [ vec3 -30 8 10, vec3 -10 8 10 ]
                    [ boxOf -40 -4 0 0 8 20 ]
                )

        "2430" ->
            Just
                (makePart "2430.dat"
                    [ vec3 10 0 10, vec3 30 0 10 ]
                    [ vec3 10 8 10, vec3 30 8 10 ]
                    [ boxOf 0 -4 0 40 8 20 ]
                )

        "4275b" ->
            Just (makeBrick 2 1 8 "4275b.dat")

        "4276b" ->
            Just (makeBrick 2 1 8 "4276b.dat")

        "44301a" ->
            Just (makeBrick 2 1 8 "44301a.dat")

        "44302a" ->
            Just (makeBrick 2 1 8 "44302a.dat")

        "44568" ->
            Just (makeBrick 4 1 8 "44568.dat")

        "3937" ->
            -- Hinge 1 x 2 base (anti-studs inferred)
            Just (makePart "3937.dat" [] [ vec3 -10 24 0, vec3 10 24 0 ] [ boxOf -20 2 -10 20 24 10 ])

        "3938" ->
            -- Hinge 1 x 2 top
            Just (makePart "3938.dat" [ vec3 -10 0 0, vec3 10 0 0 ] [] [ boxOf -20 -4 -10 20 16 10 ])

        "3679" ->
            -- Turntable 2 x 2 top
            Just (makePart "3679.dat" grid2x2Studs [] [ boxOf -17.5 -4 -17.5 17.5 8 17.5 ])

        -- Decor: no connection points, measured box
        "3742" ->
            -- Plant flower (fits flower stems, which are not modelled)
            Just (makePart "3742.dat" [] [] [ boxOf -11 -4 -11.5 11 2 11.9 ])

        "3711" ->
            -- Technic chain link
            Just (makePart "3711.dat" [] [] [ boxOf -9.7 -5 -5 9.8 5 21 ])

        "4000" ->
            -- Ladder 4 x 15.6 with pivot
            Just (makePart "4000.dat" [] [] [ boxOf -266 -21 -35 46 34 35 ])

        "4593" ->
            -- Hinge control stick
            Just (makePart "4593.dat" [] [] [ boxOf -4 -38 -4 4 4 4 ])

        "60601" ->
            -- Glass for window 1 x 2 x 2 (hidden; old models only): no boxes, so
            -- Health skips it instead of flagging the glass inside its frame
            Just (makePart "60601.dat" [] [] [])

        "60602" ->
            -- Glass for window 1 x 2 x 3 (hidden; old models only): no boxes
            Just (makePart "60602.dat" [] [] [])

        "4079" ->
            Just
                { part = "4079.dat"
                , connections =
                    [ studAt 1 (vec3 -10 0 -10)
                    , studAt 2 (vec3 10 0 -10)
                    , antiStudAt 10 (vec3 -10 8 -10)
                    , antiStudAt 11 (vec3 10 8 -10)
                    , antiStudAt 12 (vec3 -10 8 10)
                    , antiStudAt 13 (vec3 10 8 10)
                    , antiStudAt 14 (vec3 0 8 0)
                    ]
                , boundingBoxes =
                    [ { min = vec3 -20 -4 -20, max = vec3 20 8 20 }
                    , { min = vec3 -20 -40 12, max = vec3 20 0 20 }
                    ]
                }

        _ ->
            Nothing


{-| A 1 x 1 plate with a clip on its front. The clip point is the centre of the
held bar (0, 4, −20) and its normal is the bar axis (vertical), so a bar dragged
into the clip stands upright beside the plate instead of passing through it.
-}
makeClipPlate : String -> PartConnectivity
makeClipPlate pName =
    withConnections [ clipAt 20 (vec3 0 4 -20) (vec3 0 -1 0) ] (makeBrick 1 1 8 pName)


{-| A clip (`ClipMale`, 4 LDU bar) at the centre of the bar it holds, with its
normal along the held bar's axis. Clips and bars align by anti-parallel normals,
and a bar's normal runs along the bar, so this seats the bar inside the clip.
-}
clipAt : Int -> Vec3 -> Vec3 -> ConnectionPoint
clipAt id position barAxis =
    technicAt id (ClipMale { width = 4.0 }) position barAxis


{-| A connection point with an explicit type, position and normal; the tangent
is any axis perpendicular to the normal.
-}
technicAt : Int -> ConnectionType -> Vec3 -> Vec3 -> ConnectionPoint
technicAt id connType position normal =
    { id = id
    , connType = connType
    , localPosition = position
    , normal = normal
    , tangent =
        if abs (Vec3.getY normal) > 0.7 then
            vec3 1 0 0

        else
            vec3 0 1 0
    }


{-| A part with `extra` connection points added to `base`'s.
-}
withConnections : List ConnectionPoint -> PartConnectivity -> PartConnectivity
withConnections extra base =
    { base | connections = base.connections ++ extra }


{-| A part from explicit stud and anti-stud positions: studs get ids 1…,
anti-studs ids 100…. The FIRST box drives drag collisions and where a freshly
added part rests (until its mesh is built), so it must reach the part's real
bottom; Health checks every box.
-}
makePart : String -> List Vec3 -> List Vec3 -> List BoundingBox3D -> PartConnectivity
makePart partName studs antiStuds boxes =
    { part = partName
    , connections =
        List.indexedMap (\idx position -> studAt (idx + 1) position) studs
            ++ List.indexedMap (\idx position -> antiStudAt (idx + 100) position) antiStuds
    , boundingBoxes = boxes
    }


{-| A box from its min and max corners.
-}
boxOf : Float -> Float -> Float -> Float -> Float -> Float -> BoundingBox3D
boxOf minX minY minZ maxX maxY maxZ =
    { min = vec3 minX minY minZ, max = vec3 maxX maxY maxZ }


{-| Studs on a 2 x 2 grid at Y = 0.
-}
grid2x2Studs : List Vec3
grid2x2Studs =
    grid2x2At 0


{-| Positions on a 2 x 2 grid (±10, ±10) at height `y`.
-}
grid2x2At : Float -> List Vec3
grid2x2At y =
    [ vec3 -10 y -10, vec3 10 y -10, vec3 -10 y 10, vec3 10 y 10 ]


{-| Anti-studs of the 2 x 2 corner slopes 3045/3046, whose origin is a corner stud.
-}
cornerSlopeAntiStuds : List Vec3
cornerSlopeAntiStuds =
    [ vec3 0 24 0, vec3 20 24 0, vec3 0 24 -20, vec3 20 24 -20 ]


{-| The two pin holes of the panel fairings 32527/32528, along Z.
-}
fairingPinHoles : List ConnectionPoint
fairingPinHoles =
    [ technicAt 1 (TechnicPinFemale { length = 20 }) (vec3 0 -30 -20) (vec3 0 0 1)
    , technicAt 2 (TechnicPinFemale { length = 20 }) (vec3 0 -50 -20) (vec3 0 0 1)
    ]


{-| An inverted slope `width` studs along X and `depth` studs toward −Z: studs on
the full top grid (z = 0, −20, …), anti-studs only on the back row (z = 0) where
the flat bottom is. The empty space under the overhang is inside the box.
-}
makeInvertedSlope : Int -> Int -> String -> PartConnectivity
makeInvertedSlope width depth partName =
    let
        halfW =
            toFloat width * 10.0

        xs =
            centeredPitch width

        zs =
            List.map (\j -> -20.0 * toFloat j) (List.range 0 (depth - 1))
    in
    makePart partName
        (List.concatMap (\x -> List.map (\z -> vec3 x 0 z) zs) xs)
        (List.map (\x -> vec3 x 24 0) xs)
        [ boxOf -halfW -4 (10 - 20 * toFloat depth) halfW 24 10 ]


{-| A bar `n` studs long along +Y from its origin at one end (Y 0 to 20n), so a
fresh add stands it upright. Bar points (`BarFemale`, 4 LDU) sit at 1-stud steps
(Y 10, 30, …, 20n − 10) plus 4 LDU from each end (Y 4 and 20n − 4), so a bar in a
ground-level clip can seat with its end flush on the ground.
-}
makeBar : Int -> String -> PartConnectivity
makeBar n partName =
    let
        len =
            20.0 * toFloat n

        ys =
            4 :: List.map (\i -> 10.0 + 20.0 * toFloat i) (List.range 0 (n - 1)) ++ [ len - 4 ]
    in
    { part = partName
    , connections =
        List.indexedMap
            (\idx y ->
                { id = idx + 1
                , connType = BarFemale { diameter = 4.0 }
                , localPosition = vec3 0 y 0
                , normal = vec3 0 1 0
                , tangent = vec3 1 0 0
                }
            )
            ys
    , boundingBoxes = [ boxOf -4 0 -4 4 len 4 ]
    }


{-| A baseplate `length` x `width` studs: studs only (no anti-studs, so stud ids
1… never clash with anti-stud ids from 100), box from Y −4 (studs) to 4.
-}
makeBaseplate : Int -> Int -> String -> PartConnectivity
makeBaseplate length width partName =
    let
        halfL =
            toFloat length * 10.0

        halfW =
            toFloat width * 10.0
    in
    makePart partName
        (List.concatMap (\x -> List.map (\z -> vec3 x 0 z) (centeredPitch width)) (centeredPitch length))
        []
        [ boxOf -halfL -4 -halfW halfL 4 halfW ]


{-| Baseplate 32 x 32 (3811), built once.
-}
baseplate32x32 : PartConnectivity
baseplate32x32 =
    makeBaseplate 32 32 "3811.dat"


{-| Baseplate 16 x 16 (3867), built once.
-}
baseplate16x16 : PartConnectivity
baseplate16x16 =
    makeBaseplate 16 16 "3867.dat"


{-| An upward stud (normal −Y in LDraw's Y-down space) at `position`.
-}
studAt : Int -> Vec3 -> ConnectionPoint
studAt id position =
    { id = id
    , connType = Stud
    , localPosition = position
    , normal = vec3 0 -1 0
    , tangent = vec3 1 0 0
    }


{-| A downward anti-stud (normal +Y) at `position`.
-}
antiStudAt : Int -> Vec3 -> ConnectionPoint
antiStudAt id position =
    { id = id
    , connType = AntiStud
    , localPosition = position
    , normal = vec3 0 1 0
    , tangent = vec3 1 0 0
    }


{-| Side studs (SNOT): each is the stud's base on the part's face and its outward
normal. Ids start at 200 so they never clash with `makeBrick`'s.
-}
sideStudPoints : List ( Vec3, Vec3 ) -> List ConnectionPoint
sideStudPoints studs =
    List.indexedMap
        (\idx ( position, normal ) ->
            { id = 200 + idx
            , connType = Stud
            , localPosition = position
            , normal = normal
            , tangent = vec3 0 1 0
            }
        )
        studs


{-| A part with extra side studs (see `sideStudPoints`). Its box is unchanged:
side studs protrude past the body, and `effectiveEnvelope` leaves them out anyway.
-}
withSideStuds : List ( Vec3, Vec3 ) -> PartConnectivity -> PartConnectivity
withSideStuds studs base =
    { base | connections = base.connections ++ sideStudPoints studs }


{-| A tile: a plate's anti-studs underneath (8 LDU down) and no studs on top, so
its box starts at Y = 0 with no stud protrusion.
-}
makeTile : Int -> Int -> String -> PartConnectivity
makeTile length width partName =
    let
        plate =
            makeBrick length width 8 partName

        antiStuds =
            List.filter (\c -> c.connType == AntiStud) plate.connections

        box =
            List.head plate.boundingBoxes
                |> Maybe.map (\b -> { b | min = vec3 (Vec3.getX b.min) 0 (Vec3.getZ b.min) })
                |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
    in
    { plate | connections = antiStuds, boundingBoxes = [ box ] }


{-| A bracket with length `length` along X: a plate (studs at Y = 0, anti-studs at Y = 8) with a
vertical plate along its front edge (Z −14 to −10) carrying `sideStuds`.
`minY` and `maxY` are the vertical extent of the whole part.
-}
makeBracket : Int -> String -> { minY : Float, maxY : Float } -> List ( Vec3, Vec3 ) -> PartConnectivity
makeBracket length partName extent sideStuds =
    let
        halfL =
            toFloat length * 10.0

        plate =
            makeBrick length 1 8 partName
    in
    { plate
        | connections = plate.connections ++ sideStudPoints sideStuds
        , boundingBoxes = [ { min = vec3 -halfL extent.minY -14, max = vec3 halfL extent.maxY 10 } ]
    }


{-| Centered 20 LDU pitch coordinates for `count` studs or modules.
For count = 1, returns [ 0.0 ].
For count = 2, returns [ -10.0, 10.0 ].
-}
centeredPitch : Int -> List Float
centeredPitch count =
    List.map (\i -> -10.0 * toFloat count + 10.0 + toFloat i * 20.0) (List.range 0 (count - 1))


{-| Procedurally generate connectivity descriptor for a double-ended Technic pin.
`len` is the half-extent along Z (10.0 for 2L pins, 15.0 for 3L pins) or pin length (30.0).
`friction` indicates whether the pin has friction ridges.
-}
makeTechnicPin : Float -> Bool -> String -> PartConnectivity
makeTechnicPin len friction partName =
    let
        ( halfExtent, pegCenter, connLength ) =
            if len >= 12.0 then
                ( 30.0, 20.0, 30.0 )

            else
                ( 20.0, 10.0, 20.0 )

        baseConnections =
            [ { id = 1
              , connType = TechnicPinMale { length = connLength, friction = friction }
              , localPosition = vec3 pegCenter 0 0
              , normal = vec3 1 0 0
              , tangent = vec3 0 1 0
              }
            , { id = 2
              , connType = TechnicPinMale { length = connLength, friction = friction }
              , localPosition = vec3 -pegCenter 0 0
              , normal = vec3 -1 0 0
              , tangent = vec3 0 1 0
              }
            ]

        connections =
            if len >= 12.0 then
                baseConnections
                    ++ [ { id = 3
                         , connType = TechnicPinMale { length = connLength, friction = friction }
                         , localPosition = vec3 0 0 0
                         , normal = vec3 1 0 0
                         , tangent = vec3 0 1 0
                         }
                       ]

            else
                baseConnections

        bbox =
            [ { min = vec3 -halfExtent -8 -8, max = vec3 halfExtent 8 8 } ]
    in
    { part = partName
    , connections = connections
    , boundingBoxes = bbox
    }


makeBrick : Int -> Int -> Float -> String -> PartConnectivity
makeBrick length width height partName =
    let
        halfL =
            toFloat length * 10.0

        halfW =
            toFloat width * 10.0

        xCoords =
            centeredPitch length

        zCoords =
            centeredPitch width

        studPositions =
            List.concatMap (\x -> List.map (\z -> ( x, z )) zCoords) xCoords

        studPoints =
            List.indexedMap
                (\idx ( x, z ) -> studAt (idx + 1) (vec3 x 0 z))
                studPositions

        antiStudPoints =
            List.indexedMap
                (\idx ( x, z ) -> antiStudAt (idx + 100) (vec3 x height z))
                studPositions

        allConnections =
            studPoints ++ antiStudPoints

        bbox =
            { min = vec3 -halfL -4 -halfW
            , max = vec3 halfL height halfW
            }
    in
    { part = partName
    , connections = allConnections
    , boundingBoxes = [ bbox ]
    }


makeJumperPlate1x2 : String -> PartConnectivity
makeJumperPlate1x2 partName =
    { part = partName
    , connections =
        [ studAt 1 (vec3 0 0 0)
        , antiStudAt 101 (vec3 -10 8 0)
        , antiStudAt 102 (vec3 10 8 0)
        , antiStudAt 103 (vec3 0 8 0)
        ]
    , boundingBoxes = [ { min = vec3 -20 -4 -10, max = vec3 20 8 10 } ]
    }


makeJumperPlate2x2 : String -> PartConnectivity
makeJumperPlate2x2 partName =
    { part = partName
    , connections =
        [ studAt 1 (vec3 0 0 0)
        , antiStudAt 101 (vec3 -10 8 -10)
        , antiStudAt 102 (vec3 10 8 -10)
        , antiStudAt 103 (vec3 -10 8 10)
        , antiStudAt 104 (vec3 10 8 10)
        , antiStudAt 105 (vec3 0 8 0)
        ]
    , boundingBoxes = [ { min = vec3 -20 -4 -20, max = vec3 20 8 20 } ]
    }


{-| Procedurally generate connectivity descriptor for a standard LDraw slope brick.
`width` is in studs along X, `depth` is in studs along -Z (`frontZ = 10 - depth * 20`),
and `height` is the vertical height in LDU (typically 24).

Top studs form a single row along X at Z = 0, Y = 0 (studs from `-halfW + 10` to `halfW - 10`).
Anti-studs form a full grid of `width` × `depth` anti-studs at Y = height,
X from `-halfW + 10` to `halfW - 10`, Z in `{0, -20, ..., -(depth - 1) * 20}`.
Bounding box: min = vec3 -halfW -4 frontZ, max = vec3 halfW height 10.

-}
makeSlope : Int -> Int -> Float -> String -> PartConnectivity
makeSlope width depth height partName =
    let
        halfW =
            toFloat width * 10.0

        frontZ =
            10.0 - toFloat depth * 20.0

        xCoords =
            List.map (\i -> -halfW + 10.0 + toFloat i * 20.0) (List.range 0 (width - 1))

        zCoords =
            List.map (\j -> -(toFloat j) * 20.0) (List.range 0 (depth - 1))

        studPoints =
            List.indexedMap
                (\idx x ->
                    { id = idx + 1
                    , connType = Stud
                    , localPosition = vec3 x 0 0
                    , normal = vec3 0 -1 0
                    , tangent = vec3 1 0 0
                    }
                )
                xCoords

        antiStudPositions =
            List.concatMap (\x -> List.map (\z -> ( x, z )) zCoords) xCoords

        antiStudPoints =
            List.indexedMap
                (\idx ( x, z ) ->
                    { id = idx + 100
                    , connType = AntiStud
                    , localPosition = vec3 x height z
                    , normal = vec3 0 1 0
                    , tangent = vec3 1 0 0
                    }
                )
                antiStudPositions

        allConnections =
            studPoints ++ antiStudPoints

        bbox =
            { min = vec3 -halfW -4 frontZ
            , max = vec3 halfW height 10
            }
    in
    { part = partName
    , connections = allConnections
    , boundingBoxes = [ bbox ]
    }


{-| Procedurally generate connectivity descriptor for a Technic axle of given length in studs.
Each stud center along the axle has a connection point.
-}
makeAxle : Int -> String -> PartConnectivity
makeAxle studs partName =
    let
        halfL =
            toFloat studs * 10.0

        totalLen =
            toFloat studs * 20.0

        connections =
            List.range 0 (studs - 1)
                |> List.map
                    (\i ->
                        let
                            x =
                                -10.0 * toFloat studs + 10.0 + toFloat i * 20.0
                        in
                        { id = i + 1
                        , connType = TechnicAxleMale { length = totalLen }
                        , localPosition = vec3 x 0 0
                        , normal = vec3 1 0 0
                        , tangent = vec3 0 1 0
                        }
                    )

        bbox =
            { min = vec3 -halfL -4 -4
            , max = vec3 halfL 4 4
            }
    in
    { part = partName
    , connections = connections
    , boundingBoxes = [ bbox ]
    }


{-| Procedurally generate connection points for a female axle hole with given center, half-thickness, and base ID.
Generates center (dz=0), front entrance (dz=halfThick), and back entrance (dz=-halfThick) points.
-}
makeAxleHolePoints : Int -> Float -> Vec3 -> List ConnectionPoint
makeAxleHolePoints startId halfThick center =
    [ { id = startId
      , connType = TechnicAxleFemale { length = halfThick * 2.0 }
      , localPosition = center
      , normal = vec3 0 0 1
      , tangent = vec3 1 0 0
      }
    , { id = startId + 1
      , connType = TechnicAxleFemale { length = halfThick * 2.0 }
      , localPosition = Vec3.add center (vec3 0 0 halfThick)
      , normal = vec3 0 0 1
      , tangent = vec3 1 0 0
      }
    , { id = startId + 2
      , connType = TechnicAxleFemale { length = halfThick * 2.0 }
      , localPosition = Vec3.add center (vec3 0 0 -halfThick)
      , normal = vec3 0 0 -1
      , tangent = vec3 1 0 0
      }
    ]


{-| Procedurally generate connectivity descriptor for a Technic gear with given pitch radius and thickness.
Center axle hole at (0, 0, 0) with center, front entrance, and back entrance points.
-}
makeGear : Float -> Float -> String -> PartConnectivity
makeGear radius thickness partName =
    let
        halfThick =
            thickness / 2.0

        connections =
            makeAxleHolePoints 1 halfThick (vec3 0 0 0)

        boundingBoxes =
            if radius <= 8.0 || halfThick <= 5.0 then
                [ { min = vec3 -radius -radius -halfThick
                  , max = vec3 radius radius halfThick
                  }
                ]

            else
                [ { min = vec3 -8.0 -8.0 -halfThick
                  , max = vec3 8.0 8.0 halfThick
                  }
                , { min = vec3 -radius -radius -5.0
                  , max = vec3 radius radius 5.0
                  }
                ]
    in
    { part = partName
    , connections = connections
    , boundingBoxes = boundingBoxes
    }


{-| Connectivity for a thin gear whose body does not span the usual ±10 LDU
around its origin, such as the single bevel gears 6589 and 4143.

The axle-hole connection points stay at ±10 exactly as `makeGear radius 20`
places them, so axle placement does not change. Only the bounding boxes follow
the real part: first a hub box (±8 LDU across, over the whole `body` z span),
then a box of the given radius over the `teeth` z band. The hub box comes
first because the studio's snap and drag collision checks use only the first
box. With the old symmetric ±10 hub, two such gears at right angles on
crossing axles overlap by about 1–2 LDU. `isSnapColliding` never checks the
dragged part against the snap target itself, so this matters for other parts
near the pair (and for overlap reports), not for the snap onto the partner.

-}
makeGearSpan : Float -> { zMin : Float, zMax : Float } -> { zMin : Float, zMax : Float } -> String -> PartConnectivity
makeGearSpan radius body teeth partName =
    { part = partName
    , connections = makeAxleHolePoints 1 10.0 (vec3 0 0 0)
    , boundingBoxes =
        [ { min = vec3 -8.0 -8.0 body.zMin
          , max = vec3 8.0 8.0 body.zMax
          }
        , { min = vec3 -radius -radius teeth.zMin
          , max = vec3 radius radius teeth.zMax
          }
        ]
    }


{-| Procedurally generate connection points for pin holes in a gear given 2D (x, y) coordinates.
Generates center (z=0), front entrance (z=10), and back entrance (z=-10) points for each hole.
-}
makeGearPinHoles : Int -> List ( Float, Float ) -> List ConnectionPoint
makeGearPinHoles startId coords =
    coords
        |> List.indexedMap
            (\i ( x, y ) ->
                let
                    baseId =
                        startId + i * 3
                in
                [ { id = baseId
                  , connType = TechnicPinFemale { length = 20.0 }
                  , localPosition = vec3 x y 0
                  , normal = vec3 0 0 1
                  , tangent = vec3 1 0 0
                  }
                , { id = baseId + 1
                  , connType = TechnicPinFemale { length = 20.0 }
                  , localPosition = vec3 x y 10
                  , normal = vec3 0 0 1
                  , tangent = vec3 1 0 0
                  }
                , { id = baseId + 2
                  , connType = TechnicPinFemale { length = 20.0 }
                  , localPosition = vec3 x y -10
                  , normal = vec3 0 0 -1
                  , tangent = vec3 1 0 0
                  }
                ]
            )
        |> List.concat


makeTechnicBrick : Int -> String -> PartConnectivity
makeTechnicBrick studs partName =
    let
        base =
            makeBrick studs 1 24 partName

        holePoints =
            List.range 0 (studs - 2)
                |> List.concatMap
                    (\i ->
                        let
                            x =
                                -10.0 * toFloat (studs - 2) + toFloat i * 20.0

                            baseId =
                                200 + i * 3
                        in
                        [ { id = baseId
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 x 10 0
                          , normal = vec3 0 0 1
                          , tangent = vec3 1 0 0
                          }
                        , { id = baseId + 1
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 x 10 10
                          , normal = vec3 0 0 1
                          , tangent = vec3 1 0 0
                          }
                        , { id = baseId + 2
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 x 10 -10
                          , normal = vec3 0 0 -1
                          , tangent = vec3 1 0 0
                          }
                        ]
                    )
    in
    { base | connections = base.connections ++ holePoints }


makeLiftarm : Int -> String -> PartConnectivity
makeLiftarm studs partName =
    let
        halfL =
            toFloat studs * 10.0

        connections =
            List.range 0 (studs - 1)
                |> List.concatMap
                    (\i ->
                        let
                            z =
                                -10.0 * toFloat (studs - 1) + toFloat i * 20.0

                            baseId =
                                i * 3 + 1
                        in
                        [ { id = baseId
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 0 0 z
                          , normal = vec3 0 1 0
                          , tangent = vec3 0 0 1
                          }
                        , { id = baseId + 1
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 0 10 z
                          , normal = vec3 0 1 0
                          , tangent = vec3 0 0 1
                          }
                        , { id = baseId + 2
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 0 -10 z
                          , normal = vec3 0 -1 0
                          , tangent = vec3 0 0 1
                          }
                        ]
                    )

        bbox =
            { min = vec3 -10 -10 -halfL
            , max = vec3 10 10 halfL
            }
    in
    { part = partName
    , connections = connections
    , boundingBoxes = [ bbox ]
    }


{-| Procedurally generate connectivity descriptor for a wheel rim.
Center is at (0, 0, 0) with axis Z.
If isAxleHole is True, creates TechnicAxleFemale points;
if False, creates a single TechnicPinFemale point at z = min 10 halfThick on the inner face (normal = +Z).
Also includes a concentric WheelRim connection point at (0, 0, 0).
-}
makeWheelRim : Bool -> Float -> Float -> Float -> String -> PartConnectivity
makeWheelRim isAxleHole thickness radius tireDiameter partName =
    let
        halfThick =
            thickness / 2.0

        centerPoints =
            if isAxleHole then
                makeAxleHolePoints 1 halfThick (vec3 0 0 0)

            else
                [ { id = 1
                  , connType = TechnicPinFemale { length = thickness }
                  , localPosition = vec3 0 0 -(min 10.0 halfThick)
                  , normal = vec3 0 0 1
                  , tangent = vec3 1 0 0
                  }
                ]

        tirePoint =
            { id = 100
            , connType = WheelRim { diameter = tireDiameter }
            , localPosition = vec3 0 0 0
            , normal = vec3 0 0 1
            , tangent = vec3 1 0 0
            }

        connections =
            tirePoint :: centerPoints

        boundingBoxes =
            [ { min = vec3 -radius -radius -halfThick
              , max = vec3 radius radius halfThick
              }
            ]
    in
    { part = partName
    , connections = connections
    , boundingBoxes = boundingBoxes
    }


{-| Procedurally generate connectivity descriptor for a tire.
Center is at (0, 0, 0) with axis Z.
Uses 4 hollow ring wall bounding boxes surrounding the central bore [-innerR, innerR] x [-innerR, innerR]
so axles, pins, bushes, and wheel holders passing through the center never collide with the tire.
-}
makeTire : Float -> Float -> Float -> Float -> String -> PartConnectivity
makeTire thickness outerR inner tireDiameter partName =
    let
        halfThick =
            thickness / 2.0

        innerR =
            if inner >= outerR then
                inner / 2.0

            else if inner > outerR * 0.65 then
                inner / 2.0

            else
                inner

        connections =
            [ { id = 1
              , connType = WheelTire { diameter = tireDiameter }
              , localPosition = vec3 0 0 0
              , normal = vec3 0 0 1
              , tangent = vec3 1 0 0
              }
            ]

        boundingBoxes =
            [ { min = vec3 -outerR -outerR -halfThick, max = vec3 -innerR outerR halfThick }
            , { min = vec3 innerR -outerR -halfThick, max = vec3 outerR outerR halfThick }
            , { min = vec3 -innerR -outerR -halfThick, max = vec3 innerR -innerR halfThick }
            , { min = vec3 -innerR innerR -halfThick, max = vec3 innerR outerR halfThick }
            ]
    in
    { part = partName
    , connections = connections
    , boundingBoxes = boundingBoxes
    }
