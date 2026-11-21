module Gear.Physics exposing
    ( KinematicLinkage
    , LinkageState
    , Ratio
    , angleAt
    , crankSliderPosition
    , crankSliderRodAngle
    , cycleMotorTurns
    , defaultMotorGearId
    , fourBarAngles
    , propagate
    , propagateRatios
    , propagateWithLinkages
    )

{-| Pure gear physics and analytical mechanical linkage simulation.


## Model

Gear simulation is a deterministic function of time, not an incremental
integrator. At any time `t` the motor gear has rotated:

    motorAngle =
        motorSpeedRadPerSec * t

All connected gears rotate at angles determined by the gear ratio from the
motor to that gear via BFS traversal of the `GearGraph`.

Connected mechanical linkages (crank-sliders, four-bar linkages) evaluate analytical
kinematic equations synchronized with driving gears.


## Gear ratios

For each meshing pair (g1 → g2):

    ratio2 =
        ratio1 * meshRatio g1 g2

The magnitude is `teeth1 / teeth2`. The sign comes from the no-slip rule at the
pitch point `P`: with `v_X = n_X × (P − C_X)`, the signed ratio is
`k = (v_1·v_2) / |v_2|²`. Two spur gears with the same axis orientation give
`−teeth1/teeth2`. Flipping one gear's axis gives a positive ratio. Crown,
bevel and double bevel pairs (on parallel or crossed axles) take their sign
from the same formula, with `P` from `Gear.Phase.contactPoint`.

Coaxial (rigid) neighbours turn at ratio `+1` when their true axes point the
same way, and `-1` when one is flipped (`rigidSign`).

Worm ratio magnitude uses starts/tooth metadata. Local LDraw geometry for both
4716 and 32905 uses the same right-handed thread subpart: its helix angle
increases along local +Z. The ratio sign follows that handedness and the
oriented worm axis, wheel axis, and center side; a valid native arrangement
(worm +X, wheel +Z at +Y) therefore has a negative ratio. Reversing either
axis or moving the wheel to the opposite side reverses the sign.

For non-worm meshes, degenerate geometry (coincident centres, no pitch point)
falls back to the same-orientation rule `−teeth1/teeth2`.


## Exact cycles

`propagateRatios` walks the same BFS tree as `propagate` but keeps each gear's
speed ratio magnitude as a reduced fraction. `cycleMotorTurns` turns a list of
such ratios into the smallest number of motor turns after which every one of
those gears has made a whole number of revolutions, so an animation of that
length loops seamlessly.

-}

import Array
import Dict exposing (Dict)
import Gear.Detect as Detect
import Gear.Phase as Phase
import Gear.Types exposing (GearGraph, GearId, GearInstance)
import Math.Vector3 as Vec3 exposing (Vec3)
import Set exposing (Set)


{-| Definition of a 1-DOF kinematic crank-slider linkage driven by a gear.
-}
type alias KinematicLinkage =
    { id : Int
    , drivingGearId : GearId
    , crankRadius : Float
    , rodLength : Float
    , crankOffset : Float
    }


{-| An exact, non-negative gear speed ratio `num / den` (turns of a gear per
motor turn), always reduced.
-}
type alias Ratio =
    { num : Int
    , den : Int
    }


{-| Evaluated state of a kinematic linkage at a point in time.
-}
type alias LinkageState =
    { sliderX : Float
    , rodAngle : Float
    }



-- ── Public API ────────────────────────────────────────────────────────────────


{-| Propagate rotation from a motor gear through the graph via BFS.

    propagate graph motorId motorAngle

Returns a `Dict GearId Float` mapping each reachable gear to its current
angle in radians. The motor gear itself maps to `motorAngle`.

Gears not reachable from `motorId` are absent from the result.

-}
propagate : GearGraph -> GearId -> Float -> Dict GearId Float
propagate graph motorId motorAngle =
    case gearInstance graph motorId of
        Just motor ->
            if motor.spec.rotates && motor.spec.motorCapable then
                bfsWith
                    { mesh = \g1 g2 angle -> angle * meshRatio g1 g2
                    , rigid =
                        \pair angle ->
                            case pair of
                                Just ( g1, g2 ) ->
                                    angle * rigidSign g1 g2

                                Nothing ->
                                    angle
                    }
                    graph
                    [ ( motorId, motorAngle ) ]
                    (Set.singleton motorId)
                    (Dict.singleton motorId motorAngle)

            else
                Dict.empty

        Nothing ->
            Dict.empty


{-| Exact speed ratio magnitudes from the motor gear to every reachable gear.

    propagateRatios graph motorId

Uses the same BFS tree (and the same fallbacks) as `propagate`, so for every
gear `g` in the result `toFloat num / toFloat den == abs (angle g / motorAngle)`
for `propagate graph motorId motorAngle`. A mesh edge multiplies by
`teeth1 / teeth2` (a worm's `teeth` holds its start count); a rigid coaxial
edge keeps the ratio. The motor gear maps to `1 / 1`.

-}
propagateRatios : GearGraph -> GearId -> Dict GearId Ratio
propagateRatios graph motorId =
    case gearInstance graph motorId of
        Just motor ->
            if motor.spec.rotates && motor.spec.motorCapable then
                let
                    one =
                        { num = 1, den = 1 }
                in
                bfsWith
                    { mesh =
                        \g1 g2 ratio ->
                            reduceRatio
                                { num = ratio.num * abs g1.spec.teeth
                                , den = ratio.den * abs g2.spec.teeth
                                }
                    , rigid = \_ ratio -> ratio
                    }
                    graph
                    [ ( motorId, one ) ]
                    (Set.singleton motorId)
                    (Dict.singleton motorId one)

            else
                Dict.empty

        Nothing ->
            Dict.empty


{-| The smallest number of motor turns after which every gear with one of the
given speed ratios is back at its starting angle (has made a whole number of
revolutions): `lcm dens / gcd nums` for reduced ratios.

Ratios with `num = 0` (gears that do not turn) are ignored. Returns `Nothing`
for an empty list, for a ratio with `den = 0`, when the cycle would need more
than 10000 motor turns' worth of denominator (`lcm dens > 10000`), which is
far longer than any useful animation, and when a reduced `num` is over 10000
(a train so fast that its exact ratios would lose precision as Ints).

    cycleMotorTurns [ { num = 1, den = 1 }, { num = 1, den = 3 } ] == Just 3

-}
cycleMotorTurns : List Ratio -> Maybe Float
cycleMotorTurns ratios =
    let
        turning =
            ratios
                |> List.map reduceRatio
                |> List.filter (\r -> r.num /= 0)

        step r acc =
            case acc of
                Nothing ->
                    Nothing

                Just ( l, g ) ->
                    if r.den <= 0 || r.num > maxCycleDenominator then
                        Nothing

                    else
                        let
                            nextL =
                                lcmInt l r.den
                        in
                        if nextL > maxCycleDenominator then
                            Nothing

                        else
                            Just ( nextL, gcdInt g r.num )
    in
    if List.isEmpty turning then
        Nothing

    else
        List.foldl step (Just ( 1, 0 )) turning
            |> Maybe.andThen
                (\( l, g ) ->
                    if g <= 0 then
                        Nothing

                    else
                        Just (toFloat l / toFloat g)
                )


{-| The largest `lcm dens` and reduced `num` `cycleMotorTurns` accepts.
-}
maxCycleDenominator : Int
maxCycleDenominator =
    10000


{-| Choose the motor-capable rotating gear whose directed traversal reaches the
largest rotating set. `GearGraph.instances` is in document order, so ties keep
the earliest original gear ID. Worm edges remain directional during this
selection: a wheel cannot be chosen as a route back to its worm.
-}
defaultMotorGearId : GearGraph -> Maybe GearId
defaultMotorGearId graph =
    graph.instances
        |> Array.toList
        |> List.filter (\gear -> gear.spec.rotates && gear.spec.motorCapable)
        |> List.foldl
            (\gear best ->
                let
                    reachedCount =
                        Dict.size (propagate graph gear.id 1.0)
                in
                case best of
                    Nothing ->
                        Just ( gear.id, reachedCount )

                    Just ( _, bestCount ) ->
                        if reachedCount > bestCount then
                            Just ( gear.id, reachedCount )

                        else
                            best
            )
            Nothing
        |> Maybe.map Tuple.first


{-| Angle for a single gear given the propagation result.

Returns `0.0` if the gear is not reachable (not connected to the motor).

-}
angleAt : Dict GearId Float -> GearId -> Float
angleAt angles gearId =
    Dict.get gearId angles |> Maybe.withDefault 0.0


{-| Analytical crank-slider slider displacement x(theta):
x(theta) = r \* cos(theta) + sqrt(l^2 - r^2 \* sin^2(theta))
-}
crankSliderPosition : Float -> Float -> Float -> Float
crankSliderPosition crankRadius rodLength theta =
    let
        sinTheta =
            sin theta

        rSin =
            crankRadius * sinTheta

        underRoot =
            rodLength * rodLength - rSin * rSin
    in
    crankRadius * cos theta + sqrt (max 0.0 underRoot)


{-| Analytical crank-slider connecting rod tilt angle phi(theta):
phi(theta) = asin(-r/l \* sin(theta))
-}
crankSliderRodAngle : Float -> Float -> Float -> Float
crankSliderRodAngle crankRadius rodLength theta =
    if rodLength < 0.0001 then
        0.0

    else
        let
            ratio =
                clamp -1.0 1.0 (-(crankRadius / rodLength) * sin theta)
        in
        asin ratio


{-| Analytical planar four-bar linkage angles (couplerAngle, rockerAngle).
Given link lengths `{ ground, crank, coupler, rocker }` and input crank angle `theta`.
-}
fourBarAngles :
    { ground : Float, crank : Float, coupler : Float, rocker : Float }
    -> Float
    -> Maybe ( Float, Float )
fourBarAngles { ground, crank, coupler, rocker } theta =
    let
        bx =
            crank * cos theta

        by =
            crank * sin theta

        -- Vector from rocker pivot (ground, 0) to crank pin (bx, by)
        sx =
            bx - ground

        sy =
            by

        distSquared =
            sx * sx + sy * sy

        dist =
            sqrt distSquared
    in
    if ground < 0.0001 || crank < 0.0001 || dist < 0.0001 || rocker < 0.0001 || coupler < 0.0001 then
        Nothing

    else
        let
            cosDelta =
                (rocker * rocker + distSquared - coupler * coupler) / (2 * rocker * dist)
        in
        if cosDelta < -1.000001 || cosDelta > 1.000001 then
            Nothing

        else
            let
                delta =
                    acos (clamp -1.0 1.0 cosDelta)

                baseAngle =
                    atan2 sy sx

                rockerAngle =
                    baseAngle + delta

                cx =
                    ground + rocker * cos rockerAngle

                cy =
                    rocker * sin rockerAngle

                couplerAngle =
                    atan2 (cy - by) (cx - bx)
            in
            Just ( couplerAngle, rockerAngle )


{-| Propagate rotation through the gear graph and evaluate all driven kinematic linkages.
-}
propagateWithLinkages :
    GearGraph
    -> List KinematicLinkage
    -> GearId
    -> Float
    -> { gearAngles : Dict GearId Float, linkagePositions : Dict Int LinkageState }
propagateWithLinkages graph linkages motorId motorAngle =
    let
        angles =
            propagate graph motorId motorAngle

        evalLinkage linkage =
            let
                theta =
                    angleAt angles linkage.drivingGearId + linkage.crankOffset

                pos =
                    crankSliderPosition linkage.crankRadius linkage.rodLength theta

                rod =
                    crankSliderRodAngle linkage.crankRadius linkage.rodLength theta
            in
            ( linkage.id, { sliderX = pos, rodAngle = rod } )

        linkageDict =
            Dict.fromList (List.map evalLinkage linkages)
    in
    { gearAngles = angles
    , linkagePositions = linkageDict
    }



-- ── Internal ──────────────────────────────────────────────────────────────────


{-| Iterative BFS over the gear graph using an explicit queue.

`propagate` and `propagateRatios` share this traversal, so both visit gears
through exactly the same tree. `mesh g1 g2 value` gives a mesh neighbour's
value from the current gear's; `rigid` does the same for a coaxial neighbour
(`Nothing` when an instance is missing, which keeps the value).

-}
bfsWith :
    { mesh : GearInstance -> GearInstance -> a -> a
    , rigid : Maybe ( GearInstance, GearInstance ) -> a -> a
    }
    -> GearGraph
    -> List ( GearId, a )
    -> Set GearId
    -> Dict GearId a
    -> Dict GearId a
bfsWith step graph queue visited values =
    case queue of
        [] ->
            values

        ( currentId, currentValue ) :: rest ->
            let
                neighbours =
                    Dict.get currentId graph.connections |> Maybe.withDefault []

                rigidNeighbours =
                    Dict.get currentId graph.rigidAxles |> Maybe.withDefault []

                unvisited =
                    List.filter
                        (\n -> not (Set.member n visited) && isRotatingGear graph n)
                        neighbours

                unvisitedRigid =
                    List.filter
                        (\n -> not (Set.member n visited) && isRotatingGear graph n)
                        rigidNeighbours

                ( newVisited, newValues, newEntries ) =
                    List.foldl
                        (\neighbourId ( vis, vals, entries ) ->
                            case ( gearInstance graph currentId, gearInstance graph neighbourId ) of
                                ( Just g1, Just g2 ) ->
                                    let
                                        neighbourValue =
                                            step.mesh g1 g2 currentValue
                                    in
                                    ( Set.insert neighbourId vis
                                    , Dict.insert neighbourId neighbourValue vals
                                    , ( neighbourId, neighbourValue ) :: entries
                                    )

                                _ ->
                                    ( vis, vals, entries )
                        )
                        ( visited, values, [] )
                        unvisited

                ( newVisited2, newValues2, newEntries2 ) =
                    List.foldl
                        (\neighbourId ( vis, vals, entries ) ->
                            let
                                neighbourValue =
                                    step.rigid
                                        (Maybe.map2 Tuple.pair
                                            (gearInstance graph currentId)
                                            (gearInstance graph neighbourId)
                                        )
                                        currentValue
                            in
                            ( Set.insert neighbourId vis
                            , Dict.insert neighbourId neighbourValue vals
                            , ( neighbourId, neighbourValue ) :: entries
                            )
                        )
                        ( newVisited, newValues, [] )
                        unvisitedRigid
            in
            bfsWith step
                graph
                (rest ++ List.reverse newEntries ++ List.reverse newEntries2)
                newVisited2
                newValues2


reduceRatio : Ratio -> Ratio
reduceRatio r =
    let
        g =
            gcdInt r.num r.den
    in
    if g <= 1 then
        r

    else
        { num = r.num // g, den = r.den // g }


gcdInt : Int -> Int -> Int
gcdInt a b =
    if b == 0 then
        abs a

    else
        gcdInt b (modBy (abs b) a)


lcmInt : Int -> Int -> Int
lcmInt a b =
    if a == 0 || b == 0 then
        0

    else
        abs (a // gcdInt a b * b)


{-| Ratio for a co-axial (rigid) pair, `angle(g2) / angle(g1)`.

The gears share one axle, so they turn with the same angular velocity about
the same line. Each gear's angle is measured about its own true axis, so a
gear whose axis points against its partner's turns at the negated angle:
`+1` for matching axis directions, `-1` otherwise.

-}
rigidSign : GearInstance -> GearInstance -> Float
rigidSign g1 g2 =
    if Vec3.dot (Phase.gearFrame g1).axis (Phase.gearFrame g2).axis < 0 then
        -1

    else
        1


gearInstance : GearGraph -> GearId -> Maybe GearInstance
gearInstance graph gearId =
    Array.get gearId graph.instances


isRotatingGear : GearGraph -> GearId -> Bool
isRotatingGear graph gearId =
    gearInstance graph gearId
        |> Maybe.map (\gear -> gear.spec.rotates)
        |> Maybe.withDefault False


{-| Signed ratio for one mesh edge, `angle(g2) / angle(g1)`. The magnitude is
`teeth1 / teeth2`; worms use the documented native-orientation convention in
`wormMeshSign`, while ordinary meshes use `meshSign`.
-}
meshRatio : GearInstance -> GearInstance -> Float
meshRatio g1 g2 =
    let
        baseRatio =
            toFloat g1.spec.teeth / toFloat g2.spec.teeth
    in
    if Detect.isWorm g1.spec || Detect.isWorm g2.spec then
        baseRatio * wormMeshSign g1 g2

    else
        baseRatio * meshSign g1 g2


{-| Worms have no meaningful single pitch point for the spur-style no-slip
calculation. Their screw motion is grounded in the local LDraw geometry: the
shared 4716/32905 thread subpart is right-handed, with angular phase increasing
about local +Z as position increases along local +Z. The Y-up conversion negates
both LDraw Y and Z (a proper rotation), so it preserves this handedness.

For a right-handed thread, positive rotation about the worm's oriented axis
moves the thread axially toward `-wormAxis`. Equating this with the wheel's
tangential contact direction gives the sign `hand * sign((wormAxis × wheelAxis)
· (wheelCentre − wormCentre))`. Thus the +X worm / +Z wheel at +Y arrangement
has a negative ratio, and changing either oriented axis or the wheel's center
side reverses it. This is a kinematic sign inferred from the LDraw thread and
idealized contact directions, not an empirical validation of the molded gear.

If the oriented triple product is degenerate, the contact direction does not
determine an absolute sign and the implementation falls back to +1.

-}
wormMeshSign : GearInstance -> GearInstance -> Float
wormMeshSign g1 g2 =
    let
        ( worm, wheel ) =
            if Detect.isWorm g1.spec then
                ( g1, g2 )

            else
                ( g2, g1 )

        wormFrame =
            Phase.gearFrame worm

        wheelFrame =
            Phase.gearFrame wheel

        wheelFromWorm =
            Vec3.sub wheelFrame.centre wormFrame.centre

        handedOrientation =
            Vec3.dot (Vec3.cross wormFrame.axis wheelFrame.axis) wheelFromWorm
    in
    if abs handedOrientation < 1.0e-6 then
        1

    else if handedOrientation < 0 then
        -wormThreadHandedness

    else
        wormThreadHandedness


{-| The local LDraw worm thread is right-handed: angular phase increases about
local +Z as position increases along local +Z. A positive value denotes
right-handedness in the screw sign formula above.
-}
wormThreadHandedness : Float
wormThreadHandedness =
    1


{-| The no-slip sign of a spur, crown, bevel or double bevel mesh: `+1` when the two gears
turn in the same sense at the pitch point, `-1` otherwise.

With `v_X = n_X × (P − C_X)` the ratio is `k = (v_1·v_2) / |v_2|²`. Only its
sign is used here; the magnitude stays at the tooth-count ratio. Falls back
to `-1` (same-orientation spur rule) when no pitch point exists.

-}
meshSign : GearInstance -> GearInstance -> Float
meshSign g1 g2 =
    case Phase.contactPoint g1 g2 of
        Nothing ->
            -1

        Just p ->
            let
                f1 =
                    Phase.gearFrame g1

                f2 =
                    Phase.gearFrame g2

                v1 =
                    surfaceVelocity f1 p

                v2 =
                    surfaceVelocity f2 p

                denominator =
                    Vec3.dot v2 v2
            in
            if denominator < 1.0e-12 then
                -1

            else if Vec3.dot v1 v2 > 0 then
                1

            else
                -1


surfaceVelocity : Phase.Frame -> Vec3 -> Vec3
surfaceVelocity frame p =
    Vec3.cross frame.axis (Vec3.sub p frame.centre)
