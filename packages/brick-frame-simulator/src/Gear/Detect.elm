module Gear.Detect exposing
    ( MeshFacts
    , MeshMode(..)
    , buildGearGraph
    , buildGearGraphReference
    , canonicalAxis
    , crossedLayouts
    , extractGears
    , isBevelLike
    , isCrownLike
    , isWorm
    , meshFacts
    , meshMode
    , meshing
    )

{-| Gear detection: identify known gear parts within a loaded LDraw
model and build a connectivity graph from their world positions.


## Detection algorithm

1.  `extractGears` — recursive walk of the part tree (same structure as
    `LDraw.Geometry.flatten`). Every `SubFileRef` whose normalised filename
    matches a provided gear-spec entry becomes a `GearInstance`. The world matrix is
    accumulated through the call stack exactly as geometry flattening does it.

2.  `buildGearGraph` — pair comparison over a spatial hash. Two gears mesh
    according to their `MeshMode`:
      - parallel (spur-like) and worm meshes: the centre distance is the sum
        of the pitch radii, from 1.25 LDU closer to 2.02 LDU further (all
        LEGO gears share module 1 mm = 2.5 LDU, so the window is absolute;
        too close binds, too far only adds backlash);
      - crossed (bevel and crown) meshes: the axles are at right angles and
        cross, the teeth face the crossing point, and each gear sits the
        part's reference distance from the other axle (`crossedLayouts`).

-}

import Array
import Dict exposing (Dict)
import Gear.Types exposing (GearAxis(..), GearGraph, GearId, GearInstance, GearKind(..), GearSpec)
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Set exposing (Set)



-- ── Public API ────────────────────────────────────────────────────────────────


{-| Walk the part tree and collect all gear instances.

    extractGears gearSpecs topLevelLines partCache

Returns instances in the order they are encountered (depth-first). IDs are
assigned sequentially starting at 0.

-}
extractGears : List GearSpec -> List LDrawLine -> PartCache -> List GearInstance
extractGears gearSpecs lines cache =
    walkLines gearSpecs lines cache 15 Mat4.identity []
        |> List.reverse
        |> List.indexedMap (\i inst -> { inst | id = i })


{-| Build the gear adjacency graph from a list of gear instances.

Two gears are connected when `meshing` holds (see `meshFacts` for the rules).

Rather than comparing every pair (O(n²)), candidate pairs are enumerated from
two spatial indexes and then classified with the exact `meshing` / `coAxial`
tests. The output is identical to `buildGearGraphReference` — including the
order of each adjacency list — because the candidate pairs are folded in
ascending `(i, j)` order (via `Set (Int, Int)`), matching the reference's nested
`i < j` iteration.

Two indexes are needed because the two relations have different distance
bounds:

  - **Meshing** is distance-bounded (`dist ≈ r1 + r2`), so a 3D position hash
    with a 27-cell neighbour sweep captures every meshing pair.
  - **Co-axial** is _not_ distance-bounded — two gears on the same axle are
    co-axial at any axial separation. Hashing by position would silently drop
    long-axle rigid links. Instead gears are hashed by their _axle line_
    (canonical axis direction + closest point to the origin), so gears sharing
    an axle land in the same bucket however far apart they sit along it.

-}
buildGearGraph : List GearInstance -> GearGraph
buildGearGraph instances =
    let
        arr =
            Array.fromList instances

        indexed =
            Array.toIndexedList arr

        maxPitchRadius =
            List.foldl (\g m -> max m g.spec.pitchRadius) 0.0 instances

        -- A parallel or worm pair is at most `2 * maxPitchRadius + 2.02`
        -- apart (plus a little for the axial offset), and a crossed pair at
        -- most `sqrt (s1² + s2²)` with each `s` below `maxPitchRadius + 8`, so a
        -- cell this size guarantees both gears fall in the same or an
        -- adjacent cell.
        meshCell =
            2.0 * maxPitchRadius + 10.0

        meshKey p =
            ( floor (Vec3.getX p / meshCell)
            , floor (Vec3.getY p / meshCell)
            , floor (Vec3.getZ p / meshCell)
            )

        meshBuckets =
            List.foldl
                (\( idx, g ) d -> Dict.update (meshKey g.worldPosition) (prependIndex idx) d)
                Dict.empty
                indexed

        axleBuckets =
            List.foldl
                (\( idx, g ) d -> Dict.update (axleKey g) (prependIndex idx) d)
                Dict.empty
                indexed

        candidatePairs =
            List.foldl
                (\( idx, g ) set ->
                    let
                        neighbours =
                            meshNeighbourIndices (meshKey g.worldPosition) meshBuckets
                                ++ axleNeighbourIndices (axleKey g) axleBuckets
                    in
                    List.foldl
                        (\j s ->
                            if j /= idx then
                                Set.insert (orderPair idx j) s

                            else
                                s
                        )
                        set
                        neighbours
                )
                Set.empty
                indexed

        ( connections, rigidAxles ) =
            Set.foldl
                (\( i, j ) acc ->
                    case ( Array.get i arr, Array.get j arr ) of
                        ( Just g1, Just g2 ) ->
                            classifyPair i j g1 g2 acc

                        _ ->
                            acc
                )
                ( Dict.empty, Dict.empty )
                candidatePairs
    in
    { instances = arr
    , connections = connections
    , rigidAxles = rigidAxles
    }


{-| Reference O(n²) implementation of `buildGearGraph`, retained as the
correctness oracle for the spatial-hash version (see `Gear.DetectTest`).
-}
buildGearGraphReference : List GearInstance -> GearGraph
buildGearGraphReference instances =
    let
        arr =
            Array.fromList instances

        n =
            Array.length arr

        ( connections, rigidAxles ) =
            List.foldl
                (\i accOuter ->
                    List.foldl
                        (\j acc ->
                            case ( Array.get i arr, Array.get j arr ) of
                                ( Just g1, Just g2 ) ->
                                    classifyPair i j g1 g2 acc

                                _ ->
                                    acc
                        )
                        accOuter
                        (List.range (i + 1) (n - 1))
                )
                ( Dict.empty, Dict.empty )
                (List.range 0 (n - 1))
    in
    { instances = arr
    , connections = connections
    , rigidAxles = rigidAxles
    }


{-| Classify a single ordered pair (`i < j`) and fold its edges into the
`( connections, rigidAxles )` accumulator. Shared verbatim by both graph
builders so their per-pair behaviour — and adjacency-list order — cannot drift.
-}
classifyPair :
    GearId
    -> GearId
    -> GearInstance
    -> GearInstance
    -> ( Dict GearId (List GearId), Dict GearId (List GearId) )
    -> ( Dict GearId (List GearId), Dict GearId (List GearId) )
classifyPair i j g1 g2 ( accC, accA ) =
    if meshing g1 g2 then
        -- Worm drives are self-locking: only add the worm→wheel direction so
        -- that BFS cannot back-drive a worm from the wheel side.
        if isWorm g1.spec then
            ( accC |> addConnection i j, accA )

        else if isWorm g2.spec then
            ( accC |> addConnection j i, accA )

        else
            ( accC
                |> addConnection i j
                |> addConnection j i
            , accA
            )

    else if coAxial g1 g2 then
        ( accC
        , accA
            |> addConnection i j
            |> addConnection j i
        )

    else
        ( accC, accA )



-- ── Internal ──────────────────────────────────────────────────────────────────


{-| How a pair of gears can mesh.

  - `ParallelMesh` — parallel axles in one plane, like two spur gears. Spur,
    double bevel, crown (its rim teeth) and turntable gears mesh this way.
  - `CrossedMesh` — axles at right angles that cross each other: a crown with
    a spur, two double bevels, or two single bevels with the same tooth count.
  - `WormMesh` — a worm and a wheel on skew axles at right angles.

-}
type MeshMode
    = ParallelMesh
    | CrossedMesh
    | WormMesh


{-| The mesh mode two gears would use, from their kinds and, when the kinds
allow both a parallel and a crossed mesh (double bevel pairs, crown with
spur), from the angle between their axles (`|a·b| >= 0.5` is parallel).
`Nothing` when the kinds can never mesh (for example a single bevel and a
spur gear, or two worms).
-}
meshMode : GearInstance -> GearInstance -> Maybe MeshMode
meshMode g1 g2 =
    modeFor g1.spec g2.spec (abs (Vec3.dot (gearAxis g1) (gearAxis g2)))


modeFor : GearSpec -> GearSpec -> Float -> Maybe MeshMode
modeFor s1 s2 absDot =
    if isWorm s1 && isWorm s2 then
        Nothing

    else if isWorm s1 || isWorm s2 then
        Just WormMesh

    else
        let
            parallel =
                meshesParallel s1 && meshesParallel s2

            crossed =
                not (List.isEmpty (crossedLayouts s1 s2))
        in
        if parallel && crossed then
            if absDot >= 0.5 then
                Just ParallelMesh

            else
                Just CrossedMesh

        else if parallel then
            Just ParallelMesh

        else if crossed then
            Just CrossedMesh

        else
            Nothing


{-| Gear kinds whose teeth work like a spur gear on parallel axles.
-}
meshesParallel : GearSpec -> Bool
meshesParallel spec =
    case spec.kind of
        SpurGear ->
            True

        DoubleBevelGear ->
            True

        CrownGear ->
            True

        TurntableGear ->
            True

        BevelGear ->
            False

        WormGear ->
            False


{-| Allowed `distance − (r1 + r2)` (LDU) for parallel and worm meshes:
`-meshPlayNear <= play <= meshPlayFar`.

All LEGO gears share module 1 mm (2.5 LDU), so the play is absolute, not
relative to the gear size. Too close binds, too far only adds backlash, so the
window is asymmetric. On the stud/plate grid (20 / 8 LDU) it accepts every
layout from −1.18 (16T+40T, 2 studs × 7 plates) to +2.00 (8T+16T, 4 plates
straight up; 16T+40T, 9 plates) and rejects the next ones out (−1.92,
12T+24T 2 studs × 2 plates, and +2.05, 12T+40T 1 stud × 8 plates). At +2.00 the teeth still overlap by about 2 LDU (the 16T has the
smallest addendum, 1.7 LDU).

-}
meshPlayNear : Float
meshPlayNear =
    1.25


{-| See `meshPlayNear`.
-}
meshPlayFar : Float
meshPlayFar =
    2.02


{-| How far a parallel pair may sit apart along the axle (LDU). A crown's rim
teeth span about z −5..+4.6, so pairs with a crown allow a little more.
-}
axialLimitFor : GearSpec -> GearSpec -> Float
axialLimitFor s1 s2 =
    if isCrownLike s1 || isCrownLike s2 then
        3.0

    else
        2.0


{-| Largest allowed distance (LDU) between the closest points of two crossed
axles.
-}
maxCrossSkew : Float
maxCrossSkew =
    2.0


{-| Which side of a gear's own origin its bevel or face teeth point to,
along its local axis.
-}
type ToothSide
    = PlusSide
    | MinusSide
    | EitherSide


{-| One gear's requirement in a crossed mesh, given its partner: the crossing
point of the two axles must lie `required` LDU from the gear's centre along
its own axle, on `side`, within `required + near .. required + far`.
-}
type alias CrossRequirement =
    { required : Float
    , side : ToothSide
    , near : Float
    , far : Float
    }


{-| The crossed-mesh requirement for `spec` meshing with `partner`, or
`Nothing` when the two kinds cannot mesh on crossed axles.

The offsets come from the LDraw geometry of the parts (tooth tip radius by z):
a 45° pitch cone whose tips sit one addendum (2.5 LDU) outside gives
`|s| = partner pitch radius + f`.

  - Double bevels (32270, 32269): `f = 5`, either face. This gives the known
    grid layouts 12T/12T each 1 stud from the other axle, 20T/20T each
    1.5 studs, and 12T/20T at 1.5 / 1 stud.
  - Single bevels: 6589 `f = 2`, teeth toward +z; 32198/32198a `f = 2`,
    teeth toward −z; 4143 `f = −1.5`, teeth toward −z (low confidence);
    unknown bevel files `f = 2`, either face. Only equal tooth counts mesh
    (single bevels are 45° 1:1 gears).
  - Crown (3650, 3650b, 3650c): pitch plane at z = +2.5 (face teeth from the
    disc at z = 0 to their tips at z = +5), so `2.5 + spur pitch radius`, on
    the +z side, window −1.0 .. +2.5 so the spur's tips do not dig into the
    disc. The spur sits at the crown's pitch radius (30) from the crown axle,
    either face, ±3.

Crowns with bevels or double bevels, and single with double bevels, are not
allowed (conservative: unverified).

-}
crossRequirement : GearSpec -> GearSpec -> Maybe CrossRequirement
crossRequirement spec partner =
    case ( spec.kind, partner.kind ) of
        ( DoubleBevelGear, DoubleBevelGear ) ->
            Just (bevelRequirement (partner.pitchRadius + 5) EitherSide)

        ( BevelGear, BevelGear ) ->
            if spec.teeth == partner.teeth then
                let
                    ( f, side ) =
                        singleBevelProfile spec
                in
                Just (bevelRequirement (partner.pitchRadius + f) side)

            else
                Nothing

        ( CrownGear, SpurGear ) ->
            Just { required = 2.5 + partner.pitchRadius, side = PlusSide, near = -1.0, far = 2.5 }

        ( SpurGear, CrownGear ) ->
            Just { required = partner.pitchRadius, side = EitherSide, near = -3.0, far = 3.0 }

        _ ->
            Nothing


bevelRequirement : Float -> ToothSide -> CrossRequirement
bevelRequirement required side =
    { required = required, side = side, near = -1.5, far = 2.5 }


{-| Cone offset `f` and tooth side of a single bevel gear, per part file.
-}
singleBevelProfile : GearSpec -> ( Float, ToothSide )
singleBevelProfile spec =
    case spec.partFile of
        "6589.dat" ->
            ( 2.0, PlusSide )

        "32198.dat" ->
            ( 2.0, MinusSide )

        "32198a.dat" ->
            ( 2.0, MinusSide )

        "4143.dat" ->
            ( -1.5, MinusSide )

        _ ->
            ( 2.0, EitherSide )


{-| Reference crossed layouts for two gear parts: the signed offsets of the
axle crossing point along each gear's own local axis, measured from the gear's
origin (`firstOffset` for the first spec, `secondOffset` for the second). A
gear whose teeth may face either way gives one entry per side. Empty when the
two parts cannot mesh on crossed axles.

`Gear.Detect.meshing` and the studio's gear snap both use these, so a snapped
pair is always one the simulation connects.

-}
crossedLayouts : GearSpec -> GearSpec -> List { firstOffset : Float, secondOffset : Float }
crossedLayouts s1 s2 =
    case ( crossRequirement s1 s2, crossRequirement s2 s1 ) of
        ( Just r1, Just r2 ) ->
            List.concatMap
                (\o1 -> List.map (\o2 -> { firstOffset = o1, secondOffset = o2 }) (signedOffsets r2))
                (signedOffsets r1)

        _ ->
            []


signedOffsets : CrossRequirement -> List Float
signedOffsets r =
    case r.side of
        PlusSide ->
            [ r.required ]

        MinusSide ->
            [ -r.required ]

        EitherSide ->
            [ r.required, -r.required ]


sideAccepts : ToothSide -> Float -> Bool
sideAccepts side s =
    case side of
        PlusSide ->
            s > 0

        MinusSide ->
            s < 0

        EitherSide ->
            True


{-| The measurements behind the mesh test for one pair of gears, so a UI can
explain _why_ two gears do or do not mesh using exactly the rules of
`buildGearGraph`.

  - `mode` — how the pair would mesh (see `meshMode`); `Nothing` when the two
    kinds can never mesh
  - `distance` — centre-to-centre distance (LDU)
  - `expected` — the ideal centre distance: the sum of the pitch radii for
    parallel and worm meshes, `sqrt (req1² + req2²)` for crossed meshes (do
    not show this one to users; see `crossOffsets`)
  - `play` — for parallel and worm meshes `distance − expected`; for crossed
    meshes the along-axle deviation (`|s| − required`) of the gear that is
    furthest outside its window
  - `playOk` — `play` (both deviations for crossed meshes) within the window
  - `perpendicular` — the pair needs axles at right angles (crossed or worm)
  - `absDot` — `|axis1 · axis2|` (1 = parallel, 0 = perpendicular)
  - `axialOffset` — offset of the second centre along the first gear's axis
  - `axisOk` — the axis rule for this pair holds (for parallel meshes that is
    parallel axes _and_ the same plane; otherwise right angles)
  - `skew` — distance between the closest points of the two axle lines
    (crossed meshes only, otherwise 0)
  - `axesCross` — the axles cross (`skew <= 2`); always `True` for parallel
    and worm meshes
  - `facesToward` — each gear's bevel or face teeth point toward the
    crossing point; always `True` for parallel and worm meshes
  - `crossOffsets` — for crossed meshes with right-angle axles: each gear's
    signed distance along its own axle from its centre to the crossing point,
    the required distance (unsigned), and how far (LDU) `|s|` lies outside that
    gear's window (0 when inside)

-}
type alias MeshFacts =
    { mode : Maybe MeshMode
    , distance : Float
    , expected : Float
    , play : Float
    , playOk : Bool
    , perpendicular : Bool
    , absDot : Float
    , axialOffset : Float
    , axisOk : Bool
    , skew : Float
    , axesCross : Bool
    , facesToward : Bool
    , crossOffsets :
        Maybe
            { first : Float
            , firstRequired : Float
            , firstExcess : Float
            , second : Float
            , secondRequired : Float
            , secondExcess : Float
            }
    }


{-| Measure a pair of gears against the mesh rules; see `MeshFacts`.
-}
meshFacts : GearInstance -> GearInstance -> MeshFacts
meshFacts g1 g2 =
    let
        dist =
            Vec3.distance g1.worldPosition g2.worldPosition

        pitchSum =
            g1.spec.pitchRadius + g2.spec.pitchRadius

        axis1 =
            gearAxis g1

        axis2 =
            gearAxis g2

        dot =
            Vec3.dot axis1 axis2

        absDot =
            abs dot

        centerDelta =
            Vec3.sub g2.worldPosition g1.worldPosition

        axialOffset =
            abs (Vec3.dot centerDelta axis1)

        mode =
            modeFor g1.spec g2.spec absDot

        distancePlay =
            dist - pitchSum

        distancePlayOk =
            distancePlay >= -meshPlayNear && distancePlay <= meshPlayFar

        base =
            { mode = mode
            , distance = dist
            , expected = pitchSum
            , play = distancePlay
            , playOk = distancePlayOk
            , perpendicular = False
            , absDot = absDot
            , axialOffset = axialOffset
            , axisOk = False
            , skew = 0
            , axesCross = True
            , facesToward = True
            , crossOffsets = Nothing
            }
    in
    case mode of
        Nothing ->
            base

        Just ParallelMesh ->
            { base | axisOk = absDot >= 0.9 && axialOffset <= axialLimitFor g1.spec g2.spec }

        Just WormMesh ->
            { base | perpendicular = True, axisOk = absDot <= 0.35 }

        Just CrossedMesh ->
            let
                axisOk =
                    absDot <= 0.35
            in
            case ( axisOk, crossRequirement g1.spec g2.spec, crossRequirement g2.spec g1.spec ) of
                ( True, Just r1, Just r2 ) ->
                    let
                        -- Closest points of the two axle lines:
                        -- P1 = C1 + s1·a1 and P2 = C2 + s2·a2.
                        w0 =
                            Vec3.sub g1.worldPosition g2.worldPosition

                        d =
                            Vec3.dot axis1 w0

                        e =
                            Vec3.dot axis2 w0

                        den =
                            1 - dot * dot

                        t1 =
                            (dot * e - d) / den

                        t2 =
                            (e - dot * d) / den

                        p1 =
                            Vec3.add g1.worldPosition (Vec3.scale t1 axis1)

                        p2 =
                            Vec3.add g2.worldPosition (Vec3.scale t2 axis2)

                        skew =
                            Vec3.distance p1 p2

                        crossing =
                            Vec3.scale 0.5 (Vec3.add p1 p2)

                        s1 =
                            Vec3.dot (Vec3.sub crossing g1.worldPosition) axis1

                        s2 =
                            Vec3.dot (Vec3.sub crossing g2.worldPosition) axis2

                        dev1 =
                            abs s1 - r1.required

                        dev2 =
                            abs s2 - r2.required

                        excess r dev =
                            if dev < r.near then
                                r.near - dev

                            else if dev > r.far then
                                dev - r.far

                            else
                                0

                        play =
                            if compare ( excess r2 dev2, abs dev2 ) ( excess r1 dev1, abs dev1 ) == GT then
                                dev2

                            else
                                dev1
                    in
                    { base
                        | perpendicular = True
                        , expected = sqrt (r1.required * r1.required + r2.required * r2.required)
                        , play = play
                        , playOk = excess r1 dev1 == 0 && excess r2 dev2 == 0
                        , axisOk = True
                        , skew = skew
                        , axesCross = skew <= maxCrossSkew
                        , facesToward = sideAccepts r1.side s1 && sideAccepts r2.side s2
                        , crossOffsets =
                            Just
                                { first = s1
                                , firstRequired = r1.required
                                , firstExcess = excess r1 dev1
                                , second = s2
                                , secondRequired = r2.required
                                , secondExcess = excess r2 dev2
                                }
                    }

                _ ->
                    { base | perpendicular = True, axisOk = axisOk, playOk = False }


{-| True when two gears mesh: their kinds can mesh, the axis rule holds, and
the placement is within the window for the mesh mode (centre distance for
parallel and worm meshes; crossing axles, tooth sides and along-axle offsets
for crossed meshes).
-}
meshing : GearInstance -> GearInstance -> Bool
meshing g1 g2 =
    if not g1.spec.rotates || not g2.spec.rotates then
        False

    else if isWorm g1.spec && isWorm g2.spec then
        False

    else
        let
            facts =
                meshFacts g1 g2
        in
        facts.mode
            /= Nothing
            && facts.axisOk
            && facts.axesCross
            && facts.facesToward
            && facts.playOk


{-| True when two gears share a physical axle: parallel axes with negligible
radial offset between their centres. Such gears rotate together at 1:1.
-}
coAxial : GearInstance -> GearInstance -> Bool
coAxial g1 g2 =
    if not g1.spec.rotates || not g2.spec.rotates then
        False

    else
        let
            axis1 =
                gearAxis g1

            centerDelta =
                Vec3.sub g2.worldPosition g1.worldPosition

            axialProjection =
                Vec3.dot centerDelta axis1

            radialComponent =
                Vec3.sub centerDelta (Vec3.scale axialProjection axis1)

            radialOffset =
                Vec3.length radialComponent

            absDot =
                abs (Vec3.dot axis1 (gearAxis g2))
        in
        absDot >= 0.99 && radialOffset <= 2.0


gearAxis : GearInstance -> Vec3
gearAxis inst =
    let
        origin =
            Mat4.transform inst.worldMatrix (Vec3.vec3 0 0 0)

        localAxis =
            case inst.spec.axis of
                AxisY ->
                    Vec3.vec3 0 1 0

                AxisZ ->
                    Vec3.vec3 0 0 1

        alongLocalAxis =
            Mat4.transform inst.worldMatrix localAxis

        raw =
            Vec3.sub alongLocalAxis origin

        len =
            Vec3.length raw
    in
    if len < 1.0e-6 then
        if inst.spec.axis == AxisY then
            Vec3.vec3 0 1 0

        else
            Vec3.vec3 0 0 1

    else
        Vec3.scale (1 / len) raw


isWorm : GearSpec -> Bool
isWorm spec =
    spec.kind == WormGear


{-| Single or double bevel gear.
-}
isBevelLike : GearSpec -> Bool
isBevelLike spec =
    spec.kind == BevelGear || spec.kind == DoubleBevelGear


isCrownLike : GearSpec -> Bool
isCrownLike spec =
    spec.kind == CrownGear


addConnection : GearId -> GearId -> Dict GearId (List GearId) -> Dict GearId (List GearId)
addConnection from to dict =
    Dict.update from
        (\existing ->
            case existing of
                Nothing ->
                    Just [ to ]

                Just xs ->
                    Just (to :: xs)
        )
        dict



-- ── Spatial indexing (pair enumeration only) ──────────────────────────────────


prependIndex : Int -> Maybe (List Int) -> Maybe (List Int)
prependIndex idx existing =
    Just (idx :: Maybe.withDefault [] existing)


orderPair : Int -> Int -> ( Int, Int )
orderPair a b =
    if a < b then
        ( a, b )

    else
        ( b, a )


{-| The 27 neighbour offsets (including the zero offset) of a 3D grid cell.
-}
neighbourOffsets : List ( Int, Int, Int )
neighbourOffsets =
    let
        steps =
            [ -1, 0, 1 ]
    in
    List.concatMap
        (\dx -> List.concatMap (\dy -> List.map (\dz -> ( dx, dy, dz )) steps) steps)
        steps


{-| Gather gear indices from the 27 cells surrounding a meshing-hash cell.
-}
meshNeighbourIndices : ( Int, Int, Int ) -> Dict ( Int, Int, Int ) (List Int) -> List Int
meshNeighbourIndices ( cx, cy, cz ) buckets =
    List.concatMap
        (\( dx, dy, dz ) ->
            Dict.get ( cx + dx, cy + dy, cz + dz ) buckets
                |> Maybe.withDefault []
        )
        neighbourOffsets


{-| Cell size for the canonical-axis-direction dimension of the axle hash. A
co-axial pair may differ in axis direction by up to `acos 0.99 ≈ 8°`
(per-component difference ≤ 0.14); this cell plus the ±1 sweep captures that.
-}
axleDirCell : Float
axleDirCell =
    0.2


{-| Cell size for the closest-point dimension of the axle hash. `coAxial`
requires a radial offset ≤ 2.0, so co-axial gears' closest points differ by at
most that (plus small axis-tilt wobble); this cell plus the ±1 sweep captures it.
-}
axleQCell : Float
axleQCell =
    3.0


{-| The axle-line bucket key for a gear: its canonical axis direction and the
closest point on that axis to the origin, each quantized. Two gears sharing an
axle map to the same (or an adjacent) bucket regardless of axial separation.
-}
axleKey : GearInstance -> ( ( Int, Int, Int ), ( Int, Int, Int ) )
axleKey g =
    let
        a =
            canonicalAxis (gearAxis g)

        p =
            g.worldPosition

        closest =
            Vec3.sub p (Vec3.scale (Vec3.dot p a) a)
    in
    ( ( floor (Vec3.getX a / axleDirCell)
      , floor (Vec3.getY a / axleDirCell)
      , floor (Vec3.getZ a / axleDirCell)
      )
    , ( floor (Vec3.getX closest / axleQCell)
      , floor (Vec3.getY closest / axleQCell)
      , floor (Vec3.getZ closest / axleQCell)
      )
    )


{-| Gather gear indices from the buckets neighbouring an axle-hash bucket,
sweeping ±1 in each of the six quantized dimensions.
-}
axleNeighbourIndices :
    ( ( Int, Int, Int ), ( Int, Int, Int ) )
    -> Dict ( ( Int, Int, Int ), ( Int, Int, Int ) ) (List Int)
    -> List Int
axleNeighbourIndices ( ( ax, ay, az ), ( qx, qy, qz ) ) buckets =
    List.concatMap
        (\( dax, day, daz ) ->
            List.concatMap
                (\( dqx, dqy, dqz ) ->
                    Dict.get
                        ( ( ax + dax, ay + day, az + daz )
                        , ( qx + dqx, qy + dqy, qz + dqz )
                        )
                        buckets
                        |> Maybe.withDefault []
                )
                neighbourOffsets
        )
        neighbourOffsets


{-| Canonicalize an axis direction so that `a` and `-a` map to the same value:
flip the sign so the first non-negligible component is positive. `coAxial`
already treats the two directions as equivalent (via `absDot`), so this collapses
them into one bucket.
-}
canonicalAxis : Vec3 -> Vec3
canonicalAxis a =
    let
        x =
            Vec3.getX a

        y =
            Vec3.getY a

        z =
            Vec3.getZ a

        eps =
            1.0e-6
    in
    if x > eps then
        a

    else if x < -eps then
        Vec3.scale -1 a

    else if y > eps then
        a

    else if y < -eps then
        Vec3.scale -1 a

    else if z >= 0 then
        a

    else
        Vec3.scale -1 a


{-| Recursive part-tree walk. Accumulates gear instances (without final IDs —
those are assigned after the walk in `extractGears`).
-}
walkLines : List GearSpec -> List LDrawLine -> PartCache -> Int -> Mat4 -> List GearInstance -> List GearInstance
walkLines gearSpecs lines cache parentColor worldMat acc =
    List.foldl (walkLine gearSpecs cache parentColor worldMat) acc lines


walkLine : List GearSpec -> PartCache -> Int -> Mat4 -> LDrawLine -> List GearInstance -> List GearInstance
walkLine gearSpecs cache parentColor worldMat line acc =
    case line of
        SubFileRef { file, transform, color } ->
            let
                combinedMat =
                    Mat4.mul worldMat transform

                childColor =
                    if color == 16 || color == -1 then
                        parentColor

                    else
                        color
            in
            case matchGear gearSpecs file of
                Just spec ->
                    -- This sub-file is a known gear — record it
                    let
                        worldPos =
                            Mat4.transform combinedMat (Vec3.vec3 0 0 0)

                        inst =
                            { id = 0 -- placeholder; set after walk
                            , spec = spec
                            , color = childColor
                            , worldPosition = worldPos
                            , worldMatrix = combinedMat
                            }
                    in
                    inst :: acc

                Nothing ->
                    -- Not a gear — recurse into its sub-parts if cached
                    case Dict.get file cache of
                        Just (Loaded subLines) ->
                            walkLines gearSpecs subLines cache childColor combinedMat acc

                        _ ->
                            acc

        _ ->
            acc


matchGear : List GearSpec -> String -> Maybe GearSpec
matchGear gearSpecs file =
    gearSpecs
        |> List.filter (\spec -> spec.partFile == file)
        |> List.head
