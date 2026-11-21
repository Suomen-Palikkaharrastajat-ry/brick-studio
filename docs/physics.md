# Physics

The simulator models a gear train as a directed graph and computes all
gear angles as a **deterministic function of time**, not as an incremental
integrator. This makes playback scrubbing exact — seeking to any time `t` gives
the same result regardless of how you got there.

All physics logic lives in `packages/brick-frame-simulator/src/Gear/`.

## Deterministic angle model

```
motorAngle(t) = motor.speedRadPerSec × t

gearAngle(id, t) = ratio(id) × motorAngle(t)
```

`t` is the accumulated simulation time in seconds (`PlaybackState.currentTime`).
The per-gear ratio is computed once by BFS through the gear graph and reused
every frame. `Gear.Animate.gearAngles` is the entry point:

```elm
gearAngles : MotorState -> GearGraph -> Float -> Dict GearId Float
gearAngles motor graph totalTime =
    let motorAngle = motor.speedRadPerSec * totalTime
    in  Physics.propagate graph motorId motorAngle
```

## Gear detection

`Gear.Detect.extractGears` walks the LDraw part tree depth-first — the same
recursive traversal used by geometry flattening — and records every `SubFileRef`
whose normalised filename matches an entry in `Data.gearParts`. The world
matrix is accumulated through the call stack so that parts nested inside
sub-assemblies are found correctly.

`Data.gearParts` (generated from `knownGears` in `src/Lib.hs`) contains all
Technic gears and worms in the catalog's Technic category, plus the moved-file
aliases used by older LDraw models. Each `GearSpec` stores a positive tooth
count, pitch radius, explicit `GearKind`, local rotation `GearAxis`, motor and
rotation eligibility, and typed pin/axle-hole centers. For ordinary gears, the
`teeth` metadata is the tooth count; for worms, it is the screw-start count.
The gear kind—not a tooth-count sentinel—selects worm, bevel, or crown behavior.
The LDraw part name for `32905.dat` is “Technic Worm Gear 2L with Axle Hole
Two-toothed Sliding”, but its geometry reuses the one-start thread subpart from
`4716.dat`. Accordingly, both worm specs have `teeth = 1` for ratio math; the
LDraw name describes the sliding tooth profile, not two screw starts.

Moved aliases `3648.dat`, `32198.dat`, and `3650.dat` remain registered with
their replacements `3648b.dat`, `32198a.dat`, and `3650c.dat`; `3650b.dat` is
also retained as the earlier crown-gear variant. `18938.dat` is the 60T
turntable top. The 28T `99010.dat` turntable top and 60T `18938.dat` top are
rotatable and motor-capable. `99009.dat` is the fixed 28T turntable base: it is
recognized for catalog completeness and hole/connectivity metadata, but is
neither rotatable nor a motor candidate and therefore cannot form a moving
mesh edge. The 18938/99009/99010 axes are local +Y; ordinary gears use local
+Z. Hole coordinates are offsets in the corresponding local gear plane.

The bevel counts are `32198`/`32198a` = 20T, `32269` = 20T, `32270` = 12T,
`4143` = 14T, and `6589` = 12T. The catalog's `32198` title omits its count;
the replacement geometry is explicitly registered as 20T. `32270` and `32269`
are double bevel gears (`DoubleBevelGear`): their teeth work as a spur gear on
parallel axles and as a bevel gear at right angles. `6589`, `4143` and
`32198`/`32198a` are single bevels (`BevelGear`) and mesh only at right angles.

Pitch radii are `teeth × 1.25` LDU (module 1 mm = 2.5 LDU; worms, which have
one start, use 10), checked against
the outer tooth radius measured from the LDraw geometry (one addendum, about
2.5 LDU, outside the pitch circle): 8T 12.45 / 10, 12T `69778` 17.71 / 15,
24T 32.40 / 30, 40T 52.36 / 50, 12T double bevel 17.01 / 15, 20T double bevel
27.01 / 25, crown 32.98 / 30, worm 12.70 / 10.

The full tooth counts, kinds, axes, and hole positions are the executable
reference in `src/Lib.hs`; `packages/brick-frame-data/src/Data.elm` is its
generated mirror.

Each detected gear becomes a `GearInstance`:

```elm
type alias GearInstance =
    { id            : GearId    -- zero-based index
    , spec          : GearSpec  -- partFile, teeth, pitchRadius
    , worldPosition : Vec3      -- axle centre, Y-up world space
    , worldMatrix   : Mat4      -- full 4×4 world transform
    }
```

## Adjacency graph

`Gear.Detect.buildGearGraph` uses spatial indexes to enumerate possible
meshing and co-axial pairs, then applies the exact tests below. Fixed gear
features (`rotates = False`) do not form moving mesh edges. Two rotatable gears
are considered **meshing** according to their mesh mode (`Gear.Detect.meshMode`):

| Mode | Kinds |
|------|-------|
| `ParallelMesh` | any pair of spur, double bevel, crown (its rim teeth) and turntable gears |
| `CrossedMesh` | crown + spur; double bevel + double bevel; single bevel + single bevel with the same tooth count |
| `WormMesh` | a worm and any other rotating gear |

Kinds that allow both a parallel and a crossed mesh (double bevel pairs, crown
with spur) use the parallel mode when `absDot(axis1, axis2) ≥ 0.5`. Other pairs
(single bevel with a spur or a double bevel, crown with a bevel or double bevel,
single bevels with different tooth counts, two worms) never mesh. Crown with a
bevel or double bevel and single with double bevels are rejected
conservatively: they are not verified against real models.

### Parallel and worm meshes: distance window

```
play = distance(g1.worldPosition, g2.worldPosition) − (g1.pitchRadius + g2.pitchRadius)
−1.25 ≤ play ≤ +2.02   (meshPlayNear, meshPlayFar)
```

All LEGO gears share module 1 mm (2.5 LDU), so the allowed play is absolute,
not relative to the gear size. It is asymmetric because gears that are too close
bind, while gears that are too far only have more backlash. On the stud/plate
grid (20 / 8 LDU) the window accepts every layout from −1.18 (16T + 40T,
2 studs × 7 plates) to +2.00 (for example 8T + 16T 4 plates straight up, where
the 16T's small addendum still leaves about 2 LDU of tooth overlap) and rejects
the next ones out (−1.92, 12T + 24T 2 studs × 2 plates, and +2.05, 12T + 40T
1 stud × 8 plates). Examples newly accepted compared with the
old relative tolerance (`clamp 0.8 2.0 (expected × 0.035)`): 12T + 24T 2 studs ×
3 plates (+1.65), 8T + 8T 1 stud × 1 plate (+1.54), 8T + 16T / 12T + 12T 1 stud ×
3 plates (+1.24), 8T + 12T 3 plates up (−1.00), worm + 12T 1 brick apart (−1.00,
`wheeler.ldr`). Newly rejected: 16T + 40T 3 studs × 4 plates (−2.00, binds),
40T + 40T 1 stud × 12 plates (−1.94), 12T + 40T studless 3 × 1 (−1.75), and on
half-stud lattices a few tight layouts such as 12T + 40T at (30, 56) (−1.47),
40T + 40T at (90, 40) (−1.51) and 24T + 40T at (30, 72) (−2.00).

Parallel meshes also need `absDot ≥ 0.9` and an axial offset (of the second
centre along the first gear's axle) of at most 2 LDU, or 3 LDU when a crown is
involved (its rim teeth span about z −5 … +4.6). Worm meshes need
`absDot ≤ 0.35` and keep the skew layout of a real worm drive (the wheel at the
sum of the pitch radii from the worm, on the common normal).

### Crossed meshes: crossing axles

1. `absDot(axis1, axis2) ≤ 0.35`.
2. The closest points of the two axle lines, `P1 = C1 + s1·a1` and
   `P2 = C2 + s2·a2`, are at most 2 LDU apart (the axles cross; skew layouts
   are rejected). The crossing point is `O = (P1 + P2) / 2`.
3. For each gear, `s` is the signed position of `O` along its own local axle
   from its origin. Its sign must match the side its teeth face, and `|s|` must
   be within a window of the required value:

| Gear | Parts | Tooth side | Required offset (abs s) | Window on (abs s − required) |
|------|-------|------------|----------------|----------------------------|
| Double bevel | `32270`, `32269` | either | partner pitch radius + 5 | −1.5 … +2.5 |
| Single bevel | `6589` | +z | partner pitch radius + 2 | −1.5 … +2.5 |
| Single bevel | `32198`, `32198a` | −z | partner pitch radius + 2 | −1.5 … +2.5 |
| Single bevel | `4143` (low confidence) | −z | partner pitch radius − 1.5 | −1.5 … +2.5 |
| Crown | `3650`, `3650b`, `3650c` | +z | 2.5 + spur pitch radius | −1.0 … +2.5 |
| Spur on a crown | any spur | either | crown pitch radius (30) | ±3 |

The offsets come from the tooth tip radius by z of the LDraw geometry: a 45°
pitch cone whose tips sit one addendum outside gives
`|s| = partner pitch radius + f`. For the double bevels, `f = 5` matches the
known grid layouts (12T + 12T each 1 stud from the other axle, 20T + 20T each
1.5 studs, 12T + 20T at 1.5 / 1 stud). The single-bevel values are fitted the
same way but have not been checked against real models. The crown's face teeth
run from the disc at z = 0 to their tips at z = +5, so its pitch plane is at
z = +2.5; the spur sits on the crown's pitch circle.

`Gear.Detect.crossedLayouts` lists the reference offsets for a pair of parts;
the studio's gear snap places crossed gears with them, so a snapped pair always
satisfies `Detect.meshing`. `Detect.meshFacts` reports the measurements behind
each rule (`mode`, `play`, `skew`, `axesCross`, `facesToward`,
`crossOffsets`), which `Studio.GearFeedback` uses to explain a failed mesh.

The mesh rule also decides which overlaps the build health check ignores
(`Studio.Health`, meshing gears legitimately overlap their pitch circles), so
newly rejected layouts can show up as overlaps there.

## Rotation propagation

Rotating gears are rendered about their true axis (the Y-up image of the
`GearSpec.axis` through the world matrix), never a canonicalised sign, so a
gear's rendered angle and the angle the physics computes refer to the same
axis. Fixed parts such as the 99009 base remain in the static scene and are
excluded from both mesh and rigid-axle propagation.

`Gear.Physics.propagate` performs a BFS from the motor gear through the
adjacency graph. At each edge the ratio is updated:

```elm
meshRatio g1 g2 =
    if g1.kind == WormGear || g2.kind == WormGear then
        toFloat g1.teeth / toFloat g2.teeth * wormMeshSign g1 g2
    else
        toFloat g1.teeth / toFloat g2.teeth * meshSign g1 g2
```

The magnitude is the ratio of the metadata counts: ordinary gears contribute
their tooth count, while worms contribute their screw-start count. For
non-worm pairs, the sign is the no-slip sign at the pitch point `P`. With
`v_X = n_X × (P − C_X)` (`n_X` the gear's axis, `C_X` its centre), the signed
ratio is `k = (v_1·v_2) / |v_2|²`, and `meshSign` is its sign. Two spur gears
with the same axis orientation give `−teeth1/teeth2`. Flipping one gear's axis
gives a positive ratio. Crown and bevel pairs take their sign from the same
formula. If the geometry is degenerate (coincident centres, no pitch point), a
non-worm `meshSign` falls back to `−1`.

Worm contact has no single physically meaningful pitch point for the spur-style
no-slip formula. Both checked-in worm parts reference the same local LDraw
thread geometry. In LDraw local coordinates it is right-handed: angular phase
increases about local +Z as position increases along local +Z. For positive
rotation around the oriented worm axis, a right-handed screw's thread moves
axially toward `−wormAxis`. The sign used by
the idealized worm model is therefore
`sign((wormAxis × wheelAxis) · (wheelCentre − wormCentre))`: the +X worm, +Z
wheel, +Y center-side arrangement has a negative ratio. Flipping either
oriented axis or moving the wheel to the opposite side reverses the sign.

LDraw's Y-down to engine Y-up conversion negates both Y and Z, a proper 180°
rotation about X, so it preserves handedness; the formula uses the transformed
world axes. This is a kinematic inference from the LDraw helicoid and idealized
contact directions, not empirical validation of the molded parts. A degenerate
oriented triple product has no determined physical sign and falls back to +1.
Worm graph edges remain one-way from worm to wheel, so wheels cannot back-drive
worms.

Co-axial (rigid) neighbours share one axle, so they turn with the same
angular velocity. Each gear's angle is measured about its own true axis, so
the ratio is `+1` when the two axes point the same way and `-1` when one gear
is flipped (`Gear.Physics.rigidSign`).

**Example:** Motor on gear A (8T) drives gear B (24T), same orientation:
- `ratio_B = -(8/24) = −0.333`
- Gear B rotates at 1/3 the motor speed, in the opposite direction.

If gear B also drives gear C (8T), same orientation:
- `ratio_C = ratio_B × -(24/8) = (−0.333) × (−3) = 1.0`
- Gear C rotates at the same speed and direction as the motor.

Gears not reachable from the motor are absent from the result dict.
Cycles are handled by the BFS visited set — each gear appears at most once.
When no motor was explicitly requested, the simulator chooses the
motor-capable rotating gear with the largest directed BFS-reachable set. Ties
are resolved by original document order; explicit user motor selections are
preserved.

## Tooth phase

`Gear.Phase` answers where the teeth of a gear sit at a mesh, so that a
gear dropped next to another can be turned until teeth interlock. It is a
pure geometry module and is not used by `propagate`.

- Every spur, crown and bevel gear has tooth 0 on its local +X (`phaseOf` is 0),
  except bevel `32198.dat`, whose measured value (~+0.8°) is not trusted and is
  also treated as 0. Worms (`GearKind = WormGear`) have no phase (`Nothing`) and
  are never aligned; the kind, not a tooth-count sentinel, identifies worms.
- `contactPoint` gives the pitch point `P` by mesh mode: on the centre line at the
  first gear's pitch radius for parallel meshes (including crowns and double
  bevels on parallel axles), or on the rim gear's pitch circle toward the other
  gear for crossed meshes (the crown when there is one, otherwise the first
  gear). Worms and pairs that cannot mesh have none.
- `residual gear P Tooth|Gap` is the signed offset (radians, within half a pitch)
  of the nearest tooth from the target. A tooth targets `beta(P)`; a gap targets
  `beta(P) + pi/N`. `toothFraction` divides by the pitch, so `0.5` is half a tooth off.
- Rotating a gear about its own `+axis` by `d` changes its residual by `+d`, so
  `meshCorrection {fixed, moving}` returns `d = −residual` for the option that
  leaves the fixed gear best aligned. `groupCorrection` does a min-max
  over several meshes on one rigid axle.
- **Tie-break rule.** A fixed partner's offset (`fixedFraction`) cannot be changed
  by turning the group, so the rotation minimises only the moving-side residuals.
  The fixed offset is reported as `Correction.fixedFloor`. Option combinations are
  ranked by `max(fixedFloor, worstFraction)`, then by `|radians|`, then by the
  smaller `fixedFloor`. Without this, a fixed offset that is the same for every
  rotation made all candidates tie and the zero rotation won, so a new mesh stayed
  open. A single mesh still picks the option whose fixed side is aligned, because
  its floor is part of the score.

Rotation changes only the rigid group (gears and axles on the same axis line).
Re-seating a gear on an axle in 90° steps never changes its tooth phase for the
standard gears (tooth counts divisible by 4).

## Motor and playback

`Gear.Animate.MotorState` selects which gear the motor drives:

```elm
type alias MotorState =
    { drivingGearId  : Maybe GearId
    , speedRadPerSec : Float          -- default 1.0 rad/s
    , running        : Bool
    }
```

The first detected gear is auto-selected as motor when a model loads. The user
can click any gear to reassign the motor.

`PlaybackState` in `Simulation` controls the timeline:

```elm
type alias PlaybackState =
    { running          : Bool
    , currentTime      : Float   -- seconds
    , maxTime          : Float
    , speedMultiplier  : Float
    , motorGearId      : Maybe GearId
    , motorSpeedRadPerSec : Float
    }
```

`currentTime` increments each animation frame by `dt × speedMultiplier` while
running, and stops at `maxTime` (auto-pause).

## Snapshot flow

Analysis runs once per load. When `Viewport` reports `PartsReady`, the host calls
`Simulation.analyse` with the top-level lines and part cache; that detects gears
and components, builds the graph, and returns the lines the viewport should bake
as the static scene (everything that is not a gear or riding on one). Moving
parts are drawn separately by `Simulation.entities`. The studio's preview uses the
same flow on a snapshot of the document (`Writer.writeDocument`) in a fresh
`Viewport` + `Simulation`, so editing never disturbs the running simulation.

## Driven axles and parts

An axle a gear sits on turns with that gear, and so does any other top-level
part threaded onto the gear's axis. `Simulation.analyse` finds them as
*driven parts*: an axle (an axle-like component) whose centre lies within 2.5 LDU
of a gear's axis line with the same axis direction (`componentDrivingGear`), or a
non-gear, non-axle top-level part that `topLevelCoaxialGear` matches to a gear
(for parts it also accepts the rotational connectors of the part). Each one is
rotated by its driving gear's angle about the gear's true axis. A part whose own
axis points against the gear's is given the flipped axis (`alignToGear` in
`Simulation.Meshes`), so the same angle turns it the same way as the gear.

- `Simulation.drivenPartsFor` lists the driven parts (file and transform) of the
  given gears, so a host can find their placements.
- `Simulation.drivenEntitiesFor` draws only those parts, turning. Use it with
  `gearEntitiesFor` and hide the same placements from the host's static scene.
  The viewer keeps using `Simulation.entities`, which draws every axle and part.

Brick Play in Technic mode places gears and axles on its axle line (Y −60). A gear turned
to the axle's direction (along Z for a Z-axis axle, or along X for the default
axle) is driven when its centre lies within 2.5 LDU of the axle's line, measured
from the axle's origin (the middle of the axle).

## Click-to-select motor gear

On `MouseUp`, if the total mouse drag was < 3 px, the host (via `Simulation.pickGear`) fires a ray-sphere
intersection test against every gear's bounding sphere (radius = `pitchRadius`).
The nearest intersected gear is selected as the new motor gear.

## Limitations

- **Co-axial rotation is top-level only.** `topLevelCoaxialGear` in `Simulation.Meshes`
  detects non-gear, non-component `SubFileRef` nodes in the root file that share
  an axle with a detected gear (parallel axis, centre within 2.5 LDU of the
  gear's axle line). Those parts are excluded from the static scene and rendered
  with the same rotation angle as their driving gear (see "Driven axles and
  parts" above). Parts nested inside sub-assemblies are not detected and remain
  static.
- **No torque or force model.** All gears spin freely regardless of load. There is
  no simulation of mechanical advantage, torque multiplication, or stalling.
- **Worm drives are modelled as self-locking.** The gear graph only has the
  worm → wheel edge, so turning the wheel (for example by choosing it as the
  motor) never turns the worm. No friction or efficiency is modelled beyond
  that.
- **Pitch radii follow module 1 mm.** The values in `Data.gearParts` are
  `teeth × 1.25` LDU (worms: 10) and were checked against the measured outer tooth radius of
  the LDraw geometry (see "Gear detection"). The crossed-mesh offsets of the
  single bevels and the crown pitch plane are fitted from the same geometry and
  are less certain than the spur and double-bevel values.
- **No support for rack-and-pinion or differential gears.** These part types are
  not in the known gear list and will be ignored during detection.
