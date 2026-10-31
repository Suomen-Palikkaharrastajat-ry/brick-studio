# AGENTS.md — Brick Studio

Browser-based 3D LEGO® CAD Studio and core package suite built with Elm + WebGL. Loads LDraw `.ldr`/`.mpd` files, provides interactive 3D modeling, snapping, collision detection, SNOT candidate search, build health diagnostics, gear train detection, and physical simulation.

## Project Structure

```
brick-studio/
├── studio-app/                         # Desktop 3D CAD application (Vite + Elm)
│   ├── src/
│   │   ├── Main.elm                    # Studio pages: Workspace & Preview
│   │   └── Studio/                     # Workspace, Preview, Instructions, Inspector, Palette, BOM
│   ├── main.js                         # JS initialization and ports
│   ├── main.css                        # Studio Tailwind styles
│   ├── index.html                      # Studio entrypoint
│   └── vite.config.mjs                 # Vite configuration
│
├── packages/
│   ├── brick-frame-data/               # Generated LDraw data (Data.elm, EmbeddedParts.elm)
│   ├── brick-frame-simulation/         # Physics simulation loop and kinematics
│   ├── brick-frame-simulator/          # LDraw AST parser, exporter, format, BOM, gear math
│   │   └── src/
│   │       ├── LDraw/                  # Types, Parser, Resolve, Colors, Document, Export
│   │       ├── Render/                 # Camera, Mesh, Shader, Pick, Style
│   │       └── Gear/                   # Joint, Physics, Detect, Animate, Phase
│   ├── brick-frame-studio/             # Reusable 3D studio TEA component
│   │   └── src/Studio/
│   │       ├── Core.elm                # Document state, selections, undo/redo
│   │       ├── DragPlane.elm           # View-relative 3D dragging & snapping
│   │       ├── SideSnap.elm            # SNOT (studs not on top) approach matrix
│   │       ├── Health.elm              # Build health diagnostics & overlap resolution
│   │       └── GearAlign.elm           # Dynamic gear meshing tooth alignment
│   ├── brick-frame-ui/                 # Design tokens, CameraGlobe, PartCard, theme.css
│   └── brick-frame-viewport/           # WebGL rendering engine and runtime worker
│
├── generator/Main.hs                   # Haskell generator for Data.elm and ColorData.elm
├── src/Lib.hs                          # Haskell library: LDrawColor parsing & Elm code generators
├── tests/Main.hs                       # Haskell HSpec test suite
├── ldraw/LDConfig.ldr                  # Official LDraw colour definitions table
├── vendor/master-builder/              # Submodule: design tokens & ui-components
└── docs/                               # Architecture and specification docs
```

## Key Technical Specifications

- **LDraw Coordinate System:** Right-handed with **Y down** (1 LDU = 0.4 mm; stud pitch = 20 LDU; plate height = 8 LDU; brick height = 24 LDU). In Elm WebGL rendering, Y is inverted to match standard 3D coordinates.
- **Snapping & SNOT Matrix:** `Studio.DragPlane` and `Studio.SideSnap` compute magnetic snap candidates across stud-to-antistud, clip-to-bar, axle-to-hole, and lateral SNOT orientations.
- **Collision Detection:** Oriented Bounding Box (OBB) Separating Axis Theorem (SAT) prevents illegal overlaps during interactive placement.
- **Physical Kinematics:** Gear train graphs are evaluated dynamically via breadth-first search (BFS) ratio propagation (`Gear.Physics`).
- **Color Finishes:** Parsed from `LDConfig.ldr` via Haskell generator into `LDraw.ColorData.Finish` (Solid, Transparent, Chrome, Metal, Pearl, Rubber, Glitter, Speckle).

## Development & Verification Commands

All workflows run within `devenv shell`:

```bash
# Run unit tests across all 5 shared packages (simulator, viewport, simulation, ui, studio)
devenv shell -- make test-lib

# Run all test suites (Haskell generator, studio-app, and shared packages)
devenv shell -- make test

# Validate formatting and elm-review rules
devenv shell -- make check

# Auto-format Elm and Haskell source code
devenv shell -- make format

# Launch Studio dev server
devenv shell -- make dev
```

## Guidelines for Agents

- Maintain documentation integrity and preserve existing docstrings and comments.
- Do not edit generated modules directly (`packages/brick-frame-data/src/Data.elm` and `packages/brick-frame-simulator/src/LDraw/ColorData.elm`). Instead, update `src/Lib.hs` or `generator/Main.hs` and run `make generate`.
- When modifying shared packages, always verify backwards compatibility by running `make test-lib`.
