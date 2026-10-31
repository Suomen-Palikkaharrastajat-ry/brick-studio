# Brick Studio (Palikkakehys Studio)

Brick Studio is a browser-based 3D LEGO® CAD editor, WebGL renderer, and gear train physics simulator developed by Suomen Palikkaharrastajat ry. It serves as both the full-featured desktop Studio application and the upstream monorepo hosting the core `brick-frame-*` Elm packages.

## Architecture

Brick Studio is organized into an application and a suite of shared Elm packages:

```
brick-studio/
├── studio-app/                         # Desktop 3D CAD application (Vite + Elm)
├── packages/
│   ├── brick-frame-data/               # LDraw color tables and embedded part definitions
│   ├── brick-frame-simulation/         # Physics simulation loop and kinematics
│   ├── brick-frame-simulator/          # LDraw document parsing, formatters, BOM, gear math
│   ├── brick-frame-studio/             # Studio editor state, snapping, and toolbars
│   ├── brick-frame-ui/                 # Reusable UI widgets and styling
│   └── brick-frame-viewport/           # WebGL rendering engine and runtime worker
├── generator/                          # Haskell code generator for Elm Data & ColorData
├── src/                                # Haskell generation library (Lib.hs)
├── tests/                              # Haskell unit test suite (HSpec)
├── ldraw/LDConfig.ldr                  # Official LDraw color definitions table
└── vendor/master-builder/              # Submodule providing design tokens and UI components
```

## Quick Start

The development environment is managed using [devenv](https://devenv.sh) (Nix-based):

```bash
# Enter development shell
devenv shell

# Initialize git submodules & symlinks
make vendor

# Run tests across shared packages
make test-lib

# Start the Studio Vite dev server (http://localhost:5173)
make dev

# Run all test suites (Haskell generator, Studio app, shared packages)
make test
```

## Core Make Targets

- `make dev` — Start Vite dev server for `studio-app`
- `make build` — Production build of `studio-app` → `build/studio/`
- `make test-lib` — Run unit tests for all shared `brick-frame-*` Elm packages
- `make test` — Run Haskell tests, studio tests, and package tests
- `make check` — Check Elm formatting, review rules, and Haskell linting
- `make format` — Auto-format Elm and Haskell source code
- `make generate` — Run the Haskell generator to generate `Data.elm` and `ColorData.elm`

## Licensing and Trademarks

- Source code is licensed under the MIT License (see [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md)).
- LEGO® is a trademark of the LEGO Group of companies, which does not sponsor, authorize, or endorse this project.
- LDraw™ is a trademark owned and licensed by the Estate of James Jessiman.
- LDraw Parts Library content is redistributable under Creative Commons Attribution (CC BY 2.0 / CC BY 4.0).
