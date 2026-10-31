HLINT ?= hlint
FOURMOLU ?= fourmolu
GENERATOR_NIX ?= generator-nix
VITE ?= vite --configLoader native
WATCH_PORT ?= 5173
PREVIEW_PORT ?= 4173

.PHONY: help
help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# ── Vendor / submodules ──────────────────────────────────────────────────────

.PHONY: vendor
vendor: ## Init and update all git submodules to their pinned commits
	@[ -z "$$CI" ] || git config --global url."https://github.com/".insteadOf "git@github.com:"
	@if [ -d .git ]; then git submodule update --init; elif [ ! -d vendor/master-builder ]; then mkdir -p vendor && git clone https://github.com/Suomen-Palikkaharrastajat-ry/master-builder.git vendor/master-builder; fi
	ln -sfn ../vendor/master-builder/packages studio-app/packages
	ln -sfn ../../../vendor/master-builder/review/src/LlmAgent studio-app/review/src/LlmAgent

# ── Development environment ──────────────────────────────────────────────────

.PHONY: shell
shell: ## Enter devenv shell
	devenv shell

.PHONY: develop
develop: devenv.local.nix devenv.local.yaml ## Bootstrap devenv shell + VS Code
	devenv shell --profile=devcontainer -- code .

# ── Sources ──────────────────────────────────────────────────────────────────

HS_SOURCES := $(shell find src generator -name '*.hs') brick-studio.cabal $(wildcard cabal.project*)
SHARED_ELM_SOURCES := $(shell find packages/brick-frame-viewport/src packages/brick-frame-simulation/src packages/brick-frame-simulator/src packages/brick-frame-ui/src packages/brick-frame-studio/src -name '*.elm' ! -name 'ColorData.elm')
STUDIO_APP_SOURCES := $(shell find studio-app/src -name '*.elm') $(SHARED_ELM_SOURCES) packages/brick-frame-data/src/EmbeddedParts.elm
VIEWPORT_JS_SOURCES := $(shell find packages/brick-frame-viewport/runtime -name '*.js') packages/brick-frame-viewport/geometry-worker.js
SHARED_PACKAGE_DIRS := packages/brick-frame-data/src/EmbeddedParts.elm packages/brick-frame-viewport packages/brick-frame-simulation packages/brick-frame-ui packages/brick-frame-studio packages/brick-frame-simulator/src/LDraw/Document.elm packages/brick-frame-simulator/src/LDraw/Export.elm packages/brick-frame-simulator/src/LDraw/Export packages/brick-frame-simulator/src/LDraw/Format.elm packages/brick-frame-simulator/src/LDraw/Writer.elm packages/brick-frame-simulator/src/Render/Pick.elm packages/brick-frame-simulator/src/Render/Shader.elm packages/brick-frame-simulator/src/Render/Style.elm packages/brick-frame-simulator/tests/LDraw/DocumentTest.elm packages/brick-frame-simulator/tests/LDraw/ExportTest.elm packages/brick-frame-simulator/tests/LDraw/Fixtures.elm packages/brick-frame-simulator/tests/Render/PickTest.elm packages/brick-frame-simulator/tests/Render/StyleTest.elm packages/brick-frame-simulator/src/LDraw/Flex.elm packages/brick-frame-simulator/src/LDraw/Steps.elm packages/brick-frame-simulator/src/Gear/Joint.elm packages/brick-frame-simulator/src/Gear/Physics.elm packages/brick-frame-simulator/src/LDraw/Connectivity.elm packages/brick-frame-simulator/src/LDraw/ColorAvailability.elm packages/brick-frame-simulator/tests/LDraw/StepsTest.elm packages/brick-frame-simulator/tests/LDraw/FlexTest.elm packages/brick-frame-simulator/tests/LDraw/ConnectivityTest.elm packages/brick-frame-simulator/tests/LDraw/ColorAvailabilityTest.elm packages/brick-frame-simulator/tests/Gear/JointTest.elm packages/brick-frame-simulator/tests/Gear/PhysicsTest.elm packages/brick-frame-simulator/src/LDraw/Catalog.elm packages/brick-frame-simulator/src/LDraw/BOM.elm packages/brick-frame-simulator/tests/LDraw/BOMTest.elm
ELM_PACKAGE_SOURCES := $(shell find vendor/master-builder/packages -name '*.elm' -o -name '*.css' 2>/dev/null)

brick-data-generator: $(HS_SOURCES)
	cabal build generator
	cp $$(cabal list-bin generator) $@

GENERATED_ELM := packages/brick-frame-data/src/Data.elm packages/brick-frame-simulator/src/LDraw/ColorData.elm

.PHONY: generate
generate: packages/brick-frame-data/src/Data.elm ## Run Haskell generator to produce generated Elm data modules

packages/brick-frame-data/src/Data.elm: brick-data-generator ldraw/LDConfig.ldr
	./brick-data-generator
	elm-format --yes $(GENERATED_ELM)

packages/brick-frame-data/src/.data-nix-stamp:
	$(GENERATOR_NIX)
	touch $@

.PHONY: studio-dev
studio-dev: packages/brick-frame-data/src/Data.elm studio-app/.elm-tailwind/.stamp ## Start the studio Vite dev server (http://localhost:WATCH_PORT)
	cd studio-app && $(VITE) --host 0.0.0.0 --port $(WATCH_PORT)

.PHONY: dev
dev: studio-dev ## Alias for studio-dev

.PHONY: elm-tailwind-gen
elm-tailwind-gen: studio-app/.elm-tailwind/.stamp ## Generate typed Tailwind Elm modules into studio-app

studio-app/.elm-tailwind/.stamp: studio-app/elm.json studio-app/vite.config.mjs studio-app/main.css $(STUDIO_APP_SOURCES) $(ELM_PACKAGE_SOURCES) packages/brick-frame-data/src/Data.elm
	cd studio-app && elm-tailwind-classes gen
	mkdir -p studio-app/.elm-tailwind
	touch $@

build/.studio-stamp: studio-app/.elm-tailwind/.stamp $(STUDIO_APP_SOURCES) $(ELM_PACKAGE_SOURCES) $(VIEWPORT_JS_SOURCES) studio-app/elm.json studio-app/vite.config.mjs studio-app/index.html studio-app/main.js studio-app/main.css packages/brick-frame-ui/theme.css
	mkdir -p build
	cd studio-app && $(VITE) build --outDir ../build/studio
	touch $@

.PHONY: studio-build
studio-build: build/.studio-stamp ## Production build of studio-app → build/studio/

.PHONY: build
build: studio-build ## Build studio-app

.PHONY: preview
preview: build ## Preview production build locally
	cd studio-app && $(VITE) preview --outDir ../build/studio --host 0.0.0.0 --port $(PREVIEW_PORT)

.PHONY: serve
serve: preview ## Alias for preview

# ── Test & Quality ───────────────────────────────────────────────────────────

.PHONY: elm-test
elm-test: elm-tailwind-gen packages/brick-frame-data/src/Data.elm ## Run studio-app unit tests
	cd studio-app && elm-test

.PHONY: test-lib
test-lib: packages/brick-frame-data/src/Data.elm ## Run shared package unit tests (simulator, viewport, simulation, ui, studio)
	cd packages/brick-frame-simulator && elm-test
	cd packages/brick-frame-viewport && elm-test
	cd packages/brick-frame-simulation && elm-test
	cd packages/brick-frame-ui && elm-test
	cd packages/brick-frame-studio && elm-test

.PHONY: test
test: check ## Run all tests (Haskell, studio-app, and shared packages)
	cabal test
	$(MAKE) elm-test
	$(MAKE) test-lib

.PHONY: elm-check
elm-check: ## Check Elm formatting
	cd studio-app && elm-format --validate src/ tests/
	elm-format --validate $(SHARED_PACKAGE_DIRS)

.PHONY: elm-format
elm-format: ## Auto-format Elm source files
	cd studio-app && elm-format --yes src/ tests/
	elm-format --yes $(SHARED_PACKAGE_DIRS)

.PHONY: elm-review
elm-review: ## Run elm-review on studio-app
	cd studio-app && elm-review

.PHONY: elm-review-fix
elm-review-fix: ## Run elm-review with auto-fix
	cd studio-app && elm-review --fix

.PHONY: check
check: ## Check formatting, hlint, and elm-review
	$(HLINT) src generator
	$(MAKE) elm-check
	$(MAKE) elm-review

.PHONY: format
format: ## Auto-format Haskell and Elm source files
	find src generator -name '*.hs' | xargs $(FOURMOLU) --mode inplace
	$(MAKE) elm-format
	treefmt

.PHONY: clean
clean: ## Remove build artifacts
	cabal clean
	rm -rf build brick-data-generator .hpc studio-app/.elm-tailwind studio-app/elm-stuff studio-app/.vite packages/brick-frame-studio/elm-stuff packages/brick-frame-data/src/Data.elm packages/brick-frame-data/src/.data-nix-stamp build/.studio-stamp
