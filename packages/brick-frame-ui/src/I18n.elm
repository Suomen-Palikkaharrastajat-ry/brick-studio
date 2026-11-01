module I18n exposing
    ( Language(..)
    , allLanguages
    , appPlaygroundTitle
    , bomCategory
    , bomColor
    , bomCopyMarkdown
    , bomCopyText
    , bomDimensions
    , bomElementId
    , bomEstimatedWeight
    , bomName
    , bomPartId
    , bomQty
    , bomTotalParts
    , bomTotalWeight
      -- Gear Feedback
    , bomUniqueLots
    , bomUnitWeight
    , cameraAriaGroup
    , cameraControls
      -- Context Ring & Mobile Toolbar
    , cameraDragTip
    , cameraPositionAria
    , cameraRealistic
    , cameraResetView
    , cameraShortcuts
    , cameraStreetLevel
    , cameraTakeSnapshot
    , cameraZoom
    , categoryAccessories
    , categoryArches
    , categoryBaseplates
    , categoryBracketsHinges
    , categoryBracketsSnot
    , categoryBricks
    , categoryName
      -- Colors
    , categoryNature
    , categoryPlates
    , categoryRound
    , categorySlopes
    , categorySlopesAndTiles
    , categoryTechnic
    , categoryTiles
    , categoryVehicles
    , categoryWallsFences
    , categoryWedgesWings
    , categoryWheels
    , categoryWindowsDoors
    , colorName
    , colorsFilterTip
    , colorsInProduction
    , colorsOfficialOnly
    , colorsOutOfProduction
    , colorsUnofficial
      -- Camera & View Controls
    , defaultLanguage
    , fromCode
    , fromString
    , gearAllRotating
    , gearApartNeeds
    , gearMeshesWith
    , gearNoOtherGear
      -- Snapshot Modal
    , gearNotParallel
    , gearNotPerpendicular
    , gearNotSamePlane
    , headerBackToEditing
    , headerExport
    , headerExportHtml
      -- Menu Modal & Dropdown Items
    , headerInstructions
    , headerMenu
    , headerMoreActions
    , headerNew
    , headerOpen
    , headerPause
    , headerPlay
    , headerRealistic
    , headerSave
    , headerSheetView
    , headerShortcuts
    , headerSimulate
    , headerSimulateGears
    , headerStop
    , headerTidyUp
    , healthLiftOut
    , healthMoveFewLdu
    , healthMoveHalfStud
    , healthMoveOntoStuds
      -- Shortcuts Dialog
    , inspectorMultiColor
    , inspectorNothingSelected
    , inspectorOverlapsAlert
    , inspectorPosition
    , inspectorRotation
    , label
    , languageChoicePrompt
    , languageLabel
    , languageToggleLabel
    , linkCopied
      -- Palette & Categories
    , menuBrickPlay
    , menuBrickPlayDesc
    , menuBrickPlayModel
    , menuBrickPlayModelDesc
    , menuClear
    , menuCopyLink
    , menuCopyLinkTitle
    , menuDelete
    , menuDownloadIo
    , menuDraftRendering
    , menuExportBlender
    , menuExportBlenderDesc
    , menuExportGltf
    , menuExportGltfDesc
    , menuExportUsdz
    , menuExportUsdzDesc
    , menuInstructions
    , menuInstructionsDesc
    , menuNewDocument
    , menuNewDocumentDesc
    , menuOpenFile
    , menuOpenFileDesc
    , menuPartsList
    , menuPartsListDesc
    , menuPicturePng
    , menuPicturePngDesc
    , menuRealisticRendering
    , menuRedo
    , menuRenderQualityDesc
    , menuSaveModel
    , menuSaveModelDesc
    , menuSendToApp
    , menuShareEditable
    , menuShareFixed
    , menuSharePartsSet
    , menuUndo
    , paletteAdd
    , paletteAllCategories
    , paletteAllShelf
    , paletteCategoryPicker
    , paletteEverything
    , paletteFavouritesShelf
    , paletteLoading
    , paletteManualPlaceholder
    , paletteNoCatalogue
    , paletteOfficialColours
    , paletteRecentShelf
    , paletteSearchAria
    , paletteSearchPlaceholder
    , ringBitStep
    , ringBitStepTip
    , ringClipboardGroup
    , ringCopy
    , ringCopyTip
    , ringCut
    , ringCutTip
    , ringDelete
    , ringDeleteTip
    , ringGlue
    , ringGlueTip
    , ringGlued
    , ringGroup
    , ringGroupTip
    , ringHalfStudStep
    , ringHalfStudStepTip
    , ringLower
    , ringLowerTip
    , ringMoveAway
    , ringMoveAwayTip
    , ringMoveLeft
    , ringMoveLeftTip
    , ringMoveRight
    , ringMoveRightTip
    , ringMoveStepAria
    , ringMoveThePart
    , ringMoveToward
    , ringMoveTowardTip
    , ringPartControls
    , ringPaste
    , ringPasteTip
    , ringRaise
    , ringRaiseTip
    , ringRotationControls
      -- Tool Rail & Transform Tools (Studio)
    , ringShiftBitPill
    , ringShiftBitTip
    , ringStudStep
    , ringStudStepTip
    , ringTipForward
    , ringTipForwardTip
    , ringTurnLeft
    , ringTurnLeftTip
    , ringTurnRight
    , ringTurnRightTip
    , ringUngroup
    , ringUngroupTip
    , shortcutsClose
    , shortcutsDialogTitle
    , shortcutsFootnote
      -- Inspector & BOM
    , shortcutsHelpRows
    , snapshotClose
      -- Language Switcher
    , snapshotPathTracing
    , snapshotReadyToSave
    , snapshotRenderNotice
    , snapshotSaveImage
    , snapshotSavedImage
    , snapshotSavedNotice
    , snapshotSubtitle
    , snapshotTitle
    , technicAxles
    , technicBeams
    , technicGears
    , technicPins
    , tidyApply
    , tidyCancel
    , tidyPillLabel
    , tidyPlannedMoves
    , tidyPreviewTip
    , tidySummary
    , tidyTitle
    , toCode
    , toString
      -- Header & Navigation
    , toolDelete
    , toolDuplicate
    , toolEnterSubmodel
    , toolExplodeSubmodel
    , toolGroupSubmodel
    , toolMove
    , toolRedo
    , toolRotate
    , toolSelect
    , toolTurnAxis
      -- Health & Tidy Up
    , toolTurnBackAxis
    , toolUndo
    )

{-| Central localization and internationalization module for Palikkakehys applications.

Supported languages:

  - Finnish (`Fi`, default)
  - English (`En`)

-}


{-| Target languages supported in the application.
-}
type Language
    = Fi
    | En


{-| Default language across Palikkakehys is Finnish.
-}
defaultLanguage : Language
defaultLanguage =
    Fi


{-| Convert Language to ISO language code string.
-}
toCode : Language -> String
toCode lang =
    case lang of
        Fi ->
            "fi"

        En ->
            "en"


{-| Alias for `toCode`.
-}
toString : Language -> String
toString =
    toCode


{-| Parse a language code or tag into a `Language`, defaulting to `Fi`.
-}
fromCode : String -> Language
fromCode raw =
    let
        cleaned =
            String.toLower (String.trim raw)
    in
    if cleaned == "en" || String.startsWith "en-" cleaned then
        En

    else if cleaned == "fi" || String.startsWith "fi-" cleaned then
        Fi

    else
        defaultLanguage


{-| Alias for `fromCode`.
-}
fromString : String -> Language
fromString =
    fromCode


{-| Native language name.
-}
label : Language -> String
label lang =
    case lang of
        Fi ->
            "Suomi"

        En ->
            "English"


{-| Label for language switcher.
-}
languageLabel : Language -> String
languageLabel =
    label


{-| Language selection prompt label.
-}
languageChoicePrompt : Language -> String
languageChoicePrompt lang =
    case lang of
        Fi ->
            "Kieli"

        En ->
            "Language"


{-| Label showing the alternative language to toggle to.
-}
languageToggleLabel : Language -> String
languageToggleLabel lang =
    case lang of
        Fi ->
            "In English"

        En ->
            "Suomeksi"


{-| All supported languages.
-}
allLanguages : List Language
allLanguages =
    [ Fi, En ]



-- ── Header & Navigation ────────────────────────────────────────────────────────


headerMenu : Language -> String
headerMenu lang =
    case lang of
        Fi ->
            "Valikko"

        En ->
            "Menu"


headerNew : Language -> String
headerNew lang =
    case lang of
        Fi ->
            "Uusi"

        En ->
            "New"


headerOpen : Language -> String
headerOpen lang =
    case lang of
        Fi ->
            "Avaa"

        En ->
            "Open"


headerSave : Language -> String
headerSave lang =
    case lang of
        Fi ->
            "Tallenna"

        En ->
            "Save"


headerExport : Language -> String
headerExport lang =
    case lang of
        Fi ->
            "Vie"

        En ->
            "Export"


headerMoreActions : Language -> String
headerMoreActions lang =
    case lang of
        Fi ->
            "Lisää toimintoja"

        En ->
            "More actions"


headerRealistic : Language -> String
headerRealistic lang =
    case lang of
        Fi ->
            "Realistinen"

        En ->
            "Realistic"


headerInstructions : Language -> String
headerInstructions lang =
    case lang of
        Fi ->
            "Rakennusohjeet"

        En ->
            "Instructions"


headerSimulate : Language -> String
headerSimulate lang =
    case lang of
        Fi ->
            "Simuloi"

        En ->
            "Simulate"


headerSimulateGears : Language -> String
headerSimulateGears lang =
    case lang of
        Fi ->
            "Simuloi rattaita (P)"

        En ->
            "Simulate gears (P)"


appPlaygroundTitle : Language -> String
appPlaygroundTitle lang =
    case lang of
        Fi ->
            "Palikkaleikkikenttä"

        En ->
            "Brick Playground"


headerPlay : Language -> String
headerPlay lang =
    case lang of
        Fi ->
            "Käynnistä"

        En ->
            "Play"


headerPause : Language -> String
headerPause lang =
    case lang of
        Fi ->
            "Pysäytä"

        En ->
            "Pause"


headerStop : Language -> String
headerStop lang =
    case lang of
        Fi ->
            "Pysäytä"

        En ->
            "Stop"


headerTidyUp : Language -> String
headerTidyUp lang =
    case lang of
        Fi ->
            "Siisti malli"

        En ->
            "Tidy up"


headerShortcuts : Language -> String
headerShortcuts lang =
    case lang of
        Fi ->
            "Pikanäppäimet (?)"

        En ->
            "Keyboard shortcuts (?)"


headerBackToEditing : Language -> String
headerBackToEditing lang =
    case lang of
        Fi ->
            "Takaisin muokkaukseen"

        En ->
            "Back to editing"


headerSheetView : Language -> String
headerSheetView lang =
    case lang of
        Fi ->
            "Ohjelehtinäkymä"

        En ->
            "Sheet view"


headerExportHtml : Language -> String
headerExportHtml lang =
    case lang of
        Fi ->
            "Vie HTML-tiedostona"

        En ->
            "Export HTML"



-- ── Menu Items ───────────────────────────────────────────────────────────────


menuNewDocument : Language -> String
menuNewDocument lang =
    case lang of
        Fi ->
            "Uusi"

        En ->
            "New"


menuNewDocumentDesc : Language -> String
menuNewDocumentDesc lang =
    case lang of
        Fi ->
            "Aloita tyhjä malli"

        En ->
            "Start an empty model"


menuOpenFile : Language -> String
menuOpenFile lang =
    case lang of
        Fi ->
            "Avaa…"

        En ->
            "Open…"


menuOpenFileDesc : Language -> String
menuOpenFileDesc lang =
    case lang of
        Fi ->
            ".ldr-, .mpd- tai .io-tiedosto"

        En ->
            "An .ldr, .mpd or .io file"


menuSaveModel : Language -> String
menuSaveModel lang =
    case lang of
        Fi ->
            "Tallenna"

        En ->
            "Save"


menuSaveModelDesc : Language -> String
menuSaveModelDesc lang =
    case lang of
        Fi ->
            "Lataa malli .ldr-muodossa"

        En ->
            "Download the model as .ldr"


menuPartsList : Language -> String
menuPartsList lang =
    case lang of
        Fi ->
            "Osaluettelo"

        En ->
            "Parts list"


menuPartsListDesc : Language -> String
menuPartsListDesc lang =
    case lang of
        Fi ->
            "Kaikki osat ja BrickLink-vienti"

        En ->
            "Every part, with BrickLink export"


menuInstructions : Language -> String
menuInstructions lang =
    case lang of
        Fi ->
            "Rakennusohjeet"

        En ->
            "Instructions"


menuInstructionsDesc : Language -> String
menuInstructionsDesc lang =
    case lang of
        Fi ->
            "Vaiheittaiset rakennusohjeet"

        En ->
            "Step-by-step building pages"


menuRealisticRendering : Language -> String
menuRealisticRendering lang =
    case lang of
        Fi ->
            "Realistinen renderöinti"

        En ->
            "Realistic rendering"


menuDraftRendering : Language -> String
menuDraftRendering lang =
    case lang of
        Fi ->
            "Luonnosrenderöinti"

        En ->
            "Draft rendering"


menuRenderQualityDesc : Language -> String
menuRenderQualityDesc lang =
    case lang of
        Fi ->
            "Kiiltävät heijastukset ja voimakkaampi valo, hitaampi"

        En ->
            "Glossy highlights and stronger light, slower"


menuDownloadIo : Language -> String
menuDownloadIo lang =
    case lang of
        Fi ->
            "Lataa BrickLink Studio (.io)"

        En ->
            "Download BrickLink Studio (.io)"


menuExportGltf : Language -> String
menuExportGltf lang =
    case lang of
        Fi ->
            "glTF (.glb)"

        En ->
            "glTF (.glb)"


menuExportGltfDesc : Language -> String
menuExportGltfDesc lang =
    case lang of
        Fi ->
            "Three.js-, Blender- ja 3D-työkaluille"

        En ->
            "For three.js, Blender and most 3D tools"


menuExportBlender : Language -> String
menuExportBlender lang =
    case lang of
        Fi ->
            "Blender-skripti (.py)"

        En ->
            "Blender script (.py)"


menuExportBlenderDesc : Language -> String
menuExportBlenderDesc lang =
    case lang of
        Fi ->
            "Rakentaa mallin pyöristetyillä reunoilla"

        En ->
            "Rebuilds the model with bevelled parts"


menuExportUsdz : Language -> String
menuExportUsdz lang =
    case lang of
        Fi ->
            "AR-malli (.usdz)"

        En ->
            "AR model (.usdz)"


menuExportUsdzDesc : Language -> String
menuExportUsdzDesc lang =
    case lang of
        Fi ->
            "Pikakatselu iPhone- ja iPad-laitteilla"

        En ->
            "Quick Look on iPhone and iPad"


menuPicturePng : Language -> String
menuPicturePng lang =
    case lang of
        Fi ->
            "Kuva (.png)"

        En ->
            "Picture (.png)"


menuPicturePngDesc : Language -> String
menuPicturePngDesc lang =
    case lang of
        Fi ->
            "1080p-kuva nykyisestä näkymästä"

        En ->
            "A 1080p render of the current view"


menuSendToApp : Language -> String
menuSendToApp lang =
    case lang of
        Fi ->
            "Avaa sovelluksessa"

        En ->
            "Send to App"


menuBrickPlay : Language -> String
menuBrickPlay _ =
    "Brick Play"


menuBrickPlayDesc : Language -> { count : Int } -> String
menuBrickPlayDesc lang { count } =
    case lang of
        Fi ->
            "Vapaa rakentelu tällä " ++ String.fromInt count ++ " osan sarjalla"

        En ->
            "Free build with this kit of " ++ String.fromInt count ++ " parts"


menuBrickPlayModel : Language -> String
menuBrickPlayModel lang =
    case lang of
        Fi ->
            "Brick Play mallin kanssa"

        En ->
            "Brick Play with model"


menuBrickPlayModelDesc : Language -> { count : Int } -> String
menuBrickPlayModelDesc lang { count } =
    case lang of
        Fi ->
            "Simuloi ja testaa mekanismeja näillä " ++ String.fromInt count ++ " osalla"

        En ->
            "Simulate & test mechanisms with these " ++ String.fromInt count ++ " parts"


menuCopyLink : Language -> String
menuCopyLink lang =
    case lang of
        Fi ->
            "Kopioi linkki"

        En ->
            "Copy Link"


menuCopyLinkTitle : Language -> String
menuCopyLinkTitle lang =
    case lang of
        Fi ->
            "Kopioi linkki leikepöydälle"

        En ->
            "Copy link to clipboard"


menuShareEditable : Language -> String
menuShareEditable lang =
    case lang of
        Fi ->
            "Jaa muokattava malli"

        En ->
            "Share editable model"


menuSharePartsSet : Language -> String
menuSharePartsSet lang =
    case lang of
        Fi ->
            "Jaa osasarja"

        En ->
            "Share parts set"


menuShareFixed : Language -> String
menuShareFixed lang =
    case lang of
        Fi ->
            "Jaa lukittu malli"

        En ->
            "Share fixed model"


menuUndo : Language -> String
menuUndo lang =
    case lang of
        Fi ->
            "Kumoa"

        En ->
            "Undo"


menuRedo : Language -> String
menuRedo lang =
    case lang of
        Fi ->
            "Tee uudelleen"

        En ->
            "Redo"


menuDelete : Language -> String
menuDelete lang =
    case lang of
        Fi ->
            "Poista"

        En ->
            "Delete"


menuClear : Language -> String
menuClear lang =
    case lang of
        Fi ->
            "Tyhjennä"

        En ->
            "Clear"


linkCopied : Language -> String
linkCopied lang =
    case lang of
        Fi ->
            "Linkki kopioitu leikepöydälle!"

        En ->
            "Link copied to clipboard!"



-- ── Palette & Categories ────────────────────────────────────────────────────


paletteSearchPlaceholder : Language -> String
paletteSearchPlaceholder lang =
    case lang of
        Fi ->
            "Etsi osia (nimi, tunniste, hakusana)"

        En ->
            "Search parts (name, id, keyword)"


paletteSearchAria : Language -> String
paletteSearchAria lang =
    case lang of
        Fi ->
            "Etsi osia"

        En ->
            "Search parts"


paletteLoading : Language -> String
paletteLoading lang =
    case lang of
        Fi ->
            "Ladataan osaluetteloa…"

        En ->
            "Loading the part library…"


paletteNoCatalogue : Language -> { reason : String } -> String
paletteNoCatalogue lang { reason } =
    case lang of
        Fi ->
            "Osaluetteloa ei löydy: " ++ reason ++ ". Synkronoi LDraw-kirjasto (make sync-ldraw) tai syötä osanumero alle."

        En ->
            "No part catalogue: " ++ reason ++ ". Sync the LDraw library (make sync-ldraw) to build one, or type a part number below."


paletteManualPlaceholder : Language -> String
paletteManualPlaceholder lang =
    case lang of
        Fi ->
            "Osanumero, esim. 3001"

        En ->
            "Part number, e.g. 3001"


paletteAdd : Language -> String
paletteAdd lang =
    case lang of
        Fi ->
            "Lisää"

        En ->
            "Add"


paletteAllShelf : Language -> String
paletteAllShelf lang =
    case lang of
        Fi ->
            "Kaikki"

        En ->
            "All"


paletteFavouritesShelf : Language -> Maybe Int -> String
paletteFavouritesShelf lang maybeCount =
    case lang of
        Fi ->
            case maybeCount of
                Just count ->
                    "Suosikit (" ++ String.fromInt count ++ ")"

                Nothing ->
                    "Suosikit"

        En ->
            case maybeCount of
                Just count ->
                    "Favourites (" ++ String.fromInt count ++ ")"

                Nothing ->
                    "Favourites"


paletteRecentShelf : Language -> Maybe Int -> String
paletteRecentShelf lang maybeCount =
    case lang of
        Fi ->
            case maybeCount of
                Just count ->
                    "Viimeisimmät (" ++ String.fromInt count ++ ")"

                Nothing ->
                    "Viimeisimmät"

        En ->
            case maybeCount of
                Just count ->
                    "Recent (" ++ String.fromInt count ++ ")"

                Nothing ->
                    "Recent"


paletteOfficialColours : Language -> String
paletteOfficialColours lang =
    case lang of
        Fi ->
            "Viralliset värit"

        En ->
            "Official colours"


paletteEverything : Language -> String
paletteEverything lang =
    case lang of
        Fi ->
            "Kaikki osat"

        En ->
            "Everything"


paletteCategoryPicker : Language -> String
paletteCategoryPicker lang =
    case lang of
        Fi ->
            "Kategoria"

        En ->
            "Category"


paletteAllCategories : Language -> String
paletteAllCategories lang =
    case lang of
        Fi ->
            "Kaikki kategoriat"

        En ->
            "All categories"


categoryBricks : Language -> String
categoryBricks lang =
    case lang of
        Fi ->
            "Palikat"

        En ->
            "Bricks"


categoryPlates : Language -> String
categoryPlates lang =
    case lang of
        Fi ->
            "Levyt"

        En ->
            "Plates"


categoryTiles : Language -> String
categoryTiles lang =
    case lang of
        Fi ->
            "Laatat"

        En ->
            "Tiles"


categorySlopes : Language -> String
categorySlopes lang =
    case lang of
        Fi ->
            "Viistopalikat"

        En ->
            "Slopes"


categorySlopesAndTiles : Language -> String
categorySlopesAndTiles lang =
    case lang of
        Fi ->
            "Viistot ja laatat"

        En ->
            "Slopes & tiles"


categoryRound : Language -> String
categoryRound lang =
    case lang of
        Fi ->
            "Pyöreät"

        En ->
            "Round"


categoryArches : Language -> String
categoryArches lang =
    case lang of
        Fi ->
            "Kaaret"

        En ->
            "Arches"


categoryWindowsDoors : Language -> String
categoryWindowsDoors lang =
    case lang of
        Fi ->
            "Ikkunat ja ovet"

        En ->
            "Windows & doors"


categoryWallsFences : Language -> String
categoryWallsFences lang =
    case lang of
        Fi ->
            "Seinät ja aidat"

        En ->
            "Walls & fences"


categoryBracketsHinges : Language -> String
categoryBracketsHinges lang =
    case lang of
        Fi ->
            "Kiinnikkeet ja saranat"

        En ->
            "Brackets & hinges"


categoryBracketsSnot : Language -> String
categoryBracketsSnot lang =
    case lang of
        Fi ->
            "Kulmapalat ja SNOT"

        En ->
            "Brackets & SNOT"


categoryWedgesWings : Language -> String
categoryWedgesWings lang =
    case lang of
        Fi ->
            "Kiilat ja siivet"

        En ->
            "Wedges & wings"


categoryWheels : Language -> String
categoryWheels lang =
    case lang of
        Fi ->
            "Pyörät"

        En ->
            "Wheels"


categoryVehicles : Language -> String
categoryVehicles lang =
    case lang of
        Fi ->
            "Ajoneuvot"

        En ->
            "Vehicles"


categoryAccessories : Language -> String
categoryAccessories lang =
    case lang of
        Fi ->
            "Tarvikkeet"

        En ->
            "Accessories"


categoryNature : Language -> String
categoryNature lang =
    case lang of
        Fi ->
            "Luonto"

        En ->
            "Nature"


categoryBaseplates : Language -> String
categoryBaseplates lang =
    case lang of
        Fi ->
            "Rakennuslevyt"

        En ->
            "Baseplates"


categoryTechnic : Language -> String
categoryTechnic _ =
    "Technic"


technicGears : Language -> String
technicGears lang =
    case lang of
        Fi ->
            "Rattaat"

        En ->
            "Gears"


technicBeams : Language -> String
technicBeams lang =
    case lang of
        Fi ->
            "Palkit"

        En ->
            "Beams"


technicAxles : Language -> String
technicAxles lang =
    case lang of
        Fi ->
            "Akselit"

        En ->
            "Axles"


technicPins : Language -> String
technicPins lang =
    case lang of
        Fi ->
            "Tapit ja holkit"

        En ->
            "Pins & Bushings"


{-| Translates standard LDraw category names to the active language.
-}
categoryName : Language -> String -> String
categoryName lang name =
    case ( lang, String.toLower (String.trim name) ) of
        ( Fi, "brick" ) ->
            "Palikka"

        ( Fi, "bricks" ) ->
            "Palikat"

        ( Fi, "plate" ) ->
            "Levy"

        ( Fi, "plates" ) ->
            "Levyt"

        ( Fi, "tile" ) ->
            "Laatta"

        ( Fi, "tiles" ) ->
            "Laatat"

        ( Fi, "slope" ) ->
            "Viistopalikka"

        ( Fi, "slopes" ) ->
            "Viistopalikat"

        ( Fi, "round" ) ->
            "Pyöreät"

        ( Fi, "arch" ) ->
            "Kaari"

        ( Fi, "arches" ) ->
            "Kaaret"

        ( Fi, "window" ) ->
            "Ikkuna"

        ( Fi, "door" ) ->
            "Ovi"

        ( Fi, "windows & doors" ) ->
            "Ikkunat ja ovet"

        ( Fi, "wall" ) ->
            "Seinä"

        ( Fi, "fence" ) ->
            "Aita"

        ( Fi, "walls & fences" ) ->
            "Seinät ja aidat"

        ( Fi, "bracket" ) ->
            "Kiinnike"

        ( Fi, "hinge" ) ->
            "Sarana"

        ( Fi, "brackets & hinges" ) ->
            "Kiinnikkeet ja saranat"

        ( Fi, "brackets & snot" ) ->
            "Kulmapalat ja SNOT"

        ( Fi, "wedge" ) ->
            "Kiila"

        ( Fi, "wing" ) ->
            "Siipi"

        ( Fi, "wedges & wings" ) ->
            "Kiilat ja siivet"

        ( Fi, "wheel" ) ->
            "Pyörä"

        ( Fi, "wheels" ) ->
            "Pyörät"

        ( Fi, "vehicle" ) ->
            "Ajoneuvo"

        ( Fi, "vehicles" ) ->
            "Ajoneuvot"

        ( Fi, "accessories" ) ->
            "Tarvikkeet"

        ( Fi, "nature" ) ->
            "Luonto"

        ( Fi, "baseplate" ) ->
            "Rakennuslevy"

        ( Fi, "baseplates" ) ->
            "Rakennuslevyt"

        ( Fi, "technic" ) ->
            "Technic"

        ( Fi, "gear" ) ->
            "Ratas"

        ( Fi, "gears" ) ->
            "Rattaat"

        ( Fi, "beam" ) ->
            "Palkki"

        ( Fi, "beams" ) ->
            "Palkit"

        ( Fi, "axle" ) ->
            "Akseli"

        ( Fi, "axles" ) ->
            "Akselit"

        ( Fi, "pin" ) ->
            "Tappi"

        ( Fi, "pins" ) ->
            "Tapit"

        ( Fi, "bush" ) ->
            "Holkki"

        ( Fi, "bushing" ) ->
            "Holkki"

        _ ->
            name



-- ── Colors ──────────────────────────────────────────────────────────────────


{-| Translates standard LDraw colour codes to the active language.
-}
colorName : Language -> Int -> String
colorName lang code =
    case lang of
        Fi ->
            case code of
                0 ->
                    "Musta"

                1 ->
                    "Sininen"

                2 ->
                    "Vihreä"

                3 ->
                    "Tumma turkoosi"

                4 ->
                    "Punainen"

                5 ->
                    "Tumman vaaleanpunainen"

                6 ->
                    "Ruskea"

                7 ->
                    "Vaaleanharmaa"

                8 ->
                    "Tummanharmaa"

                9 ->
                    "Vaaleansininen"

                10 ->
                    "Kirkkaanvihreä"

                11 ->
                    "Vaalea turkoosi"

                12 ->
                    "Lohenpunainen"

                13 ->
                    "Vaaleanpunainen"

                14 ->
                    "Keltainen"

                15 ->
                    "Valkoinen"

                17 ->
                    "Vaaleanvihreä"

                18 ->
                    "Vaaleankeltainen"

                19 ->
                    "Hiekankeltainen"

                22 ->
                    "Violetti"

                25 ->
                    "Oranssi"

                26 ->
                    "Magenta"

                27 ->
                    "Limenvihreä"

                28 ->
                    "Tumma hiekankeltainen"

                29 ->
                    "Kirkas vaaleanpunainen"

                71 ->
                    "Vaalean siniharmaa"

                72 ->
                    "Tumman siniharmaa"

                84 ->
                    "Keskiruskea (Nougat)"

                86 ->
                    "Vaalea ihonväri"

                272 ->
                    "Tummansininen"

                320 ->
                    "Tummanpunainen"

                _ ->
                    "Väri " ++ String.fromInt code

        En ->
            case code of
                0 ->
                    "Black"

                1 ->
                    "Blue"

                2 ->
                    "Green"

                3 ->
                    "Dark Turquoise"

                4 ->
                    "Red"

                5 ->
                    "Dark Pink"

                6 ->
                    "Brown"

                7 ->
                    "Light Grey"

                8 ->
                    "Dark Grey"

                9 ->
                    "Light Blue"

                10 ->
                    "Bright Green"

                11 ->
                    "Light Turquoise"

                12 ->
                    "Salmon"

                13 ->
                    "Pink"

                14 ->
                    "Yellow"

                15 ->
                    "White"

                17 ->
                    "Light Green"

                18 ->
                    "Light Yellow"

                19 ->
                    "Tan"

                22 ->
                    "Purple"

                25 ->
                    "Orange"

                26 ->
                    "Magenta"

                27 ->
                    "Lime"

                28 ->
                    "Dark Tan"

                29 ->
                    "Bright Pink"

                71 ->
                    "Light Bluish Grey"

                72 ->
                    "Dark Bluish Grey"

                84 ->
                    "Medium Nougat"

                86 ->
                    "Light Nougat"

                272 ->
                    "Dark Blue"

                320 ->
                    "Dark Red"

                _ ->
                    "Colour " ++ String.fromInt code


colorsOfficialOnly : Language -> String
colorsOfficialOnly lang =
    case lang of
        Fi ->
            "Vain viralliset värit"

        En ->
            "Official colours only"


colorsFilterTip : Language -> String
colorsFilterTip lang =
    case lang of
        Fi ->
            "Näytä vain värit, joissa tätä osaa on valmistettu"

        En ->
            "Only list the colours this part was made in"


colorsInProduction : Language -> { year : Int, sets : Int } -> String
colorsInProduction lang { year, sets } =
    case lang of
        Fi ->
            "Virallinen: Tuotannossa (vuodesta " ++ String.fromInt year ++ ", " ++ String.fromInt sets ++ " sarjassa)"

        En ->
            "Official: In Production (since " ++ String.fromInt year ++ ", " ++ String.fromInt sets ++ " sets)"


colorsOutOfProduction : Language -> { from : Int, to : Int } -> String
colorsOutOfProduction lang { from, to } =
    case lang of
        Fi ->
            "Virallinen: Poistunut tuotannosta (" ++ String.fromInt from ++ "–" ++ String.fromInt to ++ ")"

        En ->
            "Official: Retired (" ++ String.fromInt from ++ "–" ++ String.fromInt to ++ ")"


colorsUnofficial : Language -> { part : String } -> String
colorsUnofficial lang { part } =
    case lang of
        Fi ->
            "Epävirallinen väri osalle " ++ part

        En ->
            "Unofficial color for " ++ part



-- ── Camera & View Controls ──────────────────────────────────────────────────


cameraAriaGroup : Language -> String
cameraAriaGroup lang =
    case lang of
        Fi ->
            "Kamera"

        En ->
            "Camera"


cameraDragTip : Language -> String
cameraDragTip lang =
    case lang of
        Fi ->
            "Vedä kiertääksesi mallia kameralla; napauta siirtyäksesi"

        En ->
            "Drag to fly the camera around the model; tap to jump there"


cameraPositionAria : Language -> String
cameraPositionAria lang =
    case lang of
        Fi ->
            "Kameran sijainti"

        En ->
            "Camera position"


cameraResetView : Language -> String
cameraResetView lang =
    case lang of
        Fi ->
            "Palauta näkymä (F)"

        En ->
            "Reset view (F)"


cameraZoom : Language -> String
cameraZoom lang =
    case lang of
        Fi ->
            "Lähennä/loitonna"

        En ->
            "Zoom"


cameraRealistic : Language -> String
cameraRealistic lang =
    case lang of
        Fi ->
            "Realistinen renderöinti"

        En ->
            "Realistic rendering"


cameraTakeSnapshot : Language -> String
cameraTakeSnapshot lang =
    case lang of
        Fi ->
            "Ota kuva"

        En ->
            "Take snapshot"


cameraStreetLevel : Language -> String
cameraStreetLevel lang =
    case lang of
        Fi ->
            "Katutasonäkymä"

        En ->
            "Street level view"


cameraShortcuts : Language -> String
cameraShortcuts lang =
    case lang of
        Fi ->
            "Pikanäppäimet"

        En ->
            "Keyboard shortcuts"


cameraControls : Language -> String
cameraControls lang =
    case lang of
        Fi ->
            "Kameran säätimet"

        En ->
            "Camera controls"



-- ── Context Ring & Toolbar ──────────────────────────────────────────────────


ringPartControls : Language -> String
ringPartControls lang =
    case lang of
        Fi ->
            "Osan säätimet"

        En ->
            "Part controls"


ringMoveStepAria : Language -> String
ringMoveStepAria lang =
    case lang of
        Fi ->
            "Siirtoaskel"

        En ->
            "Move step"


ringStudStep : Language -> String
ringStudStep lang =
    case lang of
        Fi ->
            "Nuppiliike"

        En ->
            "Stud step"


ringStudStepTip : Language -> String
ringStudStepTip lang =
    case lang of
        Fi ->
            "Nuppiliike: 20 LDU sivuttain, 24 LDU ylös ja alas"

        En ->
            "Stud step: 20 LDU sideways, 24 LDU up and down"


ringHalfStudStep : Language -> String
ringHalfStudStep lang =
    case lang of
        Fi ->
            "Puolinuppiliike"

        En ->
            "Half-stud step"


ringHalfStudStepTip : Language -> String
ringHalfStudStepTip lang =
    case lang of
        Fi ->
            "Puolinuppiliike: 10 LDU sivuttain, 8 LDU ylös ja alas"

        En ->
            "Half-stud step: 10 LDU sideways, 8 LDU up and down"


ringBitStep : Language -> String
ringBitStep lang =
    case lang of
        Fi ->
            "Pikkuliike (1 LDU)"

        En ->
            "Bit step"


ringBitStepTip : Language -> String
ringBitStepTip lang =
    case lang of
        Fi ->
            "Pikkuliike: 1 LDU kullakin akselilla (Shift on tilapäinen pikkuliike)"

        En ->
            "Bit step: 1 LDU on every axis (Shift is a temporary bit step)"


ringClipboardGroup : Language -> String
ringClipboardGroup lang =
    case lang of
        Fi ->
            "Leikepöytä- ja ryhmittelytoiminnot"

        En ->
            "Clipboard and grouping actions"


ringCut : Language -> String
ringCut lang =
    case lang of
        Fi ->
            "Leikkaa"

        En ->
            "Cut"


ringCutTip : Language -> String
ringCutTip lang =
    case lang of
        Fi ->
            "Leikkaa (Ctrl+X)"

        En ->
            "Cut (Ctrl+X)"


ringCopy : Language -> String
ringCopy lang =
    case lang of
        Fi ->
            "Kopioi"

        En ->
            "Copy"


ringCopyTip : Language -> String
ringCopyTip lang =
    case lang of
        Fi ->
            "Kopioi (Ctrl+C)"

        En ->
            "Copy (Ctrl+C)"


ringPaste : Language -> String
ringPaste lang =
    case lang of
        Fi ->
            "Liitä"

        En ->
            "Paste"


ringPasteTip : Language -> String
ringPasteTip lang =
    case lang of
        Fi ->
            "Liitä (Ctrl+V)"

        En ->
            "Paste (Ctrl+V)"


ringGroup : Language -> String
ringGroup lang =
    case lang of
        Fi ->
            "Ryhmittele"

        En ->
            "Group"


ringGroupTip : Language -> String
ringGroupTip lang =
    case lang of
        Fi ->
            "Ryhmittele (Ctrl+G)"

        En ->
            "Group (Ctrl+G)"


ringUngroup : Language -> String
ringUngroup lang =
    case lang of
        Fi ->
            "Pura ryhmä"

        En ->
            "Ungroup"


ringUngroupTip : Language -> String
ringUngroupTip lang =
    case lang of
        Fi ->
            "Pura ryhmä (Ctrl+Shift+G)"

        En ->
            "Ungroup (Ctrl+Shift+G)"


ringDelete : Language -> String
ringDelete lang =
    case lang of
        Fi ->
            "Poista"

        En ->
            "Delete"


ringDeleteTip : Language -> String
ringDeleteTip lang =
    case lang of
        Fi ->
            "Poista (Del)"

        En ->
            "Delete (Del)"


ringGlue : Language -> String
ringGlue lang =
    case lang of
        Fi ->
            "Lukitse"

        En ->
            "Glue"


ringGlued : Language -> String
ringGlued lang =
    case lang of
        Fi ->
            "Lukittu"

        En ->
            "Glued"


ringGlueTip : Language -> String
ringGlueTip lang =
    case lang of
        Fi ->
            "Lukitse valinta alustaan (L)"

        En ->
            "Glue the selection to the board, locked (L)"


ringLower : Language -> String
ringLower lang =
    case lang of
        Fi ->
            "Laske"

        En ->
            "Lower"


ringLowerTip : Language -> String
ringLowerTip lang =
    case lang of
        Fi ->
            "Laske (Q)"

        En ->
            "Lower (Q)"


ringRaise : Language -> String
ringRaise lang =
    case lang of
        Fi ->
            "Nosta"

        En ->
            "Raise"


ringRaiseTip : Language -> String
ringRaiseTip lang =
    case lang of
        Fi ->
            "Nosta (E)"

        En ->
            "Raise (E)"


ringMoveAway : Language -> String
ringMoveAway lang =
    case lang of
        Fi ->
            "Siirrä loitommas"

        En ->
            "Move away"


ringMoveAwayTip : Language -> String
ringMoveAwayTip lang =
    case lang of
        Fi ->
            "Siirrä loitommas (W)"

        En ->
            "Move away (W)"


ringMoveToward : Language -> String
ringMoveToward lang =
    case lang of
        Fi ->
            "Siirrä lähemmäs"

        En ->
            "Move toward"


ringMoveTowardTip : Language -> String
ringMoveTowardTip lang =
    case lang of
        Fi ->
            "Siirrä lähemmäs (S)"

        En ->
            "Move toward (S)"


ringMoveLeft : Language -> String
ringMoveLeft lang =
    case lang of
        Fi ->
            "Siirrä vasemmalle"

        En ->
            "Move left"


ringMoveLeftTip : Language -> String
ringMoveLeftTip lang =
    case lang of
        Fi ->
            "Siirrä vasemmalle (A)"

        En ->
            "Move left (A)"


ringMoveRight : Language -> String
ringMoveRight lang =
    case lang of
        Fi ->
            "Siirrä oikealle"

        En ->
            "Move right"


ringMoveRightTip : Language -> String
ringMoveRightTip lang =
    case lang of
        Fi ->
            "Siirrä oikealle (D)"

        En ->
            "Move right (D)"


ringTurnLeft : Language -> String
ringTurnLeft lang =
    case lang of
        Fi ->
            "Käännä vasemmalle"

        En ->
            "Turn left"


ringTurnLeftTip : Language -> String
ringTurnLeftTip lang =
    case lang of
        Fi ->
            "Käännä vasemmalle (←)"

        En ->
            "Turn left (←)"


ringTurnRight : Language -> String
ringTurnRight lang =
    case lang of
        Fi ->
            "Käännä oikealle"

        En ->
            "Turn right"


ringTurnRightTip : Language -> String
ringTurnRightTip lang =
    case lang of
        Fi ->
            "Käännä oikealle (→)"

        En ->
            "Turn right (→)"


ringTipForward : Language -> String
ringTipForward lang =
    case lang of
        Fi ->
            "Kallista"

        En ->
            "Tip"


ringTipForwardTip : Language -> String
ringTipForwardTip lang =
    case lang of
        Fi ->
            "Kallista eteenpäin (↑)"

        En ->
            "Tip forward (↑)"


ringShiftBitPill : Language -> String
ringShiftBitPill lang =
    case lang of
        Fi ->
            "Shift · Pikkuliike"

        En ->
            "Shift · Bit"


ringShiftBitTip : Language -> String
ringShiftBitTip lang =
    case lang of
        Fi ->
            "Shift: siirrä 1 LDU ilman kohdistusta"

        En ->
            "Shift: drag by 1 LDU without snapping (bit-dragging)"


ringMoveThePart : Language -> String
ringMoveThePart lang =
    case lang of
        Fi ->
            "Siirrä osaa"

        En ->
            "Move the part"


ringRotationControls : Language -> String
ringRotationControls lang =
    case lang of
        Fi ->
            "Kääntösäätimet"

        En ->
            "Rotation controls"



-- ── Tool Rail & Transform Tools (Studio) ────────────────────────────────────


toolSelect : Language -> String
toolSelect lang =
    case lang of
        Fi ->
            "Valitse"

        En ->
            "Select"


toolMove : Language -> String
toolMove lang =
    case lang of
        Fi ->
            "Siirrä"

        En ->
            "Move"


toolRotate : Language -> String
toolRotate lang =
    case lang of
        Fi ->
            "Kierrä"

        En ->
            "Rotate"


toolUndo : Language -> String
toolUndo lang =
    case lang of
        Fi ->
            "Kumoa"

        En ->
            "Undo"


toolRedo : Language -> String
toolRedo lang =
    case lang of
        Fi ->
            "Tee uudelleen"

        En ->
            "Redo"


toolDuplicate : Language -> String
toolDuplicate lang =
    case lang of
        Fi ->
            "Kahdenna"

        En ->
            "Duplicate"


toolDelete : Language -> String
toolDelete lang =
    case lang of
        Fi ->
            "Poista"

        En ->
            "Delete"


toolGroupSubmodel : Language -> String
toolGroupSubmodel lang =
    case lang of
        Fi ->
            "Ryhmittele alimalliksi"

        En ->
            "Group into submodel"


toolExplodeSubmodel : Language -> String
toolExplodeSubmodel lang =
    case lang of
        Fi ->
            "Pura alimalli"

        En ->
            "Explode submodel"


toolEnterSubmodel : Language -> String
toolEnterSubmodel lang =
    case lang of
        Fi ->
            "Avaa alimalli"

        En ->
            "Enter submodel"


toolTurnBackAxis : Language -> { deg : Int, axis : String } -> String
toolTurnBackAxis lang { deg, axis } =
    case lang of
        Fi ->
            "Kierrä " ++ String.fromInt deg ++ "° taaksepäin akselin " ++ axis ++ " ympäri"

        En ->
            "Turn " ++ String.fromInt deg ++ "° back about " ++ axis


toolTurnAxis : Language -> { deg : Int, axis : String } -> String
toolTurnAxis lang { deg, axis } =
    case lang of
        Fi ->
            "Kierrä " ++ String.fromInt deg ++ "° akselin " ++ axis ++ " ympäri"

        En ->
            "Turn " ++ String.fromInt deg ++ "° about " ++ axis



-- ── Health & Tidy Up ────────────────────────────────────────────────────────


tidyTitle : Language -> String
tidyTitle lang =
    case lang of
        Fi ->
            "Siisti malli"

        En ->
            "Tidy up"


tidyPreviewTip : Language -> String
tidyPreviewTip lang =
    case lang of
        Fi ->
            "Esikatsele siistimissiirrot"

        En ->
            "Preview the tidy-up moves"


tidyPillLabel : Language -> { count : Int, hasSnap : Bool, hasOverlap : Bool } -> String
tidyPillLabel lang { count, hasSnap, hasOverlap } =
    case lang of
        Fi ->
            if hasSnap && not hasOverlap then
                if count == 1 then
                    "1 irti oleva palikka · Siisti"

                else
                    String.fromInt count ++ " irti olevaa palikkaa · Siisti"

            else if hasOverlap && not hasSnap then
                if count == 1 then
                    "1 päällekkäinen palikka · Siisti"

                else
                    String.fromInt count ++ " päällekkäistä palikkaa · Siisti"

            else if count == 1 then
                "1 ongelma · Siisti"

            else
                String.fromInt count ++ " ongelmaa · Siisti"

        En ->
            if hasSnap && not hasOverlap then
                if count == 1 then
                    "1 loose brick · Tidy up"

                else
                    String.fromInt count ++ " loose bricks · Tidy up"

            else if hasOverlap && not hasSnap then
                if count == 1 then
                    "1 overlapping brick · Tidy up"

                else
                    String.fromInt count ++ " overlapping bricks · Tidy up"

            else if count == 1 then
                "1 issue · Tidy up"

            else
                String.fromInt count ++ " issues · Tidy up"


tidySummary : Language -> Int -> String
tidySummary lang count =
    case lang of
        Fi ->
            if count == 1 then
                "1 palikka siirtyy. Läpikuultava kopio näyttää uuden paikan."

            else
                String.fromInt count ++ " palikkaa siirtyy. Läpikuultavat kopiot näyttävät uudet paikat."

        En ->
            if count == 1 then
                "1 part will move. The translucent copies show where."

            else
                String.fromInt count ++ " parts will move. The translucent copies show where."


tidyPlannedMoves : Language -> String
tidyPlannedMoves lang =
    case lang of
        Fi ->
            "Suunnitellut siirrot"

        En ->
            "Planned moves"


tidyApply : Language -> String
tidyApply lang =
    case lang of
        Fi ->
            "Ota käyttöön"

        En ->
            "Apply"


tidyCancel : Language -> String
tidyCancel lang =
    case lang of
        Fi ->
            "Peruuta"

        En ->
            "Cancel"


healthLiftOut : Language -> String -> String
healthLiftOut lang part =
    case lang of
        Fi ->
            "Nosta " ++ part ++ " toisen palikan sisältä"

        En ->
            "Lift " ++ part ++ " out of another brick"


healthMoveHalfStud : Language -> String -> String
healthMoveHalfStud lang part =
    case lang of
        Fi ->
            "Siirrä " ++ part ++ " puoli nuppia"

        En ->
            "Move " ++ part ++ " half a stud"


healthMoveFewLdu : Language -> String -> String
healthMoveFewLdu lang part =
    case lang of
        Fi ->
            "Siirrä " ++ part ++ " muutama LDU"

        En ->
            "Move " ++ part ++ " a few LDU"


healthMoveOntoStuds : Language -> String -> String
healthMoveOntoStuds lang part =
    case lang of
        Fi ->
            "Siirrä " ++ part ++ " nuppeihin"

        En ->
            "Move " ++ part ++ " onto the studs"



-- ── Shortcuts Dialog ────────────────────────────────────────────────────────


shortcutsDialogTitle : Language -> String
shortcutsDialogTitle lang =
    case lang of
        Fi ->
            "Pikanäppäimet"

        En ->
            "Keyboard shortcuts"


shortcutsClose : Language -> String
shortcutsClose lang =
    case lang of
        Fi ->
            "Sulje"

        En ->
            "Close"


shortcutsFootnote : Language -> String
shortcutsFootnote lang =
    case lang of
        Fi ->
            "Katutasossa liikutaan seinien läpi — törmäyksenestoa ei ole."

        En ->
            "Street level flies through walls — there is no collision."


shortcutsHelpRows : Language -> { free : Bool, technic : Bool, hasInventory : Bool } -> List ( String, List ( String, String ) )
shortcutsHelpRows lang config =
    case lang of
        Fi ->
            [ ( "Rakentaminen"
              , [ ( "Valikko, vasen yläkulma", "Jaa linkki, kumoa, tee uudelleen, poista, tyhjennä ja tämä luettelo" )
                , ( "Vedä osaa", "Siirrä kamerakulman mukaan (kohdistuu askeleeseen)" )
                , ( "Kaksoisnapsauta tai -napauta", "Avaa säätökehä: käännä, kallista, nosta, laske, poista, lukitse ja siirtonäppäimet" )
                , ( "Kehä: Nuppi, ½ nuppi, Pikkuliike", "Siirtoaskel: Nuppi 20 LDU sivuttain ja 24 ylös/alas; ½ nuppi 10 ja 8; Pikkuliike 1 LDU" )
                , ( "Shift (pohjassa)", "Tilapäinen pikkuliike: 1 LDU näppäimille ja siirroille" )
                , ( "W A S D  Q E, osa valittuna", "Siirrä: W loitommas kamerasta, S lähemmäs, A ja D vasemmalle/oikealle, Q alas, E ylös" )
                , ( "W A S D  Q E, ei valintaa", "Siirrä kameraa (kuten Kamera-osiossa)" )
                , ( "Tab  /  Shift + Tab", "Valitse seuraava tai edellinen palikka: alhaalta ylös, takaa eteen, vasemmalta oikealle" )
                , ( "← → ↑ ↓", "Käännä valittua osaa (Shift: 45°)" )
                , ( "Delete", "Poista valittu osa" )
                , ( "Ctrl + Z", "Kumoa" )
                , ( "Ctrl + Shift + Z  Ctrl + Y", "Tee uudelleen" )
                , ( "Ctrl + C", "Kopioi" )
                , ( "Ctrl + V", "Liitä" )
                , ( "Ctrl + D", "Kahdenna" )
                , ( "Ctrl + A", "Valitse kaikki" )
                , ( "Ctrl + napsautus  Shift + napsautus", "Lisää valintaan tai poista valinnasta" )
                , ( "Siisti malli (yläpalkki)", "Näkyy kun osat ovat irti nupeista tai päällekkäin. Siisti esikatselee siirrot haamuina; Ota käyttöön tekee siirrot yhtenä askeleena, Peruuta tai Esc ei muuta mitään" )
                , ( "Esc", "Sulje kehä, peruuta siirto tai poista valinta" )
                ]
                    ++ (if config.free then
                            [ ( "L  (kehä: Lukitse)", "Lukitse valitut osat alustaan tai avaa lukitus" ) ]

                        else
                            []
                       )
              )
            ]
                ++ (if config.free then
                        [ ( "Telakka"
                          , [ ( "Välilehdet ja värit", "Palikat, levyt, viistot ja laatat, Technic; väririvi asettaa seuraavan osan värin" )
                            , ( "Hakukenttä", "Etsi mikä tahansa osa koko kirjastosta nimellä tai numerolla, napauta lisätäksesi" )
                            ]
                          )
                        ]

                    else if not config.hasInventory then
                        []

                    else
                        [ ( "Telakka"
                          , [ ( "Kortit", "Kukin kortti on sarjan osa; jäljellä oleva määrä näkyy kuvassa" )
                            , ( "Lukitut osat", "Lukittu osa on kiinteä: sitä ei voi siirtää, kääntää tai värjätä uudelleen" )
                            ]
                          )
                        ]
                   )
                ++ [ ( "Kamera"
                     , [ ( "Vedä palloa, oikea alakulma", "Kierrä kameraa (myös kosketuksella)" )
                       , ( "Napauta palloa", "Siirry suoraan mallin kyseiselle puolelle" )
                       , ( "Zoomaussäädin", "Lähennä ja loitonna" )
                       , ( "Palauta näkymä, tai F", "Sovita koko malli näkymään" )
                       , ( "Vedä tyhjää tilaa", "Panoroi" )
                       , ( "Oikea painike + veto", "Kierrä" )
                       , ( "Keskipainike + veto", "Panoroi" )
                       , ( "Yksi sormi tyhjällä tilalla", "Panoroi" )
                       , ( "Kaksi sormea tyhjällä tilalla", "Kierrä; nipistä tai levitä zoomataksesi" )
                       , ( "Rulla  + -", "Zoom" )
                       , ( "1 2 3 4 5 6  `", "Näkymät: edestä, takaa, vasemmalta, oikealta, ylhäältä, alhaalta, 3/4" )
                       , ( "G", "Katutason kävely, päälle / pois" )
                       ]
                     )
                   ]
                ++ (if config.technic then
                        [ ( "Mekanismi"
                          , [ ( "Välilyönti", "Käynnistä / pysäytä mekanismi (muokkaus pysäyttää ja kelaa alkuun)" )
                            , ( "Rattaat kytkeytyvät", "Ratas joka kohtaa toisen kääntää akseliaan niin että hampaat kytkeytyvät" )
                            , ( "Rataspilleri", "Kun yksi ratas on valittuna, kertoo miten se kytkeytyy lähimpään rattaaseen" )
                            ]
                          )
                        ]

                    else
                        []
                   )
                ++ [ ( "Ohje"
                     , [ ( "?", "Näytä tämä pikanäppäinluettelo" )
                       , ( "Esc", "Sulje pikanäppäimet" )
                       ]
                     )
                   ]

        En ->
            [ ( "Build"
              , [ ( "Menu, top left", "Share link, undo, redo, delete, clear, and this list" )
                , ( "Drag a part", "Move it as seen from the camera (snaps to the move step)" )
                , ( "Double-click a part, or double-tap", "Open its ring: turn, tip, raise, lower, delete, glue, and the move pad" )
                , ( "Ring: Stud, ½ stud, Bit", "The move step: Stud 20 LDU sideways and 24 up or down; ½ stud 10 and 8; Bit 1 LDU" )
                , ( "Shift (held)", "A temporary Bit: 1 LDU for keys and drags" )
                , ( "W A S D  Q E, with a part selected", "Move it: W away from the camera, S toward it, A and D left and right, Q down, E up" )
                , ( "W A S D  Q E, nothing selected", "Move the camera, as in the Camera section" )
                , ( "Tab  /  Shift + Tab", "Select the next or previous brick: bottom to top, then back to front, then left to right" )
                , ( "← → ↑ ↓", "Turn the selected part (Shift: 45°)" )
                , ( "Delete", "Remove the selected part" )
                , ( "Ctrl + Z", "Undo" )
                , ( "Ctrl + Shift + Z  Ctrl + Y", "Redo" )
                , ( "Ctrl + C", "Copy" )
                , ( "Ctrl + V", "Paste" )
                , ( "Ctrl + D", "Duplicate" )
                , ( "Ctrl + A", "Select all" )
                , ( "Ctrl + click  Shift + click", "Add to or remove from the selection" )
                , ( "Tidy up (pill, top left)", "Shown when parts sit off the studs or overlap. Tidy up previews the moves as ghosts; Apply makes them one undo step, Cancel or Esc changes nothing" )
                , ( "Esc", "Close the ring, then cancel a drag, or deselect" )
                ]
                    ++ (if config.free then
                            [ ( "L  (ring: Glue)", "Glue the selected parts to the board, locked, or unglue them" ) ]

                        else
                            []
                       )
              )
            ]
                ++ (if config.free then
                        [ ( "Dock"
                          , [ ( "Tabs and colour row", "Bricks, plates, slopes and tiles, Technic; the colour row sets the colour of the next part" )
                            , ( "Search box", "Find any part in the whole library by name or number, then tap it to add it" )
                            ]
                          )
                        ]

                    else if not config.hasInventory then
                        []

                    else
                        [ ( "Dock"
                          , [ ( "Cards", "Each card is one part of the kit; its count shows on the picture" )
                            , ( "Glued parts", "A glued part is locked: it cannot be moved, turned or recoloured" )
                            ]
                          )
                        ]
                   )
                ++ [ ( "Camera"
                     , [ ( "Drag the globe, bottom right", "Orbit the camera (also by touch)" )
                       , ( "Tap the globe", "Fly to that side of the model" )
                       , ( "Zoom slider", "Zoom in and out" )
                       , ( "Reset view, or F", "Fit the whole model in view" )
                       , ( "Drag empty space", "Pan" )
                       , ( "Right-drag", "Orbit" )
                       , ( "Middle drag", "Pan" )
                       , ( "One finger on empty space", "Pan" )
                       , ( "Two fingers on empty space", "Orbit; spread or pinch to zoom" )
                       , ( "Wheel  + -", "Zoom" )
                       , ( "1 2 3 4 5 6  `", "Views: front, back, left, right, top, bottom, 3/4" )
                       , ( "G", "Street-level walk, on / off" )
                       ]
                     )
                   ]
                ++ (if config.technic then
                        [ ( "Machine"
                          , [ ( "Space", "Play / pause the machine (an edit stops it and rewinds)" )
                            , ( "Gears turn to mesh", "A gear that meets another turns its axle line so the teeth interlock" )
                            , ( "Gear pill", "With one gear selected, says how it meets its nearest gear" )
                            ]
                          )
                        ]

                    else
                        []
                   )
                ++ [ ( "Help"
                     , [ ( "?", "Show this keyboard shortcut reference" )
                       , ( "Esc", "Close this reference" )
                       ]
                     )
                   ]



-- ── Inspector & BOM ─────────────────────────────────────────────────────────


inspectorNothingSelected : Language -> String
inspectorNothingSelected lang =
    case lang of
        Fi ->
            "Ei valintaa. Napsauta osaa tai valitse se rakennekuvaimesta."

        En ->
            "Nothing selected. Click a part, or pick one in the structure list."


inspectorOverlapsAlert : Language -> String
inspectorOverlapsAlert lang =
    case lang of
        Fi ->
            "Osa on päällekkäin toisen osan kanssa"

        En ->
            "Overlaps another part"


inspectorPosition : Language -> String
inspectorPosition lang =
    case lang of
        Fi ->
            "Sijainti"

        En ->
            "Position"


inspectorRotation : Language -> String
inspectorRotation lang =
    case lang of
        Fi ->
            "Kierto"

        En ->
            "Rotation"


inspectorMultiColor : Language -> String
inspectorMultiColor lang =
    case lang of
        Fi ->
            "Useita värejä. Valitse yksi muuttaaksesi kaikki."

        En ->
            "Several colours. Pick one to set them all."


bomTotalParts : Language -> String
bomTotalParts lang =
    case lang of
        Fi ->
            "Osia yhteensä"

        En ->
            "Total Parts"


bomUniqueLots : Language -> String
bomUniqueLots lang =
    case lang of
        Fi ->
            "Eri osalajeja"

        En ->
            "Unique Lots"


bomEstimatedWeight : Language -> String
bomEstimatedWeight lang =
    case lang of
        Fi ->
            "Arvioitu paino"

        En ->
            "Estimated Weight"


bomDimensions : Language -> String
bomDimensions lang =
    case lang of
        Fi ->
            "Mitat"

        En ->
            "Dimensions"


bomCopyMarkdown : Language -> String
bomCopyMarkdown lang =
    case lang of
        Fi ->
            "Kopioi Markdown-taulukkona"

        En ->
            "Copy as Markdown"


bomCopyText : Language -> String
bomCopyText lang =
    case lang of
        Fi ->
            "Kopioi tekstinä"

        En ->
            "Copy as text"


bomPartId : Language -> String
bomPartId lang =
    case lang of
        Fi ->
            "Osatunnus"

        En ->
            "Part ID"


bomName : Language -> String
bomName lang =
    case lang of
        Fi ->
            "Nimi"

        En ->
            "Name"


bomCategory : Language -> String
bomCategory lang =
    case lang of
        Fi ->
            "Kategoria"

        En ->
            "Category"


bomColor : Language -> String
bomColor lang =
    case lang of
        Fi ->
            "Väri"

        En ->
            "Color"


bomElementId : Language -> String
bomElementId lang =
    case lang of
        Fi ->
            "Elementtitunnus"

        En ->
            "Element ID"


bomQty : Language -> String
bomQty lang =
    case lang of
        Fi ->
            "Kpl"

        En ->
            "Qty"


bomUnitWeight : Language -> String
bomUnitWeight lang =
    case lang of
        Fi ->
            "Yksikköpaino"

        En ->
            "Unit weight"


bomTotalWeight : Language -> String
bomTotalWeight lang =
    case lang of
        Fi ->
            "Kokonaispaino"

        En ->
            "Total weight"



-- ── Gear Feedback ───────────────────────────────────────────────────────────


gearAllRotating : Language -> Int -> String
gearAllRotating lang count =
    case lang of
        Fi ->
            "Kaikki " ++ String.fromInt count ++ " ratasta pyörivät yhdessä!"

        En ->
            "All " ++ String.fromInt count ++ " gears are rotating together!"


gearMeshesWith : Language -> String -> String
gearMeshesWith lang other =
    case lang of
        Fi ->
            "Kytkeytyy rattaaseen " ++ other ++ " ✓"

        En ->
            "Meshes with " ++ other ++ " ✓"


gearApartNeeds : Language -> { gear : String, other : String, dist : String, req : String } -> String
gearApartNeeds lang { gear, other, dist, req } =
    case lang of
        Fi ->
            gear ++ " ↔ " ++ other ++ ": etäisyys " ++ dist ++ " LDU — vaatii " ++ req

        En ->
            gear ++ " ↔ " ++ other ++ ": " ++ dist ++ " LDU apart — needs " ++ req


gearNotParallel : Language -> { gear : String, other : String } -> String
gearNotParallel lang { gear, other } =
    case lang of
        Fi ->
            gear ++ " ↔ " ++ other ++ ": akselit eivät ole yhdensuuntaiset"

        En ->
            gear ++ " ↔ " ++ other ++ ": axles not parallel"


gearNotPerpendicular : Language -> { gear : String, other : String } -> String
gearNotPerpendicular lang { gear, other } =
    case lang of
        Fi ->
            gear ++ " ↔ " ++ other ++ ": akseleiden on oltava suorassa kulmassa"

        En ->
            gear ++ " ↔ " ++ other ++ ": axles must be at right angles"


gearNotSamePlane : Language -> { gear : String, other : String, offset : String } -> String
gearNotSamePlane lang { gear, other, offset } =
    case lang of
        Fi ->
            gear ++ " ↔ " ++ other ++ ": eivät ole samassa tasossa (siirtymä " ++ offset ++ ")"

        En ->
            gear ++ " ↔ " ++ other ++ ": not in the same plane (offset " ++ offset ++ ")"


gearNoOtherGear : Language -> String -> String
gearNoOtherGear lang gear =
    case lang of
        Fi ->
            gear ++ ": lisää toinen ratas kytkettäväksi"

        En ->
            gear ++ ": add another gear to mesh with"



-- ── Snapshot Modal ──────────────────────────────────────────────────────────


snapshotTitle : Language -> String
snapshotTitle lang =
    case lang of
        Fi ->
            "Tilannekuva"

        En ->
            "Snapshot"


snapshotSubtitle : Language -> String
snapshotSubtitle lang =
    case lang of
        Fi ->
            "1920 × 1080 PNG-kuva nykyisestä näkymästä"

        En ->
            "A 1920 × 1080 PNG of the current view"


snapshotPathTracing : Language -> Int -> String
snapshotPathTracing lang samples =
    case lang of
        Fi ->
            "Säteenseuranta… " ++ String.fromInt samples ++ " näytettä"

        En ->
            "Path tracing… " ++ String.fromInt samples ++ " samples"


snapshotRenderNotice : Language -> String
snapshotRenderNotice lang =
    case lang of
        Fi ->
            "Luodaan säteenseurattu kuva; tämä kestää noin 30 sekuntia."

        En ->
            "Rendering a path-traced image; this takes about 30 seconds."


snapshotSaveImage : Language -> String
snapshotSaveImage lang =
    case lang of
        Fi ->
            "Tallenna kuva"

        En ->
            "Save Image"


snapshotSavedImage : Language -> String
snapshotSavedImage lang =
    case lang of
        Fi ->
            "Kuva tallennettu"

        En ->
            "Saved Image"


snapshotSavedNotice : Language -> String
snapshotSavedNotice lang =
    case lang of
        Fi ->
            "Tallennettu latauskansioon"

        En ->
            "Saved to downloads"


snapshotReadyToSave : Language -> String
snapshotReadyToSave lang =
    case lang of
        Fi ->
            "Valmis tallennettavaksi"

        En ->
            "Ready to save"


snapshotClose : Language -> String
snapshotClose lang =
    case lang of
        Fi ->
            "Sulje"

        En ->
            "Close"
