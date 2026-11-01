module I18nTest exposing (suite)

{-| Unit tests for the I18n module, verifying language parsing,
serialization, default language, and translations across Finnish and English.
-}

import Expect
import I18n exposing (Language(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "I18n"
        [ describe "Language parsing and serialization"
            [ test "defaultLanguage is Fi" <|
                \_ ->
                    Expect.equal Fi I18n.defaultLanguage
            , test "fromString parses 'fi' and 'fi-FI' as Fi" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal Fi (I18n.fromString "fi")
                        , \_ -> Expect.equal Fi (I18n.fromString "fi-FI")
                        , \_ -> Expect.equal Fi (I18n.fromString "FI")
                        ]
                        ()
            , test "fromString parses 'en' and variants as En" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal En (I18n.fromString "en")
                        , \_ -> Expect.equal En (I18n.fromString "en-US")
                        , \_ -> Expect.equal En (I18n.fromString "en-GB")
                        , \_ -> Expect.equal En (I18n.fromString "EN")
                        ]
                        ()
            , test "fromString falls back to defaultLanguage (Fi) on unknown codes" <|
                \_ ->
                    Expect.equal Fi (I18n.fromString "de")
            , test "toString / toCode returns 'fi' and 'en'" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "fi" (I18n.toString Fi)
                        , \_ -> Expect.equal "en" (I18n.toString En)
                        , \_ -> Expect.equal "fi" (I18n.toCode Fi)
                        , \_ -> Expect.equal "en" (I18n.toCode En)
                        ]
                        ()
            , test "label returns native language names" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Suomi" (I18n.label Fi)
                        , \_ -> Expect.equal "English" (I18n.label En)
                        ]
                        ()
            ]
        , describe "Header & Controls translations"
            [ test "menu, new, open, save, tidyUp, play, pause" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Valikko" (I18n.headerMenu Fi)
                        , \_ -> Expect.equal "Menu" (I18n.headerMenu En)
                        , \_ -> Expect.equal "Uusi" (I18n.headerNew Fi)
                        , \_ -> Expect.equal "New" (I18n.headerNew En)
                        , \_ -> Expect.equal "Avaa" (I18n.headerOpen Fi)
                        , \_ -> Expect.equal "Open" (I18n.headerOpen En)
                        , \_ -> Expect.equal "Tallenna" (I18n.headerSave Fi)
                        , \_ -> Expect.equal "Save" (I18n.headerSave En)
                        , \_ -> Expect.equal "Siisti malli" (I18n.headerTidyUp Fi)
                        , \_ -> Expect.equal "Tidy up" (I18n.headerTidyUp En)
                        , \_ -> Expect.equal "Käynnistä" (I18n.headerPlay Fi)
                        , \_ -> Expect.equal "Play" (I18n.headerPlay En)
                        , \_ -> Expect.equal "Pysäytä" (I18n.headerPause Fi)
                        , \_ -> Expect.equal "Pause" (I18n.headerPause En)
                        ]
                        ()
            ]
        , describe "Colors translations"
            [ test "basic LDraw colors in Fi and En" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Musta" (I18n.colorName Fi 0)
                        , \_ -> Expect.equal "Black" (I18n.colorName En 0)
                        , \_ -> Expect.equal "Sininen" (I18n.colorName Fi 1)
                        , \_ -> Expect.equal "Blue" (I18n.colorName En 1)
                        , \_ -> Expect.equal "Vihreä" (I18n.colorName Fi 2)
                        , \_ -> Expect.equal "Green" (I18n.colorName En 2)
                        , \_ -> Expect.equal "Punainen" (I18n.colorName Fi 4)
                        , \_ -> Expect.equal "Red" (I18n.colorName En 4)
                        , \_ -> Expect.equal "Keltainen" (I18n.colorName Fi 14)
                        , \_ -> Expect.equal "Yellow" (I18n.colorName En 14)
                        , \_ -> Expect.equal "Valkoinen" (I18n.colorName Fi 15)
                        , \_ -> Expect.equal "White" (I18n.colorName En 15)
                        , \_ -> Expect.equal "Vaalean siniharmaa" (I18n.colorName Fi 71)
                        , \_ -> Expect.equal "Light Bluish Grey" (I18n.colorName En 71)
                        , \_ -> Expect.equal "Hiekankeltainen" (I18n.colorName Fi 19)
                        , \_ -> Expect.equal "Tan" (I18n.colorName En 19)
                        ]
                        ()
            ]
        , describe "Ring & Toolbar translations"
            [ test "step names and tooltips" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Nuppiliike" (I18n.ringStudStep Fi)
                        , \_ -> Expect.equal "Stud step" (I18n.ringStudStep En)
                        , \_ -> Expect.equal "Puolinuppiliike" (I18n.ringHalfStudStep Fi)
                        , \_ -> Expect.equal "Half-stud step" (I18n.ringHalfStudStep En)
                        , \_ -> Expect.equal "Pikkuliike (1 LDU)" (I18n.ringBitStep Fi)
                        , \_ -> Expect.equal "Bit step" (I18n.ringBitStep En)
                        , \_ -> Expect.equal "Leikkaa" (I18n.ringCut Fi)
                        , \_ -> Expect.equal "Cut" (I18n.ringCut En)
                        , \_ -> Expect.equal "Kopioi" (I18n.ringCopy Fi)
                        , \_ -> Expect.equal "Copy" (I18n.ringCopy En)
                        , \_ -> Expect.equal "Liitä" (I18n.ringPaste Fi)
                        , \_ -> Expect.equal "Paste" (I18n.ringPaste En)
                        , \_ -> Expect.equal "Poista" (I18n.ringDelete Fi)
                        , \_ -> Expect.equal "Delete" (I18n.ringDelete En)
                        , \_ -> Expect.equal "Lukitse" (I18n.ringGlue Fi)
                        , \_ -> Expect.equal "Glue" (I18n.ringGlue En)
                        ]
                        ()
            ]
        , describe "Tidy Up & Health translations"
            [ test "pill label singular and plural" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            Expect.equal "1 irti oleva palikka · Siisti"
                                (I18n.tidyPillLabel Fi { count = 1, hasSnap = True, hasOverlap = False })
                        , \_ ->
                            Expect.equal "1 loose brick · Tidy up"
                                (I18n.tidyPillLabel En { count = 1, hasSnap = True, hasOverlap = False })
                        , \_ ->
                            Expect.equal "3 irti olevaa palikkaa · Siisti"
                                (I18n.tidyPillLabel Fi { count = 3, hasSnap = True, hasOverlap = False })
                        , \_ ->
                            Expect.equal "3 loose bricks · Tidy up"
                                (I18n.tidyPillLabel En { count = 3, hasSnap = True, hasOverlap = False })
                        ]
                        ()
            , test "health fix sentences" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            Expect.equal "Nosta 3001.dat toisen palikan sisältä"
                                (I18n.healthLiftOut Fi "3001.dat")
                        , \_ ->
                            Expect.equal "Lift 3001.dat out of another brick"
                                (I18n.healthLiftOut En "3001.dat")
                        , \_ ->
                            Expect.equal "Siirrä 3001.dat puoli nuppia"
                                (I18n.healthMoveHalfStud Fi "3001.dat")
                        , \_ ->
                            Expect.equal "Move 3001.dat half a stud"
                                (I18n.healthMoveHalfStud En "3001.dat")
                        ]
                        ()
            ]
        , describe "Shortcuts dialog"
            [ test "dialog title and help rows" <|
                \_ ->
                    let
                        fiRows =
                            I18n.shortcutsHelpRows Fi { free = True, technic = True, hasInventory = True }

                        enRows =
                            I18n.shortcutsHelpRows En { free = True, technic = True, hasInventory = True }
                    in
                    Expect.all
                        [ \_ -> Expect.equal "Pikanäppäimet" (I18n.shortcutsDialogTitle Fi)
                        , \_ -> Expect.equal "Keyboard shortcuts" (I18n.shortcutsDialogTitle En)
                        , \_ -> Expect.equal True (List.any (\( group, _ ) -> group == "Rakentaminen") fiRows)
                        , \_ -> Expect.equal True (List.any (\( group, _ ) -> group == "Build") enRows)
                        ]
                        ()
            ]
        ]
