module SnapshotModalTest exposing (suite)

{-| Tests for UI.SnapshotModal state transitions and its rendered controls.
-}

import Expect
import Html.Attributes as HtmlAttr
import I18n exposing (Language(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import UI.SnapshotModal as SnapshotModal


type TestMsg
    = Save { dataUrl : String, filename : String }
    | Close
    | StartRender SnapshotModal.CropRect
    | Reframe
    | SetCrop SnapshotModal.CropRect
    | SetAspect SnapshotModal.AspectPreset
    | ResetCrop
    | Interaction SnapshotModal.InteractionMsg


testActions : SnapshotModal.Actions TestMsg
testActions =
    { onSave = Save
    , onClose = Close
    }


testFramingActions : SnapshotModal.FramingActions TestMsg
testFramingActions =
    { onSave = Save
    , onClose = Close
    , onStartRender = StartRender
    , onReframe = Reframe
    , onSetCrop = SetCrop
    , onSetAspect = SetAspect
    , onResetCrop = ResetCrop
    , onInteraction = Interaction
    }


readyModel : SnapshotModal.Model
readyModel =
    SnapshotModal.init { filename = "test.png" }
        |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,mock", filename = "test.png" }


suite : Test
suite =
    describe "UI.SnapshotModal"
        [ describe "State transitions"
            [ test "init starts in Rendering phase with no progress" <|
                \_ ->
                    SnapshotModal.init { filename = "test-render.png" }
                        |> (\modal -> ( modal.phase, modal.progress ))
                        |> Expect.equal ( SnapshotModal.Rendering, { fraction = 0, samples = 0 } )
            , test "setReady transitions to Preview with hasSaved = False" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test-render.png" }
                                |> SnapshotModal.setReady
                                    { dataUrl = "data:image/png;base64,mock"
                                    , filename = "test-render.png"
                                    }
                    in
                    case modal.phase of
                        SnapshotModal.Preview data ->
                            Expect.all
                                [ \d -> Expect.equal "data:image/png;base64,mock" d.dataUrl
                                , \d -> Expect.equal "test-render.png" d.filename
                                , \d -> Expect.equal False d.hasSaved
                                ]
                                data

                        _ ->
                            Expect.fail "Expected Preview phase"
            , test "markSaved records the save in the Preview phase" <|
                \_ ->
                    case (SnapshotModal.markSaved readyModel).phase of
                        SnapshotModal.Preview data ->
                            Expect.equal True data.hasSaved

                        _ ->
                            Expect.fail "Expected Preview phase"
            , test "markSaved is a no-op while Rendering" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test.png" }
                    in
                    Expect.equal modal (SnapshotModal.markSaved modal)
            , test "setProgress records the fraction and sample count while Rendering" <|
                \_ ->
                    SnapshotModal.init { filename = "test.png" }
                        |> SnapshotModal.setProgress { fraction = 0.42, samples = 42 }
                        |> .progress
                        |> Expect.equal { fraction = 0.42, samples = 42 }
            , test "setProgress clamps the fraction to the 0 to 1 range" <|
                \_ ->
                    SnapshotModal.init { filename = "test.png" }
                        |> SnapshotModal.setProgress { fraction = 1.7, samples = -3 }
                        |> .progress
                        |> Expect.equal { fraction = 1, samples = 0 }
            , test "setProgress is ignored once the image is ready" <|
                \_ ->
                    readyModel
                        |> SnapshotModal.setProgress { fraction = 0.5, samples = 10 }
                        |> Expect.equal readyModel
            , test "setFailed moves a rendering modal to Failed with the message" <|
                \_ ->
                    SnapshotModal.init { filename = "test.png" }
                        |> SnapshotModal.setFailed "WebGL context lost"
                        |> .phase
                        |> Expect.equal (SnapshotModal.Failed "WebGL context lost")
            , test "setFailed is ignored once the image is ready" <|
                \_ ->
                    readyModel
                        |> SnapshotModal.setFailed "late error"
                        |> Expect.equal readyModel
            , test "isRendering is True only in the Rendering phase" <|
                \_ ->
                    Expect.all
                        [ \_ -> SnapshotModal.isRendering (SnapshotModal.init { filename = "a.png" }) |> Expect.equal True
                        , \_ -> SnapshotModal.isRendering readyModel |> Expect.equal False
                        , \_ ->
                            SnapshotModal.init { filename = "a.png" }
                                |> SnapshotModal.setFailed "boom"
                                |> SnapshotModal.isRendering
                                |> Expect.equal False
                        ]
                        ()
            ]
        , describe "Rendering view"
            [ test "shows the sample count, a progress bar and the honest time estimate" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.setProgress { fraction = 0.42, samples = 42 }
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.attribute (HtmlAttr.attribute "role" "dialog") ]
                            , Query.has [ Selector.text "Path tracing… 42 samples" ]
                            , Query.has [ Selector.attribute (HtmlAttr.attribute "role" "progressbar") ]
                            , Query.has [ Selector.attribute (HtmlAttr.attribute "aria-valuenow" "42") ]
                            , Query.has [ Selector.text "Rendering a path-traced image; this takes about 30 seconds." ]
                            , Query.has [ Selector.tag "button", Selector.text "Close" ]
                            , Query.hasNot [ Selector.text "Raytrac" ]
                            , Query.hasNot [ Selector.tag "input" ]
                            ]
            , test "starts at zero samples before any progress report" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Path tracing… 0 samples" ]
            ]
        , describe "Failed view"
            [ test "shows the error and a Close button, without Save" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.setFailed "Out of memory while tracing"
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.text "Out of memory while tracing" ]
                            , Query.has [ Selector.tag "button", Selector.text "Close" ]
                            , Query.hasNot [ Selector.tag "button", Selector.text "Save Image" ]
                            ]
            ]
        , describe "Preview view"
            [ test "shows the image and Save, with Close and no agreement checkbox" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,abc123", filename = "render.png" }
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.tag "img", Selector.attribute (HtmlAttr.src "data:image/png;base64,abc123") ]
                            , Query.has [ Selector.text "render.png • PNG" ]
                            , Query.has [ Selector.tag "button", Selector.text "Save Image" ]
                            , Query.has [ Selector.tag "button", Selector.text "Close" ]
                            , Query.hasNot [ Selector.tag "input" ]
                            ]
            , test "describes the image as 1920 × 1080" <|
                \_ ->
                    readyModel
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "A 1920 × 1080 PNG of the current view" ]
            ]
        , describe "Framing phase"
            [ test "initFraming initializes with default crop and AspectFree" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.initFraming { filename = "frame.png", previewUrl = Just "data:image/png;base64,prev" }
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal True
                        , \m -> SnapshotModal.isRendering m |> Expect.equal False
                        , \m -> SnapshotModal.getCrop m |> Expect.equal SnapshotModal.defaultCrop
                        ]
                        modal
            , test "setCrop updates and clamps crop coordinates" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.initFraming { filename = "frame.png", previewUrl = Nothing }
                                |> SnapshotModal.setCrop { x = 0.2, y = 0.3, width = 0.5, height = 0.4 }
                    in
                    SnapshotModal.getCrop modal
                        |> Expect.equal { x = 0.2, y = 0.3, width = 0.5, height = 0.4 }
            , test "setAspect adjusts crop for 16:9, 4:3, and 1:1" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.initFraming { filename = "frame.png", previewUrl = Nothing }

                        m169 =
                            SnapshotModal.setAspect SnapshotModal.Aspect16_9 modal

                        m43 =
                            SnapshotModal.setAspect SnapshotModal.Aspect4_3 modal

                        m11 =
                            SnapshotModal.setAspect SnapshotModal.Aspect1_1 modal
                    in
                    Expect.all
                        [ \_ -> SnapshotModal.getCrop m169 |> Expect.equal { x = 0.0, y = 0.0, width = 1.0, height = 1.0 }
                        , \_ -> SnapshotModal.getCrop m43 |> Expect.equal { x = 0.125, y = 0.0, width = 0.75, height = 1.0 }
                        , \_ -> SnapshotModal.getCrop m11 |> Expect.equal { x = (1.0 - 9.0 / 16.0) / 2.0, y = 0.0, width = 9.0 / 16.0, height = 1.0 }
                        ]
                        ()
            , test "resetCrop restores default full frame" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.initFraming { filename = "frame.png", previewUrl = Nothing }
                                |> SnapshotModal.setAspect SnapshotModal.Aspect1_1
                                |> SnapshotModal.resetCrop
                    in
                    SnapshotModal.getCrop modal
                        |> Expect.equal SnapshotModal.defaultCrop
            , test "startRendering transitions from Framing to Rendering" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.initFraming { filename = "frame.png", previewUrl = Nothing }
                                |> SnapshotModal.startRendering
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal False
                        , \m -> SnapshotModal.isRendering m |> Expect.equal True
                        ]
                        modal
            , test "reframe transitions from Preview back to Framing" <|
                \_ ->
                    let
                        modal =
                            readyModel
                                |> SnapshotModal.reframe
                    in
                    SnapshotModal.isFraming modal
                        |> Expect.equal True
            , test "setAspect while in Preview transitions back to Framing with updated aspect crop" <|
                \_ ->
                    let
                        modal =
                            readyModel
                                |> SnapshotModal.setAspect SnapshotModal.Aspect1_1
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal True
                        , \m -> SnapshotModal.getCrop m |> Expect.equal { x = (1.0 - 9.0 / 16.0) / 2.0, y = 0.0, width = 9.0 / 16.0, height = 1.0 }
                        ]
                        modal
            , test "resetCrop while in Preview transitions back to Framing with defaultCrop" <|
                \_ ->
                    let
                        modal =
                            readyModel
                                |> SnapshotModal.setAspect SnapshotModal.Aspect1_1
                                |> SnapshotModal.resetCrop
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal True
                        , \m -> SnapshotModal.getCrop m |> Expect.equal SnapshotModal.defaultCrop
                        ]
                        modal
            , test "setReady in Framing updates previewUrl without transitioning to Preview" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.initFraming { filename = "frame.png", previewUrl = Nothing }
                                |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,rough", filename = "frame.png" }
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal True
                        , \m -> SnapshotModal.isRendering m |> Expect.equal False
                        , \m -> m.previewUrl |> Expect.equal (Just "data:image/png;base64,rough")
                        ]
                        modal
            , test "handleInteraction pan moves crop rectangle" <|
                \_ ->
                    let
                        initial =
                            SnapshotModal.initFraming { filename = "frame.png", previewUrl = Nothing }
                                |> SnapshotModal.setCrop { x = 0.2, y = 0.2, width = 0.5, height = 0.5 }

                        panned =
                            initial
                                |> SnapshotModal.handleInteraction (SnapshotModal.PointerDown 1 { x = 0.4, y = 0.4 })
                                |> SnapshotModal.handleInteraction (SnapshotModal.PointerMove 1 { x = 0.45, y = 0.48 })
                                |> SnapshotModal.handleInteraction (SnapshotModal.PointerUp 1)

                        crop =
                            SnapshotModal.getCrop panned
                    in
                    Expect.all
                        [ \c -> Expect.within (Expect.Absolute 0.001) 0.25 c.x
                        , \c -> Expect.within (Expect.Absolute 0.001) 0.28 c.y
                        , \c -> Expect.within (Expect.Absolute 0.001) 0.5 c.width
                        , \c -> Expect.within (Expect.Absolute 0.001) 0.5 c.height
                        ]
                        crop
            , test "handleInteraction wheel zooms crop rectangle" <|
                \_ ->
                    let
                        initial =
                            SnapshotModal.initFraming { filename = "frame.png", previewUrl = Nothing }
                                |> SnapshotModal.setCrop { x = 0.2, y = 0.2, width = 0.6, height = 0.6 }

                        zoomedIn =
                            initial
                                |> SnapshotModal.handleInteraction (SnapshotModal.Wheel -50.0 { x = 0.5, y = 0.5 })

                        crop =
                            SnapshotModal.getCrop zoomedIn
                    in
                    Expect.all
                        [ \c -> Expect.lessThan 0.6 c.width
                        , \c -> Expect.lessThan 0.6 c.height
                        ]
                        crop
            , test "viewFramingWithLanguage renders framing aspect pills and Render button without 80% or arrows" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "frame.png", previewUrl = Nothing }
                        |> SnapshotModal.viewFramingWithLanguage En testFramingActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q -> Query.has [ Selector.tag "button", Selector.text "Free" ] q
                            , \q -> Query.has [ Selector.tag "button", Selector.text "16:9" ] q
                            , \q -> Query.has [ Selector.tag "button", Selector.text "4:3" ] q
                            , \q -> Query.has [ Selector.tag "button", Selector.text "1:1" ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "shader-preset-glossy") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "shader-preset-used") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "shader-preset-matte") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "quality-preset-fast") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "quality-preset-standard") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "quality-preset-high") ] q
                            , \q -> Query.has [ Selector.tag "button", Selector.text "Render" ] q
                            , \q -> Query.hasNot [ Selector.text "80%" ] q
                            , \q -> Query.hasNot [ Selector.text "60%" ] q
                            , \q -> Query.hasNot [ Selector.text "←" ] q
                            ]
            , test "selecting shader preset updates shader and re-enters framing from preview" <|
                \_ ->
                    let
                        modal =
                            readyModel
                                |> SnapshotModal.handleInteraction (SnapshotModal.SelectShader SnapshotModal.ShaderUsed)
                    in
                    Expect.all
                        [ \m -> SnapshotModal.getShader m |> Expect.equal SnapshotModal.ShaderUsed
                        , \m -> SnapshotModal.isFraming m |> Expect.equal True
                        ]
                        modal
            , test "selecting quality preset updates quality and dimensions" <|
                \_ ->
                    let
                        modal =
                            readyModel
                                |> SnapshotModal.handleInteraction (SnapshotModal.SelectQuality SnapshotModal.QualityHigh)

                        dims =
                            SnapshotModal.qualityDimensions (SnapshotModal.getQuality modal)
                    in
                    Expect.all
                        [ \m -> SnapshotModal.getQuality m |> Expect.equal SnapshotModal.QualityHigh
                        , \_ -> Expect.equal 2560 dims.width
                        , \_ -> Expect.equal 1440 dims.height
                        , \_ -> Expect.equal 45000 dims.timeBudgetMs
                        ]
                        modal
            ]
        ]
