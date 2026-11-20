module SnapshotModalTest exposing (suite)

{-| Tests for UI.SnapshotModal state transitions and its rendered controls.
-}

import Expect
import Html.Attributes as HtmlAttr
import I18n exposing (Language(..))
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
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
    | Render


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
            [ test "a legacy progress report shows path tracing, the samples and the percentage" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.setProgress { fraction = 0.42, samples = 42 }
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.attribute (HtmlAttr.attribute "role" "dialog") ]
                            , Query.has [ Selector.text "Path tracing…" ]
                            , Query.has [ Selector.text "42 samples per pixel" ]
                            , Query.has [ Selector.attribute (HtmlAttr.attribute "role" "progressbar") ]
                            , Query.has [ Selector.attribute (HtmlAttr.attribute "aria-valuenow" "42") ]
                            , Query.has [ Selector.text "42 %" ]
                            , Query.has [ Selector.text "Estimating time left…" ]
                            , Query.has [ Selector.text "Closing the dialog cancels the render." ]
                            , Query.has [ Selector.tag "button", Selector.text "Close" ]
                            , Query.hasNot [ Selector.text "about 30 seconds" ]
                            , Query.hasNot [ Selector.tag "input" ]
                            ]
            , test "starts by preparing the scene before any progress report" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.text "Preparing the scene…" ]
                            , Query.has [ Selector.text "0 %" ]
                            ]
            , test "a still's full report shows samples, time left and time elapsed" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.setProgressValue
                            (Encode.object
                                [ ( "fraction", Encode.float 0.5 )
                                , ( "samples", Encode.int 12 )
                                , ( "stage", Encode.string "sampling" )
                                , ( "elapsedMs", Encode.int 65000 )
                                , ( "etaMs", Encode.int 200000 )
                                ]
                            )
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.text "Path tracing…" ]
                            , Query.has [ Selector.text "12 samples per pixel" ]
                            , Query.has [ Selector.text "About 3 min 20 s left · 1:05 elapsed" ]
                            , Query.has [ Selector.text "50 %" ]
                            ]
            , test "an animation report shows the frame, its samples and the stage" <|
                \_ ->
                    SnapshotModal.init { filename = "render.webp" }
                        |> SnapshotModal.setProgressValue
                            (Encode.object
                                [ ( "fraction", Encode.float 0.3 )
                                , ( "samples", Encode.int 10 )
                                , ( "stage", Encode.string "finishing" )
                                , ( "frame", Encode.int 7 )
                                , ( "totalFrames", Encode.int 20 )
                                , ( "targetSamples", Encode.int 10 )
                                , ( "elapsedMs", Encode.int 42000 )
                                , ( "etaMs", Encode.int 8000 )
                                ]
                            )
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.text "Removing noise and encoding…" ]
                            , Query.has [ Selector.text "Frame 7 of 20 · 10 of 10 samples" ]
                            , Query.has [ Selector.text "A few seconds left · 0:42 elapsed" ]
                            ]
            , test "assembling shows the frame count and no estimate" <|
                \_ ->
                    SnapshotModal.init { filename = "render.webp" }
                        |> SnapshotModal.setProgressValue
                            (Encode.object
                                [ ( "fraction", Encode.float 1 )
                                , ( "samples", Encode.int 10 )
                                , ( "stage", Encode.string "assembling" )
                                , ( "frame", Encode.int 20 )
                                , ( "totalFrames", Encode.int 20 )
                                , ( "targetSamples", Encode.int 10 )
                                , ( "elapsedMs", Encode.int 125000 )
                                , ( "etaMs", Encode.null )
                                ]
                            )
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.text "Assembling the animation…" ]
                            , Query.has [ Selector.text "20 frames" ]
                            , Query.has [ Selector.text "Estimating time left… · 2:05 elapsed" ]
                            ]
            , test "a report that does not decode leaves the progress unchanged" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.setProgress { fraction = 0.25, samples = 4 }
                        |> SnapshotModal.setProgressValue (Encode.object [ ( "fraction", Encode.string "half" ) ])
                        |> .progress
                        |> Expect.equal { fraction = 0.25, samples = 4 }
            , test "time left is rounded so it does not jitter" <|
                \_ ->
                    [ 4000, 23000, 61000, 125000, 754000 ]
                        |> List.map (I18n.snapshotTimeLeft En)
                        |> Expect.equal
                            [ "A few seconds left"
                            , "About 25 s left"
                            , "About 1 min left"
                            , "About 2 min 10 s left"
                            , "About 13 min left"
                            ]
            , test "the animation subtitle names the size and frame count" <|
                \_ ->
                    SnapshotModal.init { filename = "render.webp" }
                        |> SnapshotModal.setMode SnapshotModal.ModeAnimation
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "854 × 480 WebP animation, 20 frames" ]
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
                        |> Query.has [ Selector.text "1920 × 1080 PNG of the current view" ]
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
                            [ \q -> Query.has [ Selector.tag "button", Selector.text "View" ] q
                            , \q -> Query.has [ Selector.tag "button", Selector.text "16:9" ] q
                            , \q -> Query.has [ Selector.tag "button", Selector.text "4:3" ] q
                            , \q -> Query.has [ Selector.tag "button", Selector.text "1:1" ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "shader-preset-glossy") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "shader-preset-matte") ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "shader-preset-used") ] q
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
                                |> SnapshotModal.handleInteraction (SnapshotModal.SelectShader SnapshotModal.ShaderMatte)
                    in
                    Expect.all
                        [ \m -> SnapshotModal.getShader m |> Expect.equal SnapshotModal.ShaderMatte
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
                        , \_ -> Expect.equal 60000 dims.timeBudgetMs
                        ]
                        modal
            , test "selecting animation mode switches mode, exposes animation parameters, and sets webp filename" <|
                \_ ->
                    let
                        modal =
                            readyModel
                                |> SnapshotModal.handleInteraction (SnapshotModal.SelectOutputMode SnapshotModal.ModeAnimation)
                                |> SnapshotModal.handleInteraction (SnapshotModal.SelectAnimResolution SnapshotModal.Res720p)
                                |> SnapshotModal.handleInteraction (SnapshotModal.SelectAnimFps SnapshotModal.Fps30)
                                |> SnapshotModal.handleInteraction (SnapshotModal.SelectAnimDuration SnapshotModal.Dur6s)

                        dims =
                            SnapshotModal.effectiveDimensions modal

                        anim =
                            SnapshotModal.getAnimation modal
                    in
                    Expect.all
                        [ \m -> SnapshotModal.getMode m |> Expect.equal SnapshotModal.ModeAnimation
                        , \m -> SnapshotModal.getAnimResolution m |> Expect.equal SnapshotModal.Res720p
                        , \m -> SnapshotModal.getAnimFps m |> Expect.equal SnapshotModal.Fps30
                        , \m -> SnapshotModal.getAnimDuration m |> Expect.equal SnapshotModal.Dur6s
                        , \m -> SnapshotModal.effectiveFilename m |> Expect.equal "test.webp"
                        , \_ -> Expect.equal 1280 dims.width
                        , \_ -> Expect.equal 720 dims.height
                        , \_ -> Expect.equal (Just { fps = 30, durationSeconds = 6.0, speedMultiplier = 0.5 }) anim
                        , \m -> SnapshotModal.isFraming m |> Expect.equal True
                        ]
                        modal
            , test "viewFramingWithLanguage in animation mode displays animation resolution, fps, duration, and speed pills" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "frame.png", previewUrl = Nothing }
                        |> SnapshotModal.setMode SnapshotModal.ModeAnimation
                        |> SnapshotModal.viewFramingWithLanguage En testFramingActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "mode-still") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "mode-anim") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-res-720p") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-res-480p") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-res-360p") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-fps-10") ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-fps-15") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-fps-30") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-dur-2s") ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-dur-4s") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-dur-6s") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-speed-1x") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-speed-half") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-speed-quarter") ] q
                            , \q -> Query.hasNot [ Selector.text "View" ] q
                            , \q -> Query.hasNot [ Selector.text "16:9" ] q
                            , \q -> Query.hasNot [ Selector.text "4:3" ] q
                            , \q -> Query.hasNot [ Selector.text "1:1" ] q
                            ]
            , test "animation mode allows panning the zoomed area on pointer drag" <|
                \_ ->
                    let
                        initModel =
                            SnapshotModal.initFraming { filename = "frame.png", previewUrl = Nothing }
                                |> SnapshotModal.setMode SnapshotModal.ModeAnimation
                                |> SnapshotModal.setCrop { x = 0.2, y = 0.2, width = 0.5, height = 0.5 }

                        initialCrop =
                            SnapshotModal.getCrop initModel

                        pannedModel =
                            initModel
                                |> SnapshotModal.handleInteraction (SnapshotModal.PointerDown 1 { x = 0.5, y = 0.5 })
                                |> SnapshotModal.handleInteraction (SnapshotModal.PointerMove 1 { x = 0.6, y = 0.55 })
                                |> SnapshotModal.handleInteraction (SnapshotModal.PointerUp 1)

                        pannedCrop =
                            SnapshotModal.getCrop pannedModel
                    in
                    Expect.all
                        [ \c -> Expect.within (Expect.Absolute 0.001) (initialCrop.x + 0.1) c.x
                        , \c -> Expect.within (Expect.Absolute 0.001) (initialCrop.y + 0.05) c.y
                        , \c -> Expect.within (Expect.Absolute 0.001) initialCrop.width c.width
                        , \c -> Expect.within (Expect.Absolute 0.001) initialCrop.height c.height
                        ]
                        pannedCrop
            , test "preview phase allows browser dragging and does not re-enter framing on pointer down" <|
                \_ ->
                    let
                        modal =
                            readyModel
                                |> SnapshotModal.handleInteraction (SnapshotModal.PointerDown 0 { x = 0.5, y = 0.5 })
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal False
                        , \m ->
                            SnapshotModal.viewFramingWithLanguage En testFramingActions m
                                |> Query.fromHtml
                                |> Query.find [ Selector.tag "img", Selector.attribute (HtmlAttr.attribute "draggable" "true") ]
                                |> Query.has [ Selector.attribute (HtmlAttr.style "pointer-events" "auto") ]
                        ]
                        modal
            ]
        , describe "Camera overlay"
            [ test "a 16:9 frame letterboxes a wide view between the bars, centred" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "r.png", previewUrl = Nothing }
                        |> SnapshotModal.setAspect SnapshotModal.Aspect16_9
                        |> SnapshotModal.frameRect { width = 1280, height = 800 }
                        |> roundRect
                        -- 76 px top bar, 88 px bottom bar: 636 px high, 1130.7 wide.
                        |> Expect.equal (roundRect { x = 74.67 / 1280, y = 76 / 800, width = 1130.67 / 1280, height = 636 / 800 })
            , test "a 1:1 frame on a tall view fills the width" <|
                \_ ->
                    let
                        rect =
                            SnapshotModal.initFraming { filename = "r.png", previewUrl = Nothing }
                                |> SnapshotModal.setAspect SnapshotModal.Aspect1_1
                                |> SnapshotModal.frameRect { width = 390, height = 844 }
                    in
                    Expect.all
                        [ \r -> Expect.within (Expect.Absolute 1.0e-6) (358 / 390) r.width
                        , \r -> Expect.within (Expect.Absolute 1.0e-6) (358 / 844) r.height
                        , \r -> Expect.within (Expect.Absolute 1.0e-6) (16 / 390) r.x
                        ]
                        rect
            , test "the view-shaped frame takes all the room between the bars" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "r.png", previewUrl = Nothing }
                        |> SnapshotModal.frameRect { width = 1000, height = 1000 }
                        |> roundRect
                        |> Expect.equal (roundRect { x = 0.016, y = 0.076, width = 0.968, height = 0.836 })
            , test "a still keeps the quality preset's long side and the frame's aspect" <|
                \_ ->
                    let
                        framing =
                            SnapshotModal.initFraming { filename = "r.png", previewUrl = Nothing }

                        sizeFor aspect quality =
                            framing
                                |> SnapshotModal.setAspect aspect
                                |> SnapshotModal.setQuality quality
                                |> SnapshotModal.frameOutputSize { width = 1280, height = 800 }
                    in
                    [ sizeFor SnapshotModal.Aspect16_9 SnapshotModal.QualityStandard
                    , sizeFor SnapshotModal.Aspect1_1 SnapshotModal.QualityHigh
                    , sizeFor SnapshotModal.Aspect4_3 SnapshotModal.QualityFast
                    ]
                        |> Expect.equal
                            [ { width = 1920, height = 1080 }
                            , { width = 2560, height = 2560 }
                            , { width = 1280, height = 960 }
                            ]
            , test "an animation frame is 16:9 at the resolution preset" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "r.png", previewUrl = Nothing }
                        |> SnapshotModal.setAspect SnapshotModal.Aspect1_1
                        |> SnapshotModal.setMode SnapshotModal.ModeAnimation
                        |> (\modal ->
                                ( SnapshotModal.frameOutputSize { width = 1280, height = 800 } modal
                                , SnapshotModal.frameRect { width = 1280, height = 800 } modal |> (\r -> round (r.width * 1280 / (r.height * 800) * 1000))
                                )
                           )
                        |> Expect.equal ( { width = 854, height = 480 }, 1778 )
            , test "framing shows the frame, the settings and Render, and lets input through" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "r.png", previewUrl = Nothing }
                        |> SnapshotModal.setAspect SnapshotModal.Aspect16_9
                        |> SnapshotModal.viewCameraOverlay En cameraActions { width = 1280, height = 800 }
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-frame") ]
                            , Query.has [ Selector.text "1920 × 1080 PNG of the current view" ]
                            , Query.has [ Selector.text "View" ]
                            , Query.has [ Selector.text "Drag to orbit · scroll to zoom · right-drag or two fingers to pan" ]
                            , Query.has [ Selector.attribute (HtmlAttr.style "pointer-events" "none") ]
                            , Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-render") ]
                                >> Event.simulate Event.click
                                >> Event.expect Render
                            ]
            , test "rendering blocks input, hides the settings and shows progress in the frame" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "r.png", previewUrl = Nothing }
                        |> SnapshotModal.startCameraRendering { width = 1280, height = 800 }
                        |> SnapshotModal.viewCameraOverlay En cameraActions { width = 1280, height = 800 }
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-frame") ]
                                >> Query.has [ Selector.attribute (HtmlAttr.attribute "role" "progressbar") ]
                            , Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-render") ]
                            , Query.hasNot [ Selector.text "View" ]
                            , Query.has [ Selector.attribute (HtmlAttr.style "pointer-events" "auto") ]
                            ]
            , test "after a camera render the result describes the framed output" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "r.png", previewUrl = Nothing }
                        |> SnapshotModal.setAspect SnapshotModal.Aspect1_1
                        |> SnapshotModal.startCameraRendering { width = 1280, height = 800 }
                        |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,abc", filename = "r.png" }
                        |> SnapshotModal.outputSize
                        |> Expect.equal { width = 1920, height = 1920 }
            ]
        ]


cameraActions : SnapshotModal.CameraActions TestMsg
cameraActions =
    { onClose = Close
    , onStartRender = Render
    , onSetAspect = SetAspect
    , onInteraction = Interaction
    }


roundRect : SnapshotModal.CropRect -> List Int
roundRect r =
    List.map (\v -> round (v * 10000)) [ r.x, r.y, r.width, r.height ]
