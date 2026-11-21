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
import Test.Runner
import UI.SnapshotModal as SnapshotModal


type TestMsg
    = Save { dataUrl : String, filename : String }
    | Close
    | Reframe
    | SetAspect SnapshotModal.AspectPreset
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
    , onStartRender = Render
    , onReframe = Reframe
    , onSetAspect = SetAspect
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
            , test "the animation subtitle names the size, frame count, length and loop" <|
                \_ ->
                    SnapshotModal.init { filename = "render.webp" }
                        |> SnapshotModal.setMode SnapshotModal.ModeAnimation
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "854 × 480 WebP animation, 48 frames, 6 s, seamless loop" ]
            , test "the Finnish animation subtitle uses a decimal comma and leaves out the loop when not seamless" <|
                \_ ->
                    SnapshotModal.init { filename = "render.webp" }
                        |> SnapshotModal.setMode SnapshotModal.ModeAnimation
                        |> SnapshotModal.setAnimMotion (motor (Just 24) 60 60)
                        |> SnapshotModal.viewWithLanguage Fi testActions
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "854 × 480 WebP-animaatio, 48 kuvaa, 6 s" ]
            , test "snapshotAnimationSubtitle rounds seconds to a tenth in both languages" <|
                \_ ->
                    [ I18n.snapshotAnimationSubtitle Fi { width = 854, height = 480 } { frames = 72, seconds = 6, loop = True }
                    , I18n.snapshotAnimationSubtitle En { width = 854, height = 480 } { frames = 72, seconds = 6, loop = True }
                    , I18n.snapshotAnimationSubtitle Fi { width = 1280, height = 720 } { frames = 48, seconds = 2.5, loop = False }
                    , I18n.snapshotAnimationSubtitle En { width = 1280, height = 720 } { frames = 48, seconds = 7.2000001, loop = False }
                    ]
                        |> Expect.equal
                            [ "854 × 480 WebP-animaatio, 72 kuvaa, 6 s, saumaton silmukka"
                            , "854 × 480 WebP animation, 72 frames, 6 s, seamless loop"
                            , "1280 × 720 WebP-animaatio, 48 kuvaa, 2,5 s"
                            , "1280 × 720 WebP animation, 48 frames, 7.2 s"
                            ]
            ]
        , describe "Animation options"
            [ test "init and initFraming default to Normal, a 6 s turn, 480p, 1× and a turntable" <|
                \_ ->
                    [ SnapshotModal.init { filename = "a.webp" }, SnapshotModal.initFraming { filename = "a.png" } ]
                        |> List.map
                            (\m ->
                                ( ( SnapshotModal.getAnimSmoothness m, SnapshotModal.getAnimTurnTime m )
                                , ( SnapshotModal.getAnimResolution m, SnapshotModal.getAnimSpeed m )
                                , SnapshotModal.getAnimMotion m
                                )
                            )
                        |> Expect.equal
                            (List.repeat 2
                                ( ( SnapshotModal.SmoothNormal, SnapshotModal.Turn6s )
                                , ( SnapshotModal.Res480p, SnapshotModal.Speed1x )
                                , SnapshotModal.AnimTurntable
                                )
                            )
            , test "a still has no animation request" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "a.png" }
                        |> SnapshotModal.getAnimation
                        |> Expect.equal Nothing
            , test "a turntable has the smoothness level's frames over one seamless turn" <|
                \_ ->
                    [ ( SnapshotModal.SmoothQuick, SnapshotModal.Turn6s )
                    , ( SnapshotModal.SmoothNormal, SnapshotModal.Turn6s )
                    , ( SnapshotModal.SmoothSmooth, SnapshotModal.Turn10s )
                    ]
                        |> List.map (\( smooth, turn ) -> clipOf (animModel |> SnapshotModal.setAnimSmoothness smooth |> SnapshotModal.setAnimTurnTime turn))
                        |> Expect.equal
                            [ { frames = 36, playbackSeconds = 6, simulatedSeconds = 6, seamless = True }
                            , { frames = 48, playbackSeconds = 6, simulatedSeconds = 6, seamless = True }
                            , { frames = 96, playbackSeconds = 10, simulatedSeconds = 10, seamless = True }
                            ]
            , test "the turn time does not change a motor clip" <|
                \_ ->
                    animModel
                        |> SnapshotModal.setAnimMotion (motor (Just 6) 30 30)
                        |> SnapshotModal.setAnimTurnTime SnapshotModal.Turn10s
                        |> clipOf
                        |> Expect.equal { frames = 48, playbackSeconds = 6, simulatedSeconds = 6, seamless = True }
            , test "8T→24T at 30 RPM loops its 6 s cycle seamlessly, capped by the smoothness level" <|
                \_ ->
                    [ SnapshotModal.SmoothQuick, SnapshotModal.SmoothNormal, SnapshotModal.SmoothSmooth ]
                        |> List.map (\smooth -> clipOf (animModel |> SnapshotModal.setAnimMotion (motor (Just 6) 30 30) |> SnapshotModal.setAnimSmoothness smooth))
                        |> Expect.equal
                            [ { frames = 36, playbackSeconds = 6, simulatedSeconds = 6, seamless = True }
                            , { frames = 48, playbackSeconds = 6, simulatedSeconds = 6, seamless = True }
                            , { frames = 96, playbackSeconds = 6, simulatedSeconds = 6, seamless = True }
                            ]
            , test "a short cycle plays at most 30 frames a second" <|
                \_ ->
                    animModel
                        |> SnapshotModal.setAnimMotion (motor (Just 2) 30 30)
                        |> SnapshotModal.setAnimSmoothness SnapshotModal.SmoothSmooth
                        |> clipOf
                        |> Expect.equal { frames = 60, playbackSeconds = 2, simulatedSeconds = 2, seamless = True }
            , test "a cycle under 1 s is repeated until it lasts at least 1 s" <|
                \_ ->
                    [ 0.4, 1 / 3, 0.5 ]
                        |> List.map (\c -> clipOf (animModel |> SnapshotModal.setAnimMotion (motor (Just c) 30 30)))
                        |> List.map (\clip -> ( clip.frames, round (clip.simulatedSeconds * 1000), clip.seamless ))
                        |> Expect.equal [ ( 36, 1200, True ), ( 30, 1000, True ), ( 30, 1000, True ) ]
            , test "a cycle over 10 s or an unknown one falls back to about 6 s of whole motor turns, not seamless" <|
                \_ ->
                    [ motor (Just 12) 30 30, motor Nothing 30 30, motor (Just 24) 60 60, motor (Just 57.6) 25 25 ]
                        |> List.map (\m -> clipOf (animModel |> SnapshotModal.setAnimMotion m))
                        |> List.map (\clip -> ( clip.frames, round (clip.playbackSeconds * 1000), ( round (clip.simulatedSeconds * 1000), clip.seamless ) ))
                        |> Expect.equal
                            [ ( 48, 6000, ( 6000, False ) )
                            , ( 48, 6000, ( 6000, False ) )
                            , ( 48, 6000, ( 6000, False ) )
                            , ( 48, 7200, ( 7200, False ) )
                            ]
            , test "an unknown cycle still makes a motor animation with the motor's whole turns" <|
                \_ ->
                    animModel
                        |> SnapshotModal.setAnimMotion (motor Nothing 150 5)
                        |> Expect.all
                            [ \m -> SnapshotModal.slowMotionOffered m |> Expect.equal False
                            , \m -> clipOf m |> Expect.equal { frames = 48, playbackSeconds = 12, simulatedSeconds = 12, seamless = False }
                            ]
            , test "slow motion is hidden for a turntable and for slow parts" <|
                \_ ->
                    [ animModel
                    , animModel |> SnapshotModal.setAnimMotion (motor (Just 6) 30 30)
                    , animModel |> SnapshotModal.setAnimMotion (motor (Just 2) 30 30) |> SnapshotModal.setAnimSmoothness SnapshotModal.SmoothSmooth
                    ]
                        |> List.map SnapshotModal.slowMotionOffered
                        |> Expect.equal [ False, False, False ]
            , test "slow motion is hidden when the frames are already capped by the smoothness level" <|
                \_ ->
                    -- 8T→24T at 90 RPM: a 2 s cycle, the 8T at 90 RPM; Normal caps at 48 frames.
                    animModel
                        |> SnapshotModal.setAnimMotion (motor (Just 2) 90 90)
                        |> SnapshotModal.slowMotionOffered
                        |> Expect.equal False
            , test "slow motion is offered at 90 RPM with Smooth and slows the same cycle down" <|
                \_ ->
                    animModel
                        |> SnapshotModal.setAnimMotion (motor (Just 2) 90 90)
                        |> SnapshotModal.setAnimSmoothness SnapshotModal.SmoothSmooth
                        |> Expect.all
                            [ \m -> SnapshotModal.slowMotionOffered m |> Expect.equal True
                            , \m -> clipOf m |> Expect.equal { frames = 60, playbackSeconds = 2, simulatedSeconds = 2, seamless = True }
                            , \m ->
                                clipOf (SnapshotModal.setAnimSpeed SnapshotModal.SpeedHalf m)
                                    |> Expect.equal { frames = 96, playbackSeconds = 4, simulatedSeconds = 2, seamless = True }
                            , \m ->
                                SnapshotModal.getAnimation (SnapshotModal.setAnimSpeed SnapshotModal.SpeedHalf m)
                                    |> Expect.equal (Just { frames = 96, durationSeconds = 4, simulatedSeconds = 2, samplesPerPixel = Nothing })
                            ]
            , test "¼× is not offered when ½× already fills the frames, and a ¼× choice then acts as ½×" <|
                \_ ->
                    -- 40T→8T at 30 RPM: a 2 s cycle, the 8T at 150 RPM.
                    animModel
                        |> SnapshotModal.setAnimMotion (motor (Just 2) 150 30)
                        |> SnapshotModal.setAnimSmoothness SnapshotModal.SmoothSmooth
                        |> SnapshotModal.setAnimSpeed SnapshotModal.SpeedQuarter
                        |> Expect.all
                            [ \m -> clipOf m |> Expect.equal { frames = 96, playbackSeconds = 4, simulatedSeconds = 2, seamless = True }
                            , \m -> hasTestId "anim-speed-half" m |> Expect.equal True
                            , \m -> hasTestId "anim-speed-quarter" m |> Expect.equal False
                            ]
            , test "¼× is offered when the ½× clip still has fewer frames than allowed" <|
                \_ ->
                    -- 40T→8T at 60 RPM: a 1 s cycle, the 8T at 300 RPM.
                    animModel
                        |> SnapshotModal.setAnimMotion (motor (Just 1) 300 60)
                        |> SnapshotModal.setAnimSmoothness SnapshotModal.SmoothSmooth
                        |> Expect.all
                            [ \m -> clipOf m |> Expect.equal { frames = 30, playbackSeconds = 1, simulatedSeconds = 1, seamless = True }
                            , \m -> clipOf (SnapshotModal.setAnimSpeed SnapshotModal.SpeedHalf m) |> Expect.equal { frames = 60, playbackSeconds = 2, simulatedSeconds = 1, seamless = True }
                            , \m -> clipOf (SnapshotModal.setAnimSpeed SnapshotModal.SpeedQuarter m) |> Expect.equal { frames = 96, playbackSeconds = 4, simulatedSeconds = 1, seamless = True }
                            , \m -> List.map (\id -> hasTestId id m) [ "anim-speed-1x", "anim-speed-half", "anim-speed-quarter" ] |> Expect.equal [ True, True, True ]
                            ]
            , test "slowing a repeated short cycle keeps the 1× simulated time" <|
                \_ ->
                    -- A 0.4 s cycle (motor gear not animated, output 5× faster) repeats to 1.2 s.
                    animModel
                        |> SnapshotModal.setAnimMotion (motor (Just 0.4) 150 30)
                        |> SnapshotModal.setAnimSpeed SnapshotModal.SpeedHalf
                        |> clipOf
                        |> (\clip -> ( clip.frames, round (clip.playbackSeconds * 1000), ( round (clip.simulatedSeconds * 1000), clip.seamless ) ))
                        |> Expect.equal ( 48, 2400, ( 1200, True ) )
            , test "a ½× choice has no effect when slow motion is not offered" <|
                \_ ->
                    animModel
                        |> SnapshotModal.setAnimMotion (motor (Just 6) 30 30)
                        |> SnapshotModal.setAnimSpeed SnapshotModal.SpeedHalf
                        |> clipOf
                        |> Expect.equal { frames = 48, playbackSeconds = 6, simulatedSeconds = 6, seamless = True }
            , test "a motor animation without slow motion shows neither turn-time nor speed pills" <|
                \_ ->
                    animModel
                        |> SnapshotModal.setAnimMotion (motor (Just 6) 30 30)
                        |> (\m -> List.map (\id -> hasTestId id m) [ "anim-smooth-normal", "anim-turn-6s", "anim-speed-1x", "anim-speed-half" ])
                        |> Expect.equal [ True, False, False, False ]
            , test "720p asks for 10 samples per pixel, 480p leaves it to the renderer" <|
                \_ ->
                    [ SnapshotModal.Res720p, SnapshotModal.Res480p ]
                        |> List.map (\res -> animModel |> SnapshotModal.setAnimResolution res |> SnapshotModal.getAnimation |> Maybe.andThen .samplesPerPixel)
                        |> Expect.equal [ Just 10, Nothing ]
            , test "setAnimMotion keeps a finished animation on screen" <|
                \_ ->
                    readyModel
                        |> SnapshotModal.setAnimMotion (motor (Just 6) 30 30)
                        |> SnapshotModal.isFraming
                        |> Expect.equal False
            , test "choosing a smoothness or turn time on a finished animation goes back to framing" <|
                \_ ->
                    [ readyModel |> SnapshotModal.handleInteraction (SnapshotModal.SelectAnimSmoothness SnapshotModal.SmoothQuick)
                    , readyModel |> SnapshotModal.handleInteraction (SnapshotModal.SelectAnimTurnTime SnapshotModal.Turn10s)
                    ]
                        |> List.map SnapshotModal.isFraming
                        |> Expect.equal [ True, True ]
            , test "the camera overlay shows the clip's frame count" <|
                \_ ->
                    animModel
                        |> SnapshotModal.setAnimSmoothness SnapshotModal.SmoothQuick
                        |> SnapshotModal.viewCameraOverlay En cameraActions { width = 1280, height = 800 }
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "854 × 480 WebP animation, 36 frames, 6 s, seamless loop" ]
            , test "motorCycle converts turns, ratio and signed RPM into a MotorCycle" <|
                \_ ->
                    [ SnapshotModal.motorCycle 30 { motorTurns = Just 3, maxSpeedRatio = 2 }
                    , SnapshotModal.motorCycle -15 { motorTurns = Just 3, maxSpeedRatio = 2 }
                    , SnapshotModal.motorCycle 0 { motorTurns = Just 3, maxSpeedRatio = 2 }
                    , SnapshotModal.motorCycle 30 { motorTurns = Nothing, maxSpeedRatio = 0.5 }
                    ]
                        |> Expect.equal
                            [ { cycleSeconds = Just 6, fastestRpm = 60, motorRpm = 30 }
                            , { cycleSeconds = Just 12, fastestRpm = 30, motorRpm = 15 }
                            , { cycleSeconds = Nothing, fastestRpm = 0, motorRpm = 0 }
                            , { cycleSeconds = Nothing, fastestRpm = 15, motorRpm = 30 }
                            ]
            , test "frameTime advances evenly from startTime across simulatedSeconds" <|
                \_ ->
                    let
                        clip =
                            { frames = 20, simulatedSeconds = 1.0 }
                    in
                    Expect.all
                        [ \_ -> List.map (SnapshotModal.frameTime 1.5 clip) [ 0, 10, 20 ] |> Expect.equal [ 1.5, 2.0, 2.5 ]
                        , \_ -> SnapshotModal.frameTime 2.0 { frames = 0, simulatedSeconds = 3.0 } 1 |> Expect.equal 5.0
                        ]
                        ()
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
            [ test "initFraming starts framing the view-shaped frame" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.initFraming { filename = "frame.png" }
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal True
                        , \m -> SnapshotModal.isRendering m |> Expect.equal False
                        , \m -> m.aspect |> Expect.equal SnapshotModal.AspectFree
                        ]
                        modal
            , test "setAspect sets the frame's aspect while framing" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "frame.png" }
                        |> SnapshotModal.setAspect SnapshotModal.Aspect4_3
                        |> Expect.all
                            [ \m -> m.aspect |> Expect.equal SnapshotModal.Aspect4_3
                            , \m -> SnapshotModal.isFraming m |> Expect.equal True
                            ]
            , test "startRendering transitions from Framing to Rendering" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.initFraming { filename = "frame.png" }
                                |> SnapshotModal.startRendering
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal False
                        , \m -> SnapshotModal.isRendering m |> Expect.equal True
                        ]
                        modal
            , test "reframe goes from Preview back to Framing and keeps the chosen aspect" <|
                \_ ->
                    let
                        modal =
                            readyModel
                                |> SnapshotModal.setAspect SnapshotModal.Aspect1_1
                                |> SnapshotModal.startRendering
                                |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,mock", filename = "test.png" }
                                |> SnapshotModal.reframe
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal True
                        , \m -> m.aspect |> Expect.equal SnapshotModal.Aspect1_1
                        ]
                        modal
            , test "setAspect while in Preview goes back to Framing with the new aspect" <|
                \_ ->
                    let
                        modal =
                            readyModel
                                |> SnapshotModal.setAspect SnapshotModal.Aspect1_1
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal True
                        , \m -> m.aspect |> Expect.equal SnapshotModal.Aspect1_1
                        ]
                        modal
            , test "switching to animation and back keeps the still's aspect" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "frame.png" }
                        |> SnapshotModal.setAspect SnapshotModal.Aspect4_3
                        |> SnapshotModal.setMode SnapshotModal.ModeAnimation
                        |> SnapshotModal.setMode SnapshotModal.ModeStill
                        |> .aspect
                        |> Expect.equal SnapshotModal.Aspect4_3
            , test "setReady in Framing updates previewUrl without transitioning to Preview" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.initFraming { filename = "frame.png" }
                                |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,rough", filename = "frame.png" }
                    in
                    Expect.all
                        [ \m -> SnapshotModal.isFraming m |> Expect.equal True
                        , \m -> SnapshotModal.isRendering m |> Expect.equal False
                        , \m -> m.previewUrl |> Expect.equal (Just "data:image/png;base64,rough")
                        ]
                        modal
            , test "viewFramingWithLanguage renders framing aspect pills and Render button without 80% or arrows" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "frame.png" }
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
                                |> SnapshotModal.handleInteraction (SnapshotModal.SelectAnimSmoothness SnapshotModal.SmoothSmooth)
                                |> SnapshotModal.handleInteraction (SnapshotModal.SelectAnimTurnTime SnapshotModal.Turn10s)

                        dims =
                            SnapshotModal.effectiveDimensions modal

                        anim =
                            SnapshotModal.getAnimation modal
                    in
                    Expect.all
                        [ \m -> SnapshotModal.getMode m |> Expect.equal SnapshotModal.ModeAnimation
                        , \m -> SnapshotModal.getAnimResolution m |> Expect.equal SnapshotModal.Res720p
                        , \m -> SnapshotModal.getAnimSmoothness m |> Expect.equal SnapshotModal.SmoothSmooth
                        , \m -> SnapshotModal.getAnimTurnTime m |> Expect.equal SnapshotModal.Turn10s
                        , \m -> SnapshotModal.effectiveFilename m |> Expect.equal "test.webp"
                        , \_ -> Expect.equal 1280 dims.width
                        , \_ -> Expect.equal 720 dims.height
                        , \_ -> Expect.equal 60000 dims.timeBudgetMs
                        , \_ -> Expect.equal (Just { frames = 96, durationSeconds = 10, simulatedSeconds = 10, samplesPerPixel = Just 10 }) anim
                        , \m -> SnapshotModal.isFraming m |> Expect.equal True
                        ]
                        modal
            , test "viewFramingWithLanguage in animation mode shows resolution, smoothness and turn-time pills for a turntable" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "frame.png" }
                        |> SnapshotModal.setMode SnapshotModal.ModeAnimation
                        |> SnapshotModal.viewFramingWithLanguage En testFramingActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "mode-still") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "mode-anim") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-res-720p") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-res-480p") ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-res-360p") ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-fps-10") ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-fps-30") ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-dur-2s") ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-dur-6s") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-smooth-quick") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-smooth-normal") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-smooth-smooth") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-turn-6s") ] q
                            , \q -> Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-turn-10s") ] q
                            , \q -> Query.has [ Selector.text "6 s turn" ] q
                            , \q -> Query.has [ Selector.text "Normal" ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-speed-1x") ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-speed-half") ] q
                            , \q -> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "anim-speed-quarter") ] q
                            , \q -> Query.hasNot [ Selector.text "View" ] q
                            , \q -> Query.hasNot [ Selector.text "16:9" ] q
                            , \q -> Query.hasNot [ Selector.text "4:3" ] q
                            , \q -> Query.hasNot [ Selector.text "1:1" ] q
                            ]
            , test "the preview image can be dragged out of the browser" <|
                \_ ->
                    SnapshotModal.viewFramingWithLanguage En testFramingActions readyModel
                        |> Query.fromHtml
                        |> Query.find [ Selector.tag "img", Selector.attribute (HtmlAttr.attribute "draggable" "true") ]
                        |> Query.has [ Selector.attribute (HtmlAttr.style "pointer-events" "auto") ]
            ]
        , describe "Camera overlay"
            [ test "a 16:9 frame letterboxes a wide view between the bars, centred" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "r.png" }
                        |> SnapshotModal.setAspect SnapshotModal.Aspect16_9
                        |> SnapshotModal.frameRect { width = 1280, height = 800 }
                        |> roundRect
                        -- 76 px top bar, 88 px bottom bar: 636 px high, 1130.7 wide.
                        |> Expect.equal (roundRect { x = 74.67 / 1280, y = 76 / 800, width = 1130.67 / 1280, height = 636 / 800 })
            , test "a 1:1 frame on a tall view fills the width" <|
                \_ ->
                    let
                        rect =
                            SnapshotModal.initFraming { filename = "r.png" }
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
                    SnapshotModal.initFraming { filename = "r.png" }
                        |> SnapshotModal.frameRect { width = 1000, height = 1000 }
                        |> roundRect
                        |> Expect.equal (roundRect { x = 0.016, y = 0.076, width = 0.968, height = 0.836 })
            , test "a still keeps the quality preset's long side and the frame's aspect" <|
                \_ ->
                    let
                        framing =
                            SnapshotModal.initFraming { filename = "r.png" }

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
                    SnapshotModal.initFraming { filename = "r.png" }
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
                    SnapshotModal.initFraming { filename = "r.png" }
                        |> SnapshotModal.setAspect SnapshotModal.Aspect16_9
                        |> SnapshotModal.viewCameraOverlay En cameraActions { width = 1280, height = 800 }
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-frame") ]
                            , Query.has [ Selector.text "1920 × 1080 PNG of the current view" ]
                            , Query.has [ Selector.text "View" ]
                            , Query.find [ Selector.class "snapshot-hint-mouse" ]
                                >> Query.has [ Selector.text "Drag to orbit · scroll to zoom · right-drag to pan" ]
                            , Query.find [ Selector.class "snapshot-hint-touch" ]
                                >> Query.has [ Selector.text "One finger orbits · pinch to zoom · two fingers pan" ]
                            , Query.has [ Selector.attribute (HtmlAttr.style "pointer-events" "none") ]
                            , Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-render") ]
                                >> Event.simulate Event.click
                                >> Event.expect Render
                            ]
            , test "rendering blocks input, hides the settings and shows progress in the frame" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "r.png" }
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
                    SnapshotModal.initFraming { filename = "r.png" }
                        |> SnapshotModal.setAspect SnapshotModal.Aspect1_1
                        |> SnapshotModal.startCameraRendering { width = 1280, height = 800 }
                        |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,abc", filename = "r.png" }
                        |> SnapshotModal.outputSize
                        |> Expect.equal { width = 1920, height = 1920 }
            , test "preview integrates image in frame, hides toolbar, shows dimensions/badge and Save/Reframe/Close actions" <|
                \_ ->
                    let
                        rendered =
                            SnapshotModal.initFraming { filename = "r.png" }
                                |> SnapshotModal.setAspect SnapshotModal.Aspect1_1
                                |> SnapshotModal.startCameraRendering { width = 1280, height = 800 }
                                |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,result", filename = "r.png" }

                        html =
                            SnapshotModal.viewCameraOverlay En cameraActions { width = 1280, height = 800 } rendered
                    in
                    Query.fromHtml html
                        |> Expect.all
                            [ Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-frame") ]
                                >> Expect.all
                                    [ Query.has
                                        [ Selector.tag "img"
                                        , Selector.attribute (HtmlAttr.src "data:image/png;base64,result")
                                        , Selector.attribute (HtmlAttr.style "object-fit" "contain")
                                        , Selector.attribute (HtmlAttr.style "width" "100%")
                                        , Selector.attribute (HtmlAttr.style "height" "100%")
                                        , Selector.attribute (HtmlAttr.attribute "draggable" "true")
                                        ]
                                    , Query.hasNot [ Selector.attribute (HtmlAttr.attribute "role" "progressbar") ]
                                    ]
                            , Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-settings") ]
                                >> Expect.all
                                    [ Query.has [ Selector.text "Snapshot" ]
                                    , Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-dimensions"), Selector.text "1920 × 1920" ]
                                    , Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-format"), Selector.text "PNG" ]
                                    , Query.hasNot [ Selector.text "View" ]
                                    , Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "shader-preset-glossy") ]
                                    , Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "quality-preset-standard") ]
                                    ]
                            , Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-actions") ]
                                >> Expect.all
                                    [ Query.has [ Selector.tag "button", Selector.text "Save Image" ]
                                    , Query.has [ Selector.tag "button", Selector.text "Reframe" ]
                                    , Query.has [ Selector.tag "button", Selector.text "Close" ]
                                    , Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-render") ]
                                    ]
                            , Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-save") ]
                                >> Event.simulate Event.click
                                >> Event.expect (Save { dataUrl = "data:image/png;base64,result", filename = "r.png" })
                            , Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-reframe") ]
                                >> Event.simulate Event.click
                                >> Event.expect Reframe
                            , Query.has [ Selector.attribute (HtmlAttr.style "pointer-events" "auto") ]
                            ]
            , test "animation preview and markSaved update overlay badge and save button state" <|
                \_ ->
                    let
                        renderedAnim =
                            animModel
                                |> SnapshotModal.startCameraRendering { width = 1280, height = 800 }
                                |> SnapshotModal.setReady { dataUrl = "data:image/webp;base64,anim", filename = "a.webp" }
                                |> SnapshotModal.markSaved
                    in
                    SnapshotModal.viewCameraOverlay En cameraActions { width = 1280, height = 800 } renderedAnim
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-frame") ]
                                >> Query.has [ Selector.tag "img", Selector.attribute (HtmlAttr.src "data:image/webp;base64,anim") ]
                            , Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-settings") ]
                                >> Expect.all
                                    [ Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-dimensions"), Selector.text "854 × 480" ]
                                    , Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-format"), Selector.text "WebP" ]
                                    ]
                            , Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-actions") ]
                                >> Expect.all
                                    [ Query.has [ Selector.text "Saved Image" ]
                                    , Query.has [ Selector.text "Saved to downloads" ]
                                    ]
                            ]
            , test "failed render integrates error card in frame with Reframe and Close, without Save or toolbar" <|
                \_ ->
                    SnapshotModal.initFraming { filename = "r.png" }
                        |> SnapshotModal.startCameraRendering { width = 1280, height = 800 }
                        |> SnapshotModal.setFailed "Out of memory"
                        |> SnapshotModal.viewCameraOverlay En cameraActions { width = 1280, height = 800 }
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-frame") ]
                                >> Query.has [ Selector.text "Out of memory" ]
                            , Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-settings") ]
                                >> Expect.all
                                    [ Query.has [ Selector.text "Snapshot" ]
                                    , Query.hasNot [ Selector.text "View" ]
                                    ]
                            , Query.find [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-actions") ]
                                >> Expect.all
                                    [ Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-reframe") ]
                                    , Query.has [ Selector.tag "button", Selector.text "Close" ]
                                    , Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-testid" "snapshot-camera-save") ]
                                    ]
                            , Query.has [ Selector.attribute (HtmlAttr.style "pointer-events" "auto") ]
                            ]
            ]
        ]


{-| A framing modal in animation mode.
-}
animModel : SnapshotModal.Model
animModel =
    SnapshotModal.initFraming { filename = "a.png" }
        |> SnapshotModal.setMode SnapshotModal.ModeAnimation


motor : Maybe Float -> Float -> Float -> SnapshotModal.AnimMotion
motor cycleSeconds fastestRpm motorRpm =
    SnapshotModal.AnimMotor { cycleSeconds = cycleSeconds, fastestRpm = fastestRpm, motorRpm = motorRpm }


{-| The clip, with its times rounded to the millisecond so float noise does not matter.
-}
clipOf : SnapshotModal.Model -> SnapshotModal.AnimClip
clipOf modal =
    let
        clip =
            SnapshotModal.animationClip modal

        ms x =
            toFloat (round (x * 1000)) / 1000
    in
    { clip | playbackSeconds = ms clip.playbackSeconds, simulatedSeconds = ms clip.simulatedSeconds }


hasTestId : String -> SnapshotModal.Model -> Bool
hasTestId testId modal =
    let
        html =
            SnapshotModal.viewFramingWithLanguage En testFramingActions modal
    in
    case Query.has [ Selector.attribute (HtmlAttr.attribute "data-testid" testId) ] (Query.fromHtml html) |> Test.Runner.getFailureReason of
        Nothing ->
            True

        Just _ ->
            False


cameraActions : SnapshotModal.CameraActions TestMsg
cameraActions =
    { onSave = Save
    , onClose = Close
    , onStartRender = Render
    , onReframe = Reframe
    , onSetAspect = SetAspect
    , onInteraction = Interaction
    }


roundRect : SnapshotModal.CropRect -> List Int
roundRect r =
    List.map (\v -> round (v * 10000)) [ r.x, r.y, r.width, r.height ]
