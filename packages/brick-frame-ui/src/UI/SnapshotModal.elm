module UI.SnapshotModal exposing
    ( Actions
    , AnimClip
    , AnimMotion(..)
    , AnimRequest
    , AnimResolution(..)
    , AnimSmoothness(..)
    , AnimSpeed(..)
    , AnimTurnTime(..)
    , AspectPreset(..)
    , CameraActions
    , CropRect
    , FramingActions
    , InteractionMsg(..)
    , Model
    , MotorCycle
    , OutputMode(..)
    , Phase(..)
    , PreviewData
    , Progress
    , ProgressDetail
    , QualityPreset(..)
    , RenderStage(..)
    , ShaderPreset(..)
    , animDimensions
    , animSamplesPerPixel
    , animSmoothnessFrames
    , animSpeedMultiplier
    , animTurnSeconds
    , animationClip
    , defaultCrop
    , effectiveDimensions
    , effectiveFilename
    , frameOutputSize
    , frameRect
    , frameTime
    , getAnimMotion
    , getAnimResolution
    , getAnimSmoothness
    , getAnimSpeed
    , getAnimTurnTime
    , getAnimation
    , getMode
    , getQuality
    , getShader
    , handleInteraction
    , init
    , initFraming
    , isFraming
    , isRendering
    , markSaved
    , motorCycle
    , outputSize
    , progressDetailDecoder
    , qualityDimensions
    , reframe
    , setAnimMotion
    , setAnimResolution
    , setAnimSmoothness
    , setAnimSpeed
    , setAnimTurnTime
    , setAspect
    , setFailed
    , setMode
    , setProgress
    , setProgressValue
    , setQuality
    , setReady
    , setShader
    , shaderToString
    , slowMotionOffered
    , startCameraRendering
    , startRendering
    , view
    , viewCameraOverlay
    , viewFramingWithLanguage
    , viewWithLanguage
    )

{-| Framing, rendering, previewing and saving a path-traced snapshot (PNG) or
animation (WebP) of the 3D view.

Framing is a camera overlay over the live view (`viewCameraOverlay`): the host
keeps moving the camera as in the design view, and the chosen aspect
letterboxes the view (`frameRect`). The finished image is shown in a modal
dialog (`viewFramingWithLanguage`) with Save and Reframe.

-}

import Html exposing (Html, button, div, h2, img, p, span, text)
import Html.Attributes as Attr
import Html.Events
import I18n exposing (Language(..))
import Json.Decode as Decode
import UI.Button as Button
import UI.Icon as Icon
import UI.Theme as Theme


{-| How far the path tracer has got: `fraction` from 0 to 1, and the number of
samples taken so far.
-}
type alias Progress =
    { fraction : Float
    , samples : Int
    }


{-| What the renderer is doing right now: building the scene, path tracing,
denoising and encoding a frame, or (animations) assembling the WebP file.
-}
type RenderStage
    = StagePreparing
    | StageSampling
    | StageFinishing
    | StageAssembling


{-| The rest of a progress report, beyond `Progress`: the stage, for
animations the 1-based frame number, frame count and target samples per
pixel, and the time elapsed and estimated left in ms (`etaMs` is `Nothing`
until there is something to estimate from).
-}
type alias ProgressDetail =
    { stage : RenderStage
    , frame : Maybe Int
    , totalFrames : Maybe Int
    , targetSamples : Maybe Int
    , elapsedMs : Maybe Int
    , etaMs : Maybe Int
    }


initialProgressDetail : ProgressDetail
initialProgressDetail =
    { stage = StagePreparing
    , frame = Nothing
    , totalFrames = Nothing
    , targetSamples = Nothing
    , elapsedMs = Nothing
    , etaMs = Nothing
    }


{-| Data for the preview phase once the captured image is ready.
-}
type alias PreviewData =
    { dataUrl : String
    , filename : String
    , hasSaved : Bool
    }


{-| Normalized crop rectangle `[0, 1]` in viewport coordinates.
-}
type alias CropRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


{-| Default full-frame crop.
-}
defaultCrop : CropRect
defaultCrop =
    { x = 0.0
    , y = 0.0
    , width = 1.0
    , height = 1.0
    }


{-| Pre-render aspect ratio presets.
-}
type AspectPreset
    = AspectFree
    | Aspect16_9
    | Aspect4_3
    | Aspect1_1


{-| Surface finish / shader preset for the path tracer.
-}
type ShaderPreset
    = ShaderGlossy
    | ShaderMatte


{-| Convert shader preset to contract identifier string.
-}
shaderToString : ShaderPreset -> String
shaderToString preset =
    case preset of
        ShaderGlossy ->
            "glossy"

        ShaderMatte ->
            "matte"


{-| Quality / resolution preset for rendering.
-}
type QualityPreset
    = QualityFast
    | QualityStandard
    | QualityHigh


{-| Dimensions and time budget in ms for quality presets.
-}
qualityDimensions : QualityPreset -> { width : Int, height : Int, timeBudgetMs : Int }
qualityDimensions quality =
    case quality of
        QualityFast ->
            { width = 1280, height = 720, timeBudgetMs = 8000 }

        QualityStandard ->
            { width = 1920, height = 1080, timeBudgetMs = 20000 }

        QualityHigh ->
            { width = 2560, height = 1440, timeBudgetMs = 60000 }


{-| Snapshot output format: Still PNG image or Animated WebP.
-}
type OutputMode
    = ModeStill
    | ModeAnimation


{-| Fixed resolution presets for animations.
-}
type AnimResolution
    = Res720p
    | Res480p


{-| How many frames an animation may have, which sets its render time:
36, 48 or 96 frames (`animSmoothnessFrames`).
-}
type AnimSmoothness
    = SmoothQuick
    | SmoothNormal
    | SmoothSmooth


{-| How long one turn of a turntable animation takes.
-}
type AnimTurnTime
    = Turn6s
    | Turn10s


{-| Slow-motion presets for motor animations (see `slowMotionOffered`).
-}
type AnimSpeed
    = Speed1x
    | SpeedHalf
    | SpeedQuarter


{-| What an animation shows, as told by the host: the camera turning around
the model, or the model's gears turning under its motor.
-}
type AnimMotion
    = AnimTurntable
    | AnimMotor MotorCycle


{-| The motor's motion, from the host:

  - `cycleSeconds` — real time after which every animated part is back where
    it started (`Nothing` when that is too long to compute)
  - `fastestRpm` — the speed of the fastest animated part
  - `motorRpm` — the speed of the motor gear itself, used to make a clip
    that is not seamless at least loop the motor gear cleanly

All speeds are magnitudes.

-}
type alias MotorCycle =
    { cycleSeconds : Maybe Float
    , fastestRpm : Float
    , motorRpm : Float
    }


{-| Convert a gear train's cycle (`Simulation.motionCycle`, or any record with
`motorTurns` and `maxSpeedRatio`) and the motor speed in RPM into the
`MotorCycle` that `setAnimMotion` expects.
-}
motorCycle : Float -> { a | motorTurns : Maybe Float, maxSpeedRatio : Float } -> MotorCycle
motorCycle rpm cycle =
    let
        speedRpm =
            abs rpm
    in
    { cycleSeconds =
        cycle.motorTurns
            |> Maybe.andThen
                (\turns ->
                    if speedRpm > 0 then
                        Just (turns * 60 / speedRpm)

                    else
                        Nothing
                )
    , fastestRpm = speedRpm * abs cycle.maxSpeedRatio
    , motorRpm = speedRpm
    }


{-| The animation the current settings make (`animationClip`):

  - `frames` — the number of frames to render
  - `playbackSeconds` — how long the WebP takes to play once
  - `simulatedSeconds` — how much motor time the frames cover; frame `i`
    shows `start + i × simulatedSeconds / frames`. For a turntable this
    equals the playback time.
  - `seamless` — whether the last frame flows into the first

-}
type alias AnimClip =
    { frames : Int
    , playbackSeconds : Float
    , simulatedSeconds : Float
    , seamless : Bool
    }


{-| What the host needs to render the animation (`getAnimation`): the clip's
frame count, playback length (`durationSeconds`) and simulated time, and an
explicit sample count per pixel (`Nothing` leaves it to the renderer).
-}
type alias AnimRequest =
    { frames : Int
    , durationSeconds : Float
    , simulatedSeconds : Float
    , samplesPerPixel : Maybe Int
    }


{-| Convert speed preset to speed multiplier.
-}
animSpeedMultiplier : AnimSpeed -> Float
animSpeedMultiplier speed =
    case speed of
        Speed1x ->
            1.0

        SpeedHalf ->
            0.5

        SpeedQuarter ->
            0.25


{-| The most frames an animation of this smoothness may have.
-}
animSmoothnessFrames : AnimSmoothness -> Int
animSmoothnessFrames smoothness =
    case smoothness of
        SmoothQuick ->
            36

        SmoothNormal ->
            48

        SmoothSmooth ->
            96


{-| One turn of a turntable animation, in seconds.
-}
animTurnSeconds : AnimTurnTime -> Int
animTurnSeconds turnTime =
    case turnTime of
        Turn6s ->
            6

        Turn10s ->
            10


{-| The explicit samples per pixel sent for an animation: 720p gets 10, as
480p gets from the renderer's own size rule (which would give 720p only 6).
-}
animSamplesPerPixel : AnimResolution -> Maybe Int
animSamplesPerPixel res =
    case res of
        Res720p ->
            Just 10

        Res480p ->
            Nothing


{-| Calculate pixel dimensions for an animation based on resolution preset (widescreen 16:9).
-}
animDimensions : AnimResolution -> { width : Int, height : Int }
animDimensions res =
    case res of
        Res720p ->
            { width = 1280, height = 720 }

        Res480p ->
            { width = 854, height = 480 }


{-| The phases of snapshot capture: framing, rendering, preview, or error.
-}
type Phase
    = Framing
    | Rendering
    | Preview PreviewData
    | Failed String


{-| A setting chosen in the toolbar.
-}
type InteractionMsg
    = SelectShader ShaderPreset
    | SelectQuality QualityPreset
    | SelectOutputMode OutputMode
    | SelectAnimResolution AnimResolution
    | SelectAnimSmoothness AnimSmoothness
    | SelectAnimTurnTime AnimTurnTime
    | SelectAnimSpeed AnimSpeed


{-| Snapshot modal state.
-}
type alias Model =
    { filename : String
    , previewUrl : Maybe String
    , crop : CropRect
    , aspect : AspectPreset
    , shader : ShaderPreset
    , quality : QualityPreset
    , mode : OutputMode
    , animRes : AnimResolution
    , animSmoothness : AnimSmoothness
    , animTurnTime : AnimTurnTime
    , animSpeed : AnimSpeed
    , animMotion : AnimMotion
    , phase : Phase
    , progress : Progress
    , progressDetail : ProgressDetail
    , cameraView : Maybe { width : Int, height : Int }
    }


{-| Basic actions triggered from the modal.
-}
type alias Actions msg =
    { onSave : { dataUrl : String, filename : String } -> msg
    , onClose : msg
    }


{-| Actions of the modal dialog (`viewFramingWithLanguage`).
-}
type alias FramingActions msg =
    { onSave : { dataUrl : String, filename : String } -> msg
    , onClose : msg
    , onStartRender : msg
    , onReframe : msg
    , onSetAspect : AspectPreset -> msg
    , onInteraction : InteractionMsg -> msg
    }


{-| Initialize modal directly in the rendering phase (backward compatible).
-}
init : { filename : String } -> Model
init config =
    { filename = config.filename
    , previewUrl = Nothing
    , crop = defaultCrop
    , aspect = AspectFree
    , shader = ShaderGlossy
    , quality = QualityStandard
    , mode = ModeStill
    , animRes = Res480p
    , animSmoothness = SmoothNormal
    , animTurnTime = Turn6s
    , animSpeed = Speed1x
    , animMotion = AnimTurntable
    , phase = Rendering
    , progress = { fraction = 0, samples = 0 }
    , progressDetail = initialProgressDetail
    , cameraView = Nothing
    }


{-| Initialize modal in the pre-render framing phase.
-}
initFraming : { filename : String } -> Model
initFraming config =
    { filename = config.filename
    , previewUrl = Nothing
    , crop = defaultCrop
    , aspect = AspectFree
    , shader = ShaderGlossy
    , quality = QualityStandard
    , mode = ModeStill
    , animRes = Res480p
    , animSmoothness = SmoothNormal
    , animTurnTime = Turn6s
    , animSpeed = Speed1x
    , animMotion = AnimTurntable
    , phase = Framing
    , progress = { fraction = 0, samples = 0 }
    , progressDetail = initialProgressDetail
    , cameraView = Nothing
    }


{-| Whether the modal is in the framing phase.
-}
isFraming : Model -> Bool
isFraming model =
    case model.phase of
        Framing ->
            True

        _ ->
            False


{-| Whether the modal is waiting for the path tracer.
-}
isRendering : Model -> Bool
isRendering model =
    case model.phase of
        Rendering ->
            True

        _ ->
            False


{-| Transition to the rendering phase.
-}
startRendering : Model -> Model
startRendering model =
    { model
        | phase = Rendering
        , progress = { fraction = 0, samples = 0 }
        , progressDetail = initialProgressDetail
    }


{-| Return from a finished or failed render to framing, keeping the settings.
-}
reframe : Model -> Model
reframe model =
    { model | phase = Framing, crop = defaultCrop }


{-| Set the aspect of the camera frame (see `frameRect`). On a finished image
this goes back to framing so it can be rendered again.
-}
setAspect : AspectPreset -> Model -> Model
setAspect aspect model =
    case model.phase of
        Preview _ ->
            { model | aspect = aspect, phase = Framing }

        _ ->
            { model | aspect = aspect }


{-| Record the path tracer's progress from a host that sends only
`{ fraction, samples }`: the stage reads as path tracing, with no frame or
timing details. Prefer `setProgressValue`.
-}
setProgress : Progress -> Model -> Model
setProgress progress model =
    let
        detail =
            model.progressDetail
    in
    recordProgress progress { detail | stage = StageSampling } model


recordProgress : Progress -> ProgressDetail -> Model -> Model
recordProgress progress detail model =
    case model.phase of
        Preview _ ->
            model

        _ ->
            { model
                | progress =
                    { fraction = clamp 0 1 progress.fraction
                    , samples = max 0 progress.samples
                    }
                , progressDetail = detail
            }


{-| Record a full progress report as the `snapshotProgress` port sends it:
`{ fraction, samples }` plus the optional `stage`, `frame`, `totalFrames`,
`targetSamples`, `elapsedMs` and `etaMs`. A report that does not decode
leaves the model unchanged.
-}
setProgressValue : Decode.Value -> Model -> Model
setProgressValue value model =
    case Decode.decodeValue (Decode.map2 Tuple.pair progressDecoder progressDetailDecoder) value of
        Ok ( progress, detail ) ->
            recordProgress progress detail model

        Err _ ->
            model


progressDecoder : Decode.Decoder Progress
progressDecoder =
    Decode.map2 Progress
        (Decode.field "fraction" Decode.float)
        (Decode.field "samples" Decode.int)


{-| Decodes the detail fields of a progress report. Missing or null fields
become `Nothing`, and a missing or unknown `stage` counts as sampling.
-}
progressDetailDecoder : Decode.Decoder ProgressDetail
progressDetailDecoder =
    let
        optionalInt name =
            Decode.maybe (Decode.field name (Decode.map round Decode.float))

        stageDecoder =
            Decode.maybe (Decode.field "stage" Decode.string)
                |> Decode.map
                    (\stage ->
                        case stage of
                            Just "preparing" ->
                                StagePreparing

                            Just "finishing" ->
                                StageFinishing

                            Just "assembling" ->
                                StageAssembling

                            _ ->
                                StageSampling
                    )
    in
    Decode.map6 ProgressDetail
        stageDecoder
        (optionalInt "frame")
        (optionalInt "totalFrames")
        (optionalInt "targetSamples")
        (optionalInt "elapsedMs")
        (optionalInt "etaMs")


{-| Size in pixels of the image the renderer produces for the current
settings. A crop keeps the longer side of the preset and follows the crop's
own aspect ratio, as the path tracer does (never below 64 px).
-}
outputSize : Model -> { width : Int, height : Int }
outputSize model =
    case model.cameraView of
        Just viewSize ->
            frameOutputSize viewSize model

        Nothing ->
            cropOutputSize model


cropOutputSize : Model -> { width : Int, height : Int }
cropOutputSize model =
    let
        dims =
            effectiveDimensions model

        cropW =
            clamp 0.01 1 model.crop.width

        cropH =
            clamp 0.01 1 model.crop.height

        rawW =
            toFloat dims.width * cropW

        rawH =
            toFloat dims.height * cropH

        scale =
            toFloat (max dims.width dims.height) / max rawW rawH
    in
    { width = max 64 (round (rawW * scale))
    , height = max 64 (round (rawH * scale))
    }


{-| Show error message on render failure.
-}
setFailed : String -> Model -> Model
setFailed message model =
    case model.phase of
        Preview _ ->
            model

        _ ->
            { model | phase = Failed message }


{-| Transition to the preview phase when the render is ready. While framing,
the image is kept as the rough preview and the modal stays in framing.
-}
setReady : { dataUrl : String, filename : String } -> Model -> Model
setReady { dataUrl, filename } model =
    case model.phase of
        Framing ->
            { model
                | filename = filename
                , previewUrl = Just dataUrl
            }

        Rendering ->
            { model
                | filename = filename
                , previewUrl =
                    case model.previewUrl of
                        Just u ->
                            Just u

                        Nothing ->
                            Just dataUrl
                , phase =
                    Preview
                        { dataUrl = dataUrl
                        , filename = filename
                        , hasSaved = False
                        }
            }

        Preview data ->
            { model
                | filename = filename
                , previewUrl =
                    case model.previewUrl of
                        Just u ->
                            Just u

                        Nothing ->
                            Just dataUrl
                , phase =
                    Preview
                        { dataUrl = dataUrl
                        , filename = filename
                        , hasSaved = False
                        }
            }

        Failed _ ->
            { model | filename = filename, previewUrl = Just dataUrl }


{-| Mark the image as saved.
-}
markSaved : Model -> Model
markSaved model =
    case model.phase of
        Preview data ->
            { model | phase = Preview { data | hasSaved = True } }

        _ ->
            model


{-| Set the active shader / surface finish preset.
If in the preview phase, switches back to framing so the user can re-render.
-}
setShader : ShaderPreset -> Model -> Model
setShader preset model =
    case model.phase of
        Preview _ ->
            { model
                | shader = preset
                , phase = Framing
            }

        _ ->
            { model | shader = preset }


{-| Get the currently selected shader preset.
-}
getShader : Model -> ShaderPreset
getShader model =
    model.shader


{-| Set the active quality / resolution preset.
If in the preview phase, switches back to framing so the user can re-render.
-}
setQuality : QualityPreset -> Model -> Model
setQuality preset model =
    case model.phase of
        Preview _ ->
            { model
                | quality = preset
                , phase = Framing
            }

        _ ->
            { model | quality = preset }


{-| Get the currently selected quality preset.
-}
getQuality : Model -> QualityPreset
getQuality model =
    model.quality


{-| Get the current output mode (still vs animation).
-}
getMode : Model -> OutputMode
getMode model =
    model.mode


{-| Set the output mode (Still vs Animation).
If in preview phase, switches back to framing so the user can re-render.
-}
setMode : OutputMode -> Model -> Model
setMode mode model =
    let
        updated =
            case mode of
                ModeAnimation ->
                    { model | mode = ModeAnimation }

                ModeStill ->
                    { model | mode = ModeStill }
    in
    case model.phase of
        Preview _ ->
            { updated | phase = Framing }

        _ ->
            updated


{-| Get the active animation resolution preset.
-}
getAnimResolution : Model -> AnimResolution
getAnimResolution model =
    model.animRes


{-| Set the active animation resolution preset.
-}
setAnimResolution : AnimResolution -> Model -> Model
setAnimResolution res model =
    let
        updated =
            { model | animRes = res }
    in
    case model.phase of
        Preview _ ->
            { updated | phase = Framing }

        _ ->
            updated


{-| Get the active animation smoothness preset.
-}
getAnimSmoothness : Model -> AnimSmoothness
getAnimSmoothness model =
    model.animSmoothness


{-| Set the active animation smoothness preset.
-}
setAnimSmoothness : AnimSmoothness -> Model -> Model
setAnimSmoothness smoothness model =
    let
        updated =
            { model | animSmoothness = smoothness }
    in
    case model.phase of
        Preview _ ->
            { updated | phase = Framing }

        _ ->
            updated


{-| Get the active turntable turn time preset.
-}
getAnimTurnTime : Model -> AnimTurnTime
getAnimTurnTime model =
    model.animTurnTime


{-| Set the active turntable turn time preset.
-}
setAnimTurnTime : AnimTurnTime -> Model -> Model
setAnimTurnTime turnTime model =
    let
        updated =
            { model | animTurnTime = turnTime }
    in
    case model.phase of
        Preview _ ->
            { updated | phase = Framing }

        _ ->
            updated


{-| What the animation shows, as last told by the host.
-}
getAnimMotion : Model -> AnimMotion
getAnimMotion model =
    model.animMotion


{-| Tell the modal what an animation would show (host information: the
motor's cycle). A host sets it when the user switches to animation mode and
again at render, so it is only computed for animations. Unlike the setters
for the user's choices, this keeps the phase, so a finished animation stays
on screen.
-}
setAnimMotion : AnimMotion -> Model -> Model
setAnimMotion motion model =
    { model | animMotion = motion }


{-| Get the active slow-motion preset as chosen (see `animationClip` for the one in effect).
-}
getAnimSpeed : Model -> AnimSpeed
getAnimSpeed model =
    model.animSpeed


{-| Set the active slow-motion preset.
-}
setAnimSpeed : AnimSpeed -> Model -> Model
setAnimSpeed speed model =
    let
        updated =
            { model | animSpeed = speed }
    in
    case model.phase of
        Preview _ ->
            { updated | phase = Framing }

        _ ->
            updated


{-| Effective rendering dimensions and time budget based on current mode and settings.
An animation's render time follows its frame count and size, not a budget:
the renderer ignores `timeBudgetMs` for animations, so it is a constant here.
-}
effectiveDimensions : Model -> { width : Int, height : Int, timeBudgetMs : Int }
effectiveDimensions model =
    case model.mode of
        ModeStill ->
            qualityDimensions model.quality

        ModeAnimation ->
            let
                dims =
                    animDimensions model.animRes
            in
            { width = dims.width
            , height = dims.height
            , timeBudgetMs = animationTimeBudgetMs
            }


animationTimeBudgetMs : Int
animationTimeBudgetMs =
    60000


{-| Get the animation to render, or `Nothing` for a still.
-}
getAnimation : Model -> Maybe AnimRequest
getAnimation model =
    case model.mode of
        ModeStill ->
            Nothing

        ModeAnimation ->
            let
                clip =
                    animationClip model
            in
            Just
                { frames = clip.frames
                , durationSeconds = clip.playbackSeconds
                , simulatedSeconds = clip.simulatedSeconds
                , samplesPerPixel = animSamplesPerPixel model.animRes
                }


{-| Simulation time of 0-based frame `index` starting from `startTime`, matching
`AnimClip`: `startTime + index × simulatedSeconds / frames`.
-}
frameTime : Float -> { a | frames : Int, simulatedSeconds : Float } -> Int -> Float
frameTime startTime clip index =
    startTime + toFloat index * clip.simulatedSeconds / toFloat (max 1 clip.frames)



-- ── Animation clip rules ──────────────────────────────────────────────────────


{-| Highest frame rate of a motor animation; short loops play at this rate.
-}
maxAnimFps : Float
maxAnimFps =
    30


{-| Longest seamless motor loop played at 1×; longer cycles fall back.
-}
seamlessMaxSeconds : Float
seamlessMaxSeconds =
    10


{-| Longest playback of a seamless loop in slow motion (the renderer's maximum).
-}
slowSeamlessMaxSeconds : Float
slowSeamlessMaxSeconds =
    30


{-| Simulated length of a motor clip whose cycle is too long to loop.
-}
fallbackClipSeconds : Float
fallbackClipSeconds =
    6


{-| A motor cycle shorter than this is repeated until it is at least this long.
-}
minCycleSeconds : Float
minCycleSeconds =
    1


{-| Above this step per frame (of the fastest part) slow motion is offered.
-}
slowMotionDegreesPerFrame : Float
slowMotionDegreesPerFrame =
    15


{-| The animation the current settings make.

A turntable has the smoothness level's frame count over one turn, and always
loops seamlessly.

A motor animation at 1× plays its cycle in real time when the cycle is known
and at most `seamlessMaxSeconds` long (a cycle under 1 s is repeated a whole
number of times to reach 1 s), so it loops seamlessly. Otherwise it falls back
to about `fallbackClipSeconds`, rounded to a whole number of motor turns so
at least the motor gear loops cleanly. The frame count is the playback time
at `maxAnimFps`, capped by the smoothness level.

In slow motion (only when offered, see `slowMotionOffered`) the same
simulated time plays `1 / multiplier` times as long, with more frames.

-}
animationClip : Model -> AnimClip
animationClip model =
    case model.animMotion of
        AnimTurntable ->
            let
                seconds =
                    toFloat (animTurnSeconds model.animTurnTime)
            in
            { frames = animSmoothnessFrames model.animSmoothness
            , playbackSeconds = seconds
            , simulatedSeconds = seconds
            , seamless = True
            }

        AnimMotor cycle ->
            motorClip (animSmoothnessFrames model.animSmoothness)
                cycle
                (animSpeedMultiplier (effectiveSpeed (animSmoothnessFrames model.animSmoothness) cycle model.animSpeed))


{-| The motor clip at slow-motion factor `s` (1 for real time), for at most
`maxFrames` frames.
-}
motorClip : Int -> MotorCycle -> Float -> AnimClip
motorClip maxFrames cycle s =
    let
        withFrames playback simulated seamless =
            { frames = clamp 1 maxFrames (round (maxAnimFps * playback))
            , playbackSeconds = playback
            , simulatedSeconds = simulated
            , seamless = seamless
            }
    in
    case seamlessCycle cycle of
        Just c ->
            withFrames (c / s) c True

        Nothing ->
            let
                -- In practice `s` is always 1 here: a fallback clip lasts
                -- about 6 s, so it is capped by the smoothness level and
                -- slow motion is never offered for it.
                simulated =
                    if cycle.motorRpm > 0 then
                        let
                            turnSeconds =
                                60 / cycle.motorRpm

                            turns =
                                max 1 (round (fallbackClipSeconds * s / turnSeconds))
                        in
                        toFloat turns * turnSeconds

                    else
                        fallbackClipSeconds * s
            in
            withFrames (simulated / s) simulated False


{-| The simulated length of a seamless loop at 1×, or `Nothing` when the
cycle is unknown or longer than `seamlessMaxSeconds`. A cycle under
`minCycleSeconds` is repeated the fewest whole times that reach it.
-}
seamlessCycle : MotorCycle -> Maybe Float
seamlessCycle cycle =
    case cycle.cycleSeconds of
        Just c ->
            if c <= 0 then
                Nothing

            else
                let
                    repeats =
                        max 1 (ceiling ((minCycleSeconds - 1.0e-9) / c))

                    stretched =
                        toFloat repeats * c
                in
                if stretched <= seamlessMaxSeconds then
                    Just stretched

                else
                    Nothing

        Nothing ->
            Nothing


{-| The slow-motion presets worth offering for this motor motion, slowest
last. ½× is offered when the fastest part moves more than
`slowMotionDegreesPerFrame` per frame at 1× and the 1× clip has fewer frames
than the smoothness level allows, so slowing it down adds frames. ¼× is
offered only when the ½× clip still has fewer frames than allowed. A seamless
loop stays seamless: a multiplier is offered only while its playback stays
within `slowSeamlessMaxSeconds`.
-}
offeredSlowSpeeds : Int -> MotorCycle -> List AnimSpeed
offeredSlowSpeeds maxFrames cycle =
    let
        realTime =
            motorClip maxFrames cycle 1

        degreesPerFrame =
            cycle.fastestRpm * 6 * realTime.simulatedSeconds / toFloat realTime.frames

        fits clip =
            not clip.seamless || clip.playbackSeconds <= slowSeamlessMaxSeconds

        half =
            motorClip maxFrames cycle 0.5

        quarter =
            motorClip maxFrames cycle 0.25
    in
    if degreesPerFrame > slowMotionDegreesPerFrame && realTime.frames < maxFrames && fits half then
        if half.frames < maxFrames && fits quarter then
            [ SpeedHalf, SpeedQuarter ]

        else
            [ SpeedHalf ]

    else
        []


{-| The slow-motion preset in effect: the chosen one when it is offered, ½×
when ¼× was chosen but only ½× is offered, and 1× otherwise.
-}
effectiveSpeed : Int -> MotorCycle -> AnimSpeed -> AnimSpeed
effectiveSpeed maxFrames cycle chosen =
    let
        offered =
            offeredSlowSpeeds maxFrames cycle
    in
    case chosen of
        Speed1x ->
            Speed1x

        SpeedHalf ->
            if List.member SpeedHalf offered then
                SpeedHalf

            else
                Speed1x

        SpeedQuarter ->
            if List.member SpeedQuarter offered then
                SpeedQuarter

            else if List.member SpeedHalf offered then
                SpeedHalf

            else
                Speed1x


{-| Whether the slow-motion presets are shown: only for a motor animation
whose fastest part jumps more than `slowMotionDegreesPerFrame` per frame at
1× and where slowing down would add frames (see `animationClip`).
-}
slowMotionOffered : Model -> Bool
slowMotionOffered model =
    not (List.isEmpty (slowSpeedsFor model))


slowSpeedsFor : Model -> List AnimSpeed
slowSpeedsFor model =
    case model.animMotion of
        AnimTurntable ->
            []

        AnimMotor cycle ->
            offeredSlowSpeeds (animSmoothnessFrames model.animSmoothness) cycle


{-| The slow-motion preset shown as active.
-}
activeSpeedFor : Model -> AnimSpeed
activeSpeedFor model =
    case model.animMotion of
        AnimTurntable ->
            Speed1x

        AnimMotor cycle ->
            effectiveSpeed (animSmoothnessFrames model.animSmoothness) cycle model.animSpeed


{-| The subtitle facts of the current animation.
-}
clipSummary : Model -> { frames : Int, seconds : Float, loop : Bool }
clipSummary model =
    let
        clip =
            animationClip model
    in
    { frames = clip.frames, seconds = clip.playbackSeconds, loop = clip.seamless }


{-| Ensure the filename has the correct extension (.png for still, .webp for animation).
-}
effectiveFilename : Model -> String
effectiveFilename model =
    case model.mode of
        ModeStill ->
            if String.endsWith ".webp" model.filename then
                String.dropRight 5 model.filename ++ ".png"

            else if String.endsWith ".png" model.filename then
                model.filename

            else
                model.filename ++ ".png"

        ModeAnimation ->
            if String.endsWith ".png" model.filename then
                String.dropRight 4 model.filename ++ ".webp"

            else if String.endsWith ".webp" model.filename then
                model.filename

            else
                model.filename ++ ".webp"


{-| Apply a setting chosen in the toolbar. On a finished image this goes back
to framing so it can be rendered again.
-}
handleInteraction : InteractionMsg -> Model -> Model
handleInteraction msg model =
    case msg of
        SelectShader preset ->
            setShader preset model

        SelectQuality preset ->
            setQuality preset model

        SelectOutputMode mode ->
            setMode mode model

        SelectAnimResolution res ->
            setAnimResolution res model

        SelectAnimSmoothness smoothness ->
            setAnimSmoothness smoothness model

        SelectAnimTurnTime turnTime ->
            setAnimTurnTime turnTime model

        SelectAnimSpeed speed ->
            setAnimSpeed speed model


{-| Render the modal overlay with English defaults.
-}
view : Actions msg -> Model -> Html msg
view =
    viewWithLanguage En


{-| Backward-compatible view with default actions.
-}
viewWithLanguage : Language -> Actions msg -> Model -> Html msg
viewWithLanguage lang actions model =
    viewFramingWithLanguage lang
        { onSave = actions.onSave
        , onClose = actions.onClose
        , onStartRender = actions.onClose
        , onReframe = actions.onClose
        , onSetAspect = \_ -> actions.onClose
        , onInteraction = \_ -> actions.onClose
        }
        model


{-| Render the modal overlay with framing actions.
-}
viewFramingWithLanguage : Language -> FramingActions msg -> Model -> Html msg
viewFramingWithLanguage lang actions model =
    div
        [ Attr.attribute "role" "dialog"
        , Attr.attribute "aria-modal" "true"
        , Attr.attribute "aria-label" (I18n.snapshotTitle lang)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "z-index" "99999"
        , Attr.style "background" scrim
        , Attr.style "backdrop-filter" "blur(12px)"
        , Attr.style "-webkit-backdrop-filter" "blur(12px)"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "padding" "20px"
        , Attr.style "box-sizing" "border-box"
        ]
        [ viewStyles
        , div
            [ Attr.style "background" Theme.panelSurface
            , Attr.style "color" Theme.textPrimary
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "12px"
            , Attr.style "box-shadow" ("0 25px 50px -12px " ++ shadowColour)
            , Attr.style "max-width" "840px"
            , Attr.style "width" "100%"
            , Attr.style "max-height" "calc(100vh - 40px)"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "overflow" "hidden"
            ]
            [ viewHeader lang model
            , viewBody lang actions model
            , viewFooter lang actions model
            ]
        ]


scrim : String
scrim =
    "color-mix(in srgb, " ++ Theme.brand ++ " 85%, transparent)"


previewBackground : String
previewBackground =
    "var(--color-bg-dark)"


shadowColour : String
shadowColour =
    "color-mix(in srgb, " ++ Theme.brand ++ " 75%, transparent)"


viewStyles : Html msg
viewStyles =
    Html.node "style"
        []
        [ text """
@keyframes snapshotSpinAnim {
    from { transform: rotate(0deg); }
    to { transform: rotate(360deg); }
}
.snapshot-camera-scroll div { flex-wrap: nowrap !important; }
.snapshot-camera-scroll button { white-space: nowrap; flex: none; }
.snapshot-camera-scroll { scrollbar-width: none; }
.snapshot-camera-scroll::-webkit-scrollbar { display: none; }
.snapshot-hint-touch { display: none; }
@media (hover: none) and (pointer: coarse) {
    .snapshot-hint-touch { display: block; }
    .snapshot-hint-mouse { display: none; }
}
"""
        ]


viewHeader : Language -> Model -> Html msg
viewHeader lang model =
    div
        [ Attr.style "padding" "16px 20px"
        , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        ]
        [ div []
            [ h2
                [ Attr.style "margin" "0"
                , Attr.style "font-size" "15px"
                , Attr.style "font-weight" "600"
                , Attr.style "letter-spacing" "-0.01em"
                ]
                [ text
                    (case model.phase of
                        Framing ->
                            I18n.snapshotFramingTitle lang

                        _ ->
                            I18n.snapshotTitle lang
                    )
                ]
            , p
                [ Attr.style "margin" "2px 0 0"
                , Attr.style "font-size" "12px"
                , Attr.style "color" Theme.textMuted
                ]
                [ text
                    (case model.phase of
                        Framing ->
                            I18n.snapshotFramingDesc lang

                        _ ->
                            let
                                size =
                                    outputSize model
                            in
                            case model.mode of
                                ModeAnimation ->
                                    I18n.snapshotAnimationSubtitle lang size (clipSummary model)

                                ModeStill ->
                                    I18n.snapshotStillSubtitle lang size
                    )
                ]
            ]
        , span
            [ Attr.style "font-size" "11px"
            , Attr.style "font-weight" "600"
            , Attr.style "padding" "3px 8px"
            , Attr.style "border-radius" "9999px"
            , Attr.style "background" Theme.panelSubtleBackground
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "color" Theme.textMuted
            ]
            [ text
                (case model.mode of
                    ModeAnimation ->
                        "WebP"

                    ModeStill ->
                        "PNG"
                )
            ]
        ]


viewBody : Language -> FramingActions msg -> Model -> Html msg
viewBody lang actions model =
    div
        [ Attr.style "padding" "20px"
        , Attr.style "overflow-y" "auto"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "14px"
        ]
        [ -- 1. Top Aspect Ratio & Reset Toolbar: ALWAYS present across all phases!
          viewToolbar lang actions model
        , -- 2. Unified 16:9 Interactive Stage: ALWAYS stays at the center across all phases!
          viewStage lang model
        , -- Details row for Preview
          case model.phase of
            Preview data ->
                viewPreviewDetails lang model data

            _ ->
                text ""
        , -- Failure message if Failed
          case model.phase of
            Failed message ->
                viewFailed message

            _ ->
                text ""
        ]


viewToolbar :
    Language
    -> { a | onSetAspect : AspectPreset -> msg, onInteraction : InteractionMsg -> msg }
    -> Model
    -> Html msg
viewToolbar lang actions model =
    let
        activeAspect =
            model.aspect

        activeShader =
            model.shader

        activeQuality =
            model.quality

        activeMode =
            model.mode

        activeRes =
            model.animRes

        activeSmoothness =
            model.animSmoothness

        activeTurnTime =
            model.animTurnTime

        activeSpeed =
            activeSpeedFor model

        slowSpeeds =
            slowSpeedsFor model

        motionPills =
            case model.animMotion of
                AnimTurntable ->
                    [ optionPill "anim-turn-6s" (actions.onInteraction (SelectAnimTurnTime Turn6s)) (I18n.snapshotAnimTurnSeconds lang (animTurnSeconds Turn6s)) (activeTurnTime == Turn6s)
                    , optionPill "anim-turn-10s" (actions.onInteraction (SelectAnimTurnTime Turn10s)) (I18n.snapshotAnimTurnSeconds lang (animTurnSeconds Turn10s)) (activeTurnTime == Turn10s)
                    , toolbarDivider
                    ]

                AnimMotor _ ->
                    if List.isEmpty slowSpeeds then
                        []

                    else
                        optionPill "anim-speed-1x" (actions.onInteraction (SelectAnimSpeed Speed1x)) (I18n.snapshotAnimSpeed1x lang) (activeSpeed == Speed1x)
                            :: List.map
                                (\speed ->
                                    case speed of
                                        SpeedQuarter ->
                                            optionPill "anim-speed-quarter" (actions.onInteraction (SelectAnimSpeed SpeedQuarter)) (I18n.snapshotAnimSpeedQuarter lang) (activeSpeed == SpeedQuarter)

                                        _ ->
                                            optionPill "anim-speed-half" (actions.onInteraction (SelectAnimSpeed SpeedHalf)) (I18n.snapshotAnimSpeedHalf lang) (activeSpeed == SpeedHalf)
                                )
                                slowSpeeds
                            ++ [ toolbarDivider ]
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "gap" "8px"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "flex-wrap" "wrap"
            , Attr.style "gap" "6px"
            ]
            ([ -- 1. Output mode: Still vs Animation
               optionPill "mode-still" (actions.onInteraction (SelectOutputMode ModeStill)) (I18n.snapshotModeStill lang) (activeMode == ModeStill)
             , optionPill "mode-anim" (actions.onInteraction (SelectOutputMode ModeAnimation)) (I18n.snapshotModeAnimation lang) (activeMode == ModeAnimation)
             , toolbarDivider
             ]
                ++ (case activeMode of
                        ModeStill ->
                            [ -- 2. Aspect Ratio presets (Still mode only)
                              aspectPill (actions.onSetAspect AspectFree) (I18n.snapshotAspectView lang) (activeAspect == AspectFree)
                            , aspectPill (actions.onSetAspect Aspect16_9) (I18n.snapshotAspect16_9 lang) (activeAspect == Aspect16_9)
                            , aspectPill (actions.onSetAspect Aspect4_3) (I18n.snapshotAspect4_3 lang) (activeAspect == Aspect4_3)
                            , aspectPill (actions.onSetAspect Aspect1_1) (I18n.snapshotAspect1_1 lang) (activeAspect == Aspect1_1)
                            , toolbarDivider

                            -- 3. Shader presets
                            , optionPill "shader-preset-glossy" (actions.onInteraction (SelectShader ShaderGlossy)) (I18n.snapshotShaderGlossy lang) (activeShader == ShaderGlossy)
                            , optionPill "shader-preset-matte" (actions.onInteraction (SelectShader ShaderMatte)) (I18n.snapshotShaderMatte lang) (activeShader == ShaderMatte)
                            , toolbarDivider

                            -- 4. Quality presets
                            , optionPill "quality-preset-fast" (actions.onInteraction (SelectQuality QualityFast)) (I18n.snapshotQualityFast lang) (activeQuality == QualityFast)
                            , optionPill "quality-preset-standard" (actions.onInteraction (SelectQuality QualityStandard)) (I18n.snapshotQualityStandard lang) (activeQuality == QualityStandard)
                            , optionPill "quality-preset-high" (actions.onInteraction (SelectQuality QualityHigh)) (I18n.snapshotQualityHigh lang) (activeQuality == QualityHigh)
                            ]

                        ModeAnimation ->
                            [ -- 2. Fixed resolutions (Animation mode is 16:9 widescreen)
                              optionPill "anim-res-720p" (actions.onInteraction (SelectAnimResolution Res720p)) (I18n.snapshotAnimRes720p lang) (activeRes == Res720p)
                            , optionPill "anim-res-480p" (actions.onInteraction (SelectAnimResolution Res480p)) (I18n.snapshotAnimRes480p lang) (activeRes == Res480p)
                            , toolbarDivider

                            -- 3. Smoothness (the frame count, so the render time)
                            , optionPill "anim-smooth-quick" (actions.onInteraction (SelectAnimSmoothness SmoothQuick)) (I18n.snapshotAnimSmoothQuick lang) (activeSmoothness == SmoothQuick)
                            , optionPill "anim-smooth-normal" (actions.onInteraction (SelectAnimSmoothness SmoothNormal)) (I18n.snapshotAnimSmoothNormal lang) (activeSmoothness == SmoothNormal)
                            , optionPill "anim-smooth-smooth" (actions.onInteraction (SelectAnimSmoothness SmoothSmooth)) (I18n.snapshotAnimSmoothSmooth lang) (activeSmoothness == SmoothSmooth)
                            , toolbarDivider
                            ]
                                -- 4. Turn time (turntable) or slow motion (motor, when it helps)
                                ++ motionPills
                                ++ [ -- 5. Shader presets
                                     optionPill "shader-preset-glossy" (actions.onInteraction (SelectShader ShaderGlossy)) (I18n.snapshotShaderGlossy lang) (activeShader == ShaderGlossy)
                                   , optionPill "shader-preset-matte" (actions.onInteraction (SelectShader ShaderMatte)) (I18n.snapshotShaderMatte lang) (activeShader == ShaderMatte)
                                   ]
                   )
            )
        ]


toolbarDivider : Html msg
toolbarDivider =
    div
        [ Attr.style "width" "1px"
        , Attr.style "height" "16px"
        , Attr.style "background" Theme.borderDefault
        , Attr.style "margin" "0 2px"
        ]
        []


optionPill : String -> msg -> String -> Bool -> Html msg
optionPill testId msg label active =
    button
        [ Attr.type_ "button"
        , Attr.attribute "data-testid" testId
        , Html.Events.onClick msg
        , Attr.class "ui-button"
        , Attr.style "padding" "4px 10px"
        , Attr.style "font-size" "12px"
        , Attr.style "font-weight" "600"
        , Attr.style "border-radius" "6px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "cursor" "pointer"
        , Attr.style "background"
            (if active then
                Theme.brandYellow

             else
                Theme.panelSubtleBackground
            )
        , Attr.style "color"
            (if active then
                Theme.brand

             else
                Theme.textPrimary
            )
        ]
        [ text label ]


viewStage : Language -> Model -> Html msg
viewStage lang model =
    div
        [ Attr.style "position" "relative"
        , Attr.style "width" "100%"
        , Attr.style "aspect-ratio" "16 / 9"
        , Attr.style "background" previewBackground
        , Attr.style "background-image" "radial-gradient(rgba(255, 255, 255, 0.08) 1px, transparent 1px)"
        , Attr.style "background-size" "16px 16px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "8px"
        , Attr.style "overflow" "hidden"
        , Attr.style "cursor"
            (if isRendering model then
                "wait"

             else
                "default"
            )
        ]
        (case model.phase of
            Preview data ->
                [ img
                    [ Attr.src data.dataUrl
                    , Attr.alt "Snapshot preview"
                    , Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "object-fit" "contain"
                    , Attr.style "display" "block"
                    , Attr.style "pointer-events" "auto"
                    , Attr.attribute "draggable" "true"
                    ]
                    []
                ]

            Rendering ->
                [ viewRoughPreview model.previewUrl lang
                , viewCropBox model.crop
                , viewProgressOverlay lang model
                ]

            _ ->
                [ viewRoughPreview model.previewUrl lang
                , viewCropBox model.crop
                ]
        )


viewRoughPreview : Maybe String -> Language -> Html msg
viewRoughPreview maybeUrl lang =
    case maybeUrl of
        Just url ->
            img
                [ Attr.src url
                , Attr.alt "Framing preview"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "object-fit" "contain"
                , Attr.style "display" "block"
                , Attr.style "pointer-events" "none"
                ]
                []

        Nothing ->
            div
                [ Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "gap" "10px"
                , Attr.style "color" Theme.textMuted
                , Attr.style "font-size" "13px"
                , Attr.style "pointer-events" "none"
                ]
                [ div
                    [ Attr.style "animation" "snapshotSpinAnim 1.2s linear infinite"
                    , Attr.style "display" "flex"
                    , Attr.style "color" Theme.brandYellow
                    ]
                    [ Icon.featherSized 22 "loader" ]
                , text (I18n.snapshotFramingLoading lang)
                ]


{-| The rendered part of the view over the rough preview; nothing when it is
the whole view.
-}
viewCropBox : CropRect -> Html msg
viewCropBox crop =
    if crop.width >= 0.999 && crop.height >= 0.999 then
        text ""

    else
        div
            [ Attr.style "position" "absolute"
            , Attr.style "left" (String.fromFloat (crop.x * 100.0) ++ "%")
            , Attr.style "top" (String.fromFloat (crop.y * 100.0) ++ "%")
            , Attr.style "width" (String.fromFloat (crop.width * 100.0) ++ "%")
            , Attr.style "height" (String.fromFloat (crop.height * 100.0) ++ "%")
            , Attr.style "box-sizing" "border-box"
            , Attr.style "border" ("2px solid " ++ Theme.brandYellow)
            , Attr.style "box-shadow" "0 0 0 9999px rgba(0, 0, 0, 0.55)"
            , Attr.style "pointer-events" "none"
            ]
            []



-- CAMERA OVERLAY


{-| Actions of the camera overlay (`viewCameraOverlay`).
-}
type alias CameraActions msg =
    { onClose : msg
    , onStartRender : msg
    , onSetAspect : AspectPreset -> msg
    , onInteraction : InteractionMsg -> msg
    }


{-| Room the camera overlay keeps clear around the frame for its bars, in CSS
pixels of a view of the given size. Both bars stay one line high at any width
(the settings scroll sideways, the labels truncate), so this does not depend
on how wide the view is.
-}
cameraInsets : { width : Int, height : Int } -> { top : Float, bottom : Float, side : Float }
cameraInsets viewSize =
    let
        h =
            toFloat viewSize.height
    in
    { top = min 76 (h * 0.16)
    , bottom = min 88 (h * 0.18)
    , side = 16
    }


{-| Width-to-height ratio of the camera frame, or `Nothing` for the shape of
the view itself (`AspectFree`, shown as "View"). Animations are always 16:9.
-}
frameAspect : Model -> Maybe Float
frameAspect model =
    case model.mode of
        ModeAnimation ->
            Just (16 / 9)

        ModeStill ->
            case model.aspect of
                AspectFree ->
                    Nothing

                Aspect16_9 ->
                    Just (16 / 9)

                Aspect4_3 ->
                    Just (4 / 3)

                Aspect1_1 ->
                    Just 1


{-| The camera frame over a live view of `width` × `height` CSS pixels, as
fractions of the view from its top-left corner: the largest rectangle of the
frame's aspect that fits between the overlay's bars, centred. This is the
`crop` of the render request, and the view's own aspect is its `viewAspect`.
-}
frameRect : { width : Int, height : Int } -> Model -> CropRect
frameRect viewSize model =
    let
        w =
            toFloat (max 1 viewSize.width)

        h =
            toFloat (max 1 viewSize.height)

        insets =
            cameraInsets viewSize

        availW =
            max 1 (w - 2 * insets.side)

        availH =
            max 1 (h - insets.top - insets.bottom)

        target =
            Maybe.withDefault (availW / availH) (frameAspect model)

        ( fw, fh ) =
            if availW / availH > target then
                ( availH * target, availH )

            else
                ( availW, availW / target )
    in
    { x = (insets.side + (availW - fw) / 2) / w
    , y = (insets.top + (availH - fh) / 2) / h
    , width = fw / w
    , height = fh / h
    }


{-| Output size in pixels of a render of the camera frame over a view of the
given size. A still keeps the quality preset's long side (1280, 1920 or 2560)
and the frame's aspect; an animation has its resolution preset's size.
-}
frameOutputSize : { width : Int, height : Int } -> Model -> { width : Int, height : Int }
frameOutputSize viewSize model =
    case model.mode of
        ModeAnimation ->
            animDimensions model.animRes

        ModeStill ->
            let
                rect =
                    frameRect viewSize model

                aspect =
                    (rect.width * toFloat (max 1 viewSize.width)) / (rect.height * toFloat (max 1 viewSize.height))

                long =
                    (qualityDimensions model.quality).width
            in
            if aspect >= 1 then
                { width = long, height = max 64 (round (toFloat long / aspect)) }

            else
                { width = max 64 (round (toFloat long * aspect)), height = long }


{-| Start rendering the camera frame over a view of the given size: like
`startRendering`, and the modal then describes the output by that frame.
-}
startCameraRendering : { width : Int, height : Int } -> Model -> Model
startCameraRendering viewSize model =
    let
        rendering =
            startRendering model
    in
    { rendering | crop = frameRect viewSize model, cameraView = Just viewSize }


{-| The camera overlay, drawn over the live 3D view while framing and while
rendering: the view outside the frame is dimmed, the frame has a thin border
and thirds, and floating bars hold the settings, the output size and the
Render and Close buttons.

The overlay lets pointer input through to the view, so the host keeps
orbiting, zooming and panning the camera as in the design view (and should
route that input to the camera only). While rendering it blocks input and
shows the progress inside the frame. `view` is the size of the 3D view in CSS
pixels, which must fill the window under the overlay.

-}
viewCameraOverlay : Language -> CameraActions msg -> { width : Int, height : Int } -> Model -> Html msg
viewCameraOverlay lang actions viewSize model =
    let
        rect =
            frameRect viewSize model

        rendering =
            isRendering model

        pct v =
            String.fromFloat (v * 100) ++ "%"

        mask attrs =
            div
                ([ Attr.style "position" "absolute"
                 , Attr.style "background" cameraMask
                 , Attr.style "pointer-events" "none"
                 ]
                    ++ attrs
                )
                []

        size =
            frameOutputSize viewSize model

        outputLabel =
            case model.mode of
                ModeAnimation ->
                    I18n.snapshotAnimationSubtitle lang size (clipSummary model)

                ModeStill ->
                    I18n.snapshotStillSubtitle lang size

        thirdLine attrs =
            div
                ([ Attr.style "position" "absolute"
                 , Attr.style "background" "rgba(255, 255, 255, 0.18)"
                 ]
                    ++ attrs
                )
                []
    in
    div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "z-index" "40"
        , Attr.style "pointer-events"
            (if rendering then
                "auto"

             else
                "none"
            )
        , Attr.attribute "role" "region"
        , Attr.attribute "aria-label" (I18n.snapshotCameraTitle lang)
        , Attr.attribute "data-testid" "snapshot-camera"
        ]
        [ viewStyles
        , mask [ Attr.style "top" "0", Attr.style "left" "0", Attr.style "right" "0", Attr.style "height" (pct rect.y) ]
        , mask [ Attr.style "top" (pct (rect.y + rect.height)), Attr.style "left" "0", Attr.style "right" "0", Attr.style "bottom" "0" ]
        , mask [ Attr.style "top" (pct rect.y), Attr.style "height" (pct rect.height), Attr.style "left" "0", Attr.style "width" (pct rect.x) ]
        , mask [ Attr.style "top" (pct rect.y), Attr.style "height" (pct rect.height), Attr.style "left" (pct (rect.x + rect.width)), Attr.style "right" "0" ]
        , div
            [ Attr.style "position" "absolute"
            , Attr.style "left" (pct rect.x)
            , Attr.style "top" (pct rect.y)
            , Attr.style "width" (pct rect.width)
            , Attr.style "height" (pct rect.height)
            , Attr.style "box-sizing" "border-box"
            , Attr.style "border" "1px solid rgba(255, 255, 255, 0.8)"
            , Attr.style "box-shadow" "0 0 0 1px rgba(0, 0, 0, 0.35)"
            , Attr.style "pointer-events" "none"
            , Attr.attribute "data-testid" "snapshot-camera-frame"
            ]
            (if rendering then
                [ viewProgressOverlay lang model ]

             else
                [ thirdLine [ Attr.style "left" "33.333%", Attr.style "top" "0", Attr.style "bottom" "0", Attr.style "width" "1px" ]
                , thirdLine [ Attr.style "left" "66.667%", Attr.style "top" "0", Attr.style "bottom" "0", Attr.style "width" "1px" ]
                , thirdLine [ Attr.style "top" "33.333%", Attr.style "left" "0", Attr.style "right" "0", Attr.style "height" "1px" ]
                , thirdLine [ Attr.style "top" "66.667%", Attr.style "left" "0", Attr.style "right" "0", Attr.style "height" "1px" ]
                ]
            )
        , div
            (cameraBar
                [ Attr.style "top" "12px"
                , Attr.attribute "data-testid" "snapshot-camera-settings"
                ]
            )
            [ div
                [ Attr.style "font-size" "13px"
                , Attr.style "font-weight" "600"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "6px"
                , Attr.style "white-space" "nowrap"
                , Attr.style "flex" "none"
                ]
                [ Icon.featherSized 16 "camera", text (I18n.snapshotCameraTitle lang) ]
            , if rendering then
                text ""

              else
                div
                    [ Attr.class "snapshot-camera-scroll"
                    , Attr.style "min-width" "0"
                    , Attr.style "flex" "1 1 auto"
                    , Attr.style "overflow-x" "auto"
                    ]
                    [ viewToolbar lang actions model ]
            ]
        , div
            (cameraBar
                [ Attr.style "bottom" "12px"
                , Attr.attribute "data-testid" "snapshot-camera-actions"
                ]
            )
            [ div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "2px"
                , Attr.style "min-width" "0"
                , Attr.style "flex" "1 1 auto"
                ]
                [ span (oneLine [ Attr.style "font-size" "13px", Attr.style "font-weight" "600" ]) [ text outputLabel ]
                , if rendering then
                    -- The progress inside the frame says how to cancel.
                    text ""

                  else
                    span [ Attr.style "display" "flex", Attr.style "min-width" "0" ]
                        [ cameraHint "snapshot-hint-mouse" (I18n.snapshotCameraHint lang)
                        , cameraHint "snapshot-hint-touch" (I18n.snapshotCameraTouchHint lang)
                        ]
                ]
            , div [ Attr.style "display" "flex", Attr.style "gap" "8px", Attr.style "margin-left" "auto", Attr.style "flex" "none" ]
                (Button.secondary actions.onClose (I18n.snapshotClose lang)
                    :: (if rendering then
                            []

                        else
                            [ button
                                [ Attr.type_ "button"
                                , Html.Events.onClick actions.onStartRender
                                , Attr.class "ui-button touch-target"
                                , Attr.attribute "data-testid" "snapshot-camera-render"
                                , Attr.style "display" "inline-flex"
                                , Attr.style "align-items" "center"
                                , Attr.style "gap" "8px"
                                , Attr.style "padding" "8px 18px"
                                , Attr.style "border" ("1px solid " ++ Theme.brand)
                                , Attr.style "border-radius" "6px"
                                , Attr.style "font-weight" "600"
                                , Attr.style "font-size" "13px"
                                , Attr.style "cursor" "pointer"
                                , Attr.style "background" Theme.brandYellow
                                , Attr.style "color" Theme.brand
                                ]
                                [ Icon.feather "camera"
                                , text (I18n.snapshotRenderAction lang)
                                ]
                            ]
                       )
                )
            ]
        ]


{-| Text kept to one line, cut with an ellipsis when it does not fit.
-}
oneLine : List (Html.Attribute msg) -> List (Html.Attribute msg)
oneLine attrs =
    [ Attr.style "white-space" "nowrap"
    , Attr.style "overflow" "hidden"
    , Attr.style "text-overflow" "ellipsis"
    ]
        ++ attrs


{-| One of the camera hints; the stylesheet shows the touch one on touch
screens and the mouse one elsewhere.
-}
cameraHint : String -> String -> Html msg
cameraHint class hint =
    span
        (oneLine
            [ Attr.class class
            , Attr.style "font-size" "12px"
            , Attr.style "color" Theme.textMuted
            , Attr.style "min-width" "0"
            , Attr.title hint
            ]
        )
        [ text hint ]


{-| Dim over the view outside the camera frame.
-}
cameraMask : String
cameraMask =
    "rgba(2, 6, 10, 0.72)"


{-| A floating bar of the camera overlay, centred horizontally; `attrs` place it.
-}
cameraBar : List (Html.Attribute msg) -> List (Html.Attribute msg)
cameraBar attrs =
    [ Attr.style "position" "absolute"
    , Attr.style "left" "50%"
    , Attr.style "transform" "translateX(-50%)"
    , Attr.style "width" "max-content"
    , Attr.style "max-width" "calc(100% - 24px)"
    , Attr.style "box-sizing" "border-box"
    , Attr.style "display" "flex"
    , Attr.style "align-items" "center"
    , Attr.style "flex-wrap" "nowrap"
    , Attr.style "gap" "12px"
    , Attr.style "padding" "8px 12px"
    , Attr.style "background" "var(--color-bg-page)"
    , Attr.style "color" "var(--color-text-primary)"
    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
    , Attr.style "border-radius" "10px"
    , Attr.style "box-shadow" "0 10px 30px -10px rgba(0, 0, 0, 0.6)"
    , Attr.style "pointer-events" "auto"
    ]
        ++ attrs


viewProgressOverlay : Language -> Model -> Html msg
viewProgressOverlay lang model =
    let
        progress =
            model.progress

        detail =
            model.progressDetail

        percent =
            round (progress.fraction * 100)

        headline =
            case detail.stage of
                StagePreparing ->
                    I18n.snapshotStagePreparing lang

                StageSampling ->
                    I18n.snapshotStageSampling lang

                StageFinishing ->
                    I18n.snapshotStageFinishing lang

                StageAssembling ->
                    I18n.snapshotStageAssembling lang

        counts =
            case ( detail.frame, detail.totalFrames ) of
                ( Just frame, Just total ) ->
                    if detail.stage == StageAssembling then
                        I18n.snapshotFrameCount lang total

                    else
                        I18n.snapshotFrameOf lang frame total
                            ++ (case detail.targetSamples of
                                    Just target ->
                                        " · " ++ I18n.snapshotSamplesOf lang (min target progress.samples) target

                                    Nothing ->
                                        ""
                               )

                _ ->
                    if detail.stage == StagePreparing then
                        ""

                    else
                        I18n.snapshotSamplesPerPixel lang progress.samples

        timing =
            let
                left =
                    case detail.etaMs of
                        Just ms ->
                            I18n.snapshotTimeLeft lang ms

                        Nothing ->
                            I18n.snapshotEstimatingTime lang
            in
            case detail.elapsedMs of
                Just ms ->
                    left ++ " · " ++ I18n.snapshotElapsed lang ms

                Nothing ->
                    left
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "inset" "0"
        , Attr.style "background" "rgba(0, 0, 0, 0.5)"
        , Attr.style "backdrop-filter" "blur(3px)"
        , Attr.style "-webkit-backdrop-filter" "blur(3px)"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "10px"
        , Attr.style "padding" "20px"
        , Attr.style "z-index" "20"
        , Attr.style "pointer-events" "none"
        , Attr.style "text-align" "center"
        ]
        [ div
            [ Attr.style "animation" "snapshotSpinAnim 1.2s linear infinite"
            , Attr.style "display" "flex"
            , Attr.style "color" Theme.brandYellow
            , Attr.attribute "aria-hidden" "true"
            ]
            [ Icon.featherSized 32 "loader" ]
        , div
            [ Attr.style "font-size" "15px"
            , Attr.style "font-weight" "600"
            , Attr.style "color" "#FFFFFF"
            , Attr.attribute "aria-live" "polite"
            , Attr.attribute "data-testid" "snapshot-stage"
            ]
            [ text headline ]
        , if String.isEmpty counts then
            text ""

          else
            div
                [ Attr.style "font-size" "13px"
                , Attr.style "color" "rgba(255, 255, 255, 0.9)"
                , Attr.style "font-variant-numeric" "tabular-nums"
                , Attr.attribute "data-testid" "snapshot-counts"
                ]
                [ text counts ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "10px"
            , Attr.style "width" "100%"
            , Attr.style "max-width" "420px"
            ]
            [ div
                [ Attr.attribute "role" "progressbar"
                , Attr.attribute "aria-label" (I18n.snapshotProgressLabel lang)
                , Attr.attribute "aria-valuemin" "0"
                , Attr.attribute "aria-valuemax" "100"
                , Attr.attribute "aria-valuenow" (String.fromInt percent)
                , Attr.attribute "aria-valuetext" (String.fromInt percent ++ " % · " ++ headline)
                , Attr.style "flex" "1"
                , Attr.style "height" "8px"
                , Attr.style "background" "rgba(255, 255, 255, 0.25)"
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "border-radius" "9999px"
                , Attr.style "overflow" "hidden"
                ]
                [ div
                    [ Attr.style "width" (String.fromInt percent ++ "%")
                    , Attr.style "height" "100%"
                    , Attr.style "background" Theme.brandYellow
                    , Attr.style "transition" "width 0.3s linear"
                    ]
                    []
                ]
            , span
                [ Attr.style "font-size" "13px"
                , Attr.style "font-weight" "600"
                , Attr.style "color" "#FFFFFF"
                , Attr.style "font-variant-numeric" "tabular-nums"
                , Attr.style "min-width" "3.2em"
                , Attr.style "text-align" "right"
                ]
                [ text (String.fromInt percent ++ " %") ]
            ]
        , div
            [ Attr.style "font-size" "13px"
            , Attr.style "color" "rgba(255, 255, 255, 0.9)"
            , Attr.style "font-variant-numeric" "tabular-nums"
            , Attr.attribute "data-testid" "snapshot-timing"
            ]
            [ text timing ]
        , p
            [ Attr.style "margin" "0"
            , Attr.style "font-size" "12px"
            , Attr.style "color" "rgba(255, 255, 255, 0.7)"
            ]
            [ text (I18n.snapshotCloseCancels lang) ]
        ]


viewPreviewDetails : Language -> Model -> PreviewData -> Html msg
viewPreviewDetails lang model data =
    let
        fmt =
            if String.endsWith ".webp" data.filename || model.mode == ModeAnimation then
                "WebP"

            else
                "PNG"
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "font-size" "12px"
        , Attr.style "color" Theme.textMuted
        ]
        [ div []
            [ text (data.filename ++ " • " ++ fmt) ]
        , if data.hasSaved then
            div
                [ Attr.style "display" "inline-flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "6px"
                , Attr.style "color" Theme.brandYellow
                , Attr.style "font-weight" "500"
                ]
                [ Icon.featherSized 14 "check"
                , text (I18n.snapshotSavedNotice lang)
                ]

          else
            div [] [ text (I18n.snapshotReadyToSave lang) ]
        ]


aspectPill : msg -> String -> Bool -> Html msg
aspectPill msg label active =
    button
        [ Attr.type_ "button"
        , Html.Events.onClick msg
        , Attr.class "ui-button"
        , Attr.style "padding" "4px 10px"
        , Attr.style "font-size" "12px"
        , Attr.style "font-weight" "600"
        , Attr.style "border-radius" "6px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "cursor" "pointer"
        , Attr.style "background"
            (if active then
                Theme.brandYellow

             else
                Theme.panelSubtleBackground
            )
        , Attr.style "color"
            (if active then
                Theme.brand

             else
                Theme.textPrimary
            )
        ]
        [ text label ]


viewFailed : String -> Html msg
viewFailed message =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "padding" "16px"
        , Attr.style "gap" "8px"
        , Attr.style "text-align" "center"
        ]
        [ div
            [ Attr.style "font-size" "15px"
            , Attr.style "font-weight" "600"
            , Attr.style "color" Theme.brandRed
            ]
            [ text "The snapshot could not be rendered." ]
        , p
            [ Attr.style "margin" "0"
            , Attr.style "font-size" "13px"
            , Attr.style "color" Theme.textMuted
            ]
            [ text message ]
        ]


viewFooter : Language -> FramingActions msg -> Model -> Html msg
viewFooter lang actions model =
    div
        [ Attr.style "padding" "14px 20px"
        , Attr.style "border-top" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "gap" "12px"
        , Attr.style "flex-wrap" "wrap"
        ]
        (case model.phase of
            Framing ->
                [ renderButton lang actions
                , Button.secondary actions.onClose (I18n.snapshotClose lang)
                ]

            Rendering ->
                [ div
                    [ Attr.style "font-size" "13px"
                    , Attr.style "color" Theme.textMuted
                    , Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "8px"
                    ]
                    [ div
                        [ Attr.style "animation" "snapshotSpinAnim 1.2s linear infinite"
                        , Attr.style "display" "flex"
                        , Attr.style "color" Theme.brandYellow
                        ]
                        [ Icon.featherSized 16 "loader" ]
                    , text
                        (case model.mode of
                            ModeAnimation ->
                                I18n.snapshotRenderingAnimation lang

                            ModeStill ->
                                case lang of
                                    Fi ->
                                        "Renderöidään tilannekuvaa…"

                                    En ->
                                        "Rendering snapshot…"
                        )
                    ]
                , Button.secondary actions.onClose (I18n.snapshotClose lang)
                ]

            Preview data ->
                [ div
                    [ Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "10px"
                    ]
                    [ saveButton lang actions data
                    , Button.secondary actions.onReframe (I18n.snapshotReframe lang)
                    ]
                , Button.secondary actions.onClose (I18n.snapshotClose lang)
                ]

            Failed _ ->
                [ div
                    [ Attr.style "font-size" "13px"
                    , Attr.style "color" Theme.textMuted
                    ]
                    [ text
                        (case lang of
                            Fi ->
                                "Mitään ei tallennettu."

                            En ->
                                "Nothing was saved."
                        )
                    ]
                , Button.secondary actions.onClose (I18n.snapshotClose lang)
                ]
        )


renderButton : Language -> FramingActions msg -> Html msg
renderButton lang actions =
    button
        [ Attr.type_ "button"
        , Html.Events.onClick actions.onStartRender
        , Attr.class "ui-button touch-target"
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "padding" "8px 18px"
        , Attr.style "border" ("1px solid " ++ Theme.brand)
        , Attr.style "border-radius" "6px"
        , Attr.style "font-weight" "600"
        , Attr.style "font-size" "13px"
        , Attr.style "cursor" "pointer"
        , Attr.style "background" Theme.brandYellow
        , Attr.style "color" Theme.brand
        ]
        [ Icon.feather "camera"
        , text (I18n.snapshotRenderAction lang)
        ]


saveButton : Language -> FramingActions msg -> PreviewData -> Html msg
saveButton lang actions data =
    button
        [ Attr.type_ "button"
        , Html.Events.onClick (actions.onSave { dataUrl = data.dataUrl, filename = data.filename })
        , Attr.class "ui-button touch-target"
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "padding" "8px 16px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "6px"
        , Attr.style "font-weight" "600"
        , Attr.style "font-size" "13px"
        , Attr.style "cursor" "pointer"
        , Attr.style "background"
            (if data.hasSaved then
                Theme.panelSubtleBackground

             else
                Theme.brandYellow
            )
        , Attr.style "color"
            (if data.hasSaved then
                Theme.textPrimary

             else
                Theme.brand
            )
        ]
        [ Icon.feather
            (if data.hasSaved then
                "check"

             else
                "download"
            )
        , text
            (if data.hasSaved then
                I18n.snapshotSavedImage lang

             else
                I18n.snapshotSaveImage lang
            )
        ]
