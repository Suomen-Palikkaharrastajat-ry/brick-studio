module UI.SnapshotModal exposing
    ( Actions
    , AnimDuration(..)
    , AnimFps(..)
    , AnimResolution(..)
    , AspectPreset(..)
    , CropRect
    , DragState(..)
    , FramingActions
    , FramingData
    , GestureState(..)
    , Handle(..)
    , InteractionMsg(..)
    , Model
    , OutputMode(..)
    , Phase(..)
    , Point
    , PreviewData
    , Progress
    , QualityPreset(..)
    , ShaderPreset(..)
    , animDimensions
    , animFpsValue
    , animSecondsValue
    , defaultCrop
    , effectiveDimensions
    , effectiveFilename
    , getAnimDuration
    , getAnimFps
    , getAnimResolution
    , getAnimation
    , getCrop
    , getMode
    , getQuality
    , getShader
    , handleInteraction
    , init
    , initFraming
    , isFraming
    , isRendering
    , markSaved
    , qualityDimensions
    , reframe
    , resetCrop
    , setAnimDuration
    , setAnimFps
    , setAnimResolution
    , setAspect
    , setCrop
    , setFailed
    , setMode
    , setProgress
    , setQuality
    , setReady
    , setShader
    , shaderToString
    , startRendering
    , view
    , viewFramingWithLanguage
    , viewWithLanguage
    )

{-| Full-screen modal for framing, capturing, previewing, and saving a snapshot of
the canvas as a PNG.

The modal provides a single unified dialog where the aspect ratio toolbar, the 16:9
interactive stage, and action controls stay permanently in place across framing,
rendering, and preview phases.

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


{-| Snapshot output format: Still PNG image or Animated WebP turntable.
-}
type OutputMode
    = ModeStill
    | ModeAnimation


{-| Fixed resolution presets for animations.
-}
type AnimResolution
    = Res720p
    | Res480p
    | Res360p


{-| Frame rate presets for animations.
-}
type AnimFps
    = Fps10
    | Fps15
    | Fps30


{-| Animation duration presets.
-}
type AnimDuration
    = Dur2s
    | Dur4s
    | Dur6s


{-| Convert FPS preset to integer value.
-}
animFpsValue : AnimFps -> Int
animFpsValue fps =
    case fps of
        Fps10 ->
            10

        Fps15 ->
            15

        Fps30 ->
            30


{-| Convert duration preset to float seconds.
-}
animSecondsValue : AnimDuration -> Float
animSecondsValue dur =
    case dur of
        Dur2s ->
            2.0

        Dur4s ->
            4.0

        Dur6s ->
            6.0


{-| Calculate pixel dimensions for an animation based on resolution preset (widescreen 16:9).
-}
animDimensions : AnimResolution -> { width : Int, height : Int }
animDimensions res =
    case res of
        Res720p ->
            { width = 1280, height = 720 }

        Res480p ->
            { width = 854, height = 480 }

        Res360p ->
            { width = 640, height = 360 }


{-| State for the interactive pre-render framing phase.
-}
type alias FramingData =
    { previewUrl : Maybe String
    , crop : CropRect
    , aspect : AspectPreset
    }


{-| The phases of snapshot capture: framing, rendering, preview, or error.
-}
type Phase
    = Framing FramingData
    | Rendering
    | Preview PreviewData
    | Failed String


{-| A 2D point normalized in `[0, 1]`.
-}
type alias Point =
    { x : Float
    , y : Float
    }


{-| Resize handles on the crop box border.
-}
type Handle
    = CornerNW
    | CornerNE
    | CornerSW
    | CornerSE
    | EdgeN
    | EdgeS
    | EdgeW
    | EdgeE


{-| Single pointer active drag state.
-}
type DragState
    = HandleDrag Handle Point CropRect
    | PanDrag Point CropRect
    | DrawDrag Point


{-| Active touch / pointer gesture state.
-}
type GestureState
    = SinglePointer Int DragState
    | TwoPointer
        { id1 : Int
        , p1 : Point
        , id2 : Int
        , p2 : Point
        , initDist : Float
        , initCenter : Point
        , initCrop : CropRect
        }


{-| Interactive pointer, gesture, and wheel messages on the framing canvas.
-}
type InteractionMsg
    = PointerDown Int Point
    | PointerMove Int Point
    | PointerUp Int
    | PointerCancel Int
    | Wheel Float Point
    | SelectShader ShaderPreset
    | SelectQuality QualityPreset
    | SelectOutputMode OutputMode
    | SelectAnimResolution AnimResolution
    | SelectAnimFps AnimFps
    | SelectAnimDuration AnimDuration


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
    , animFps : AnimFps
    , animDuration : AnimDuration
    , phase : Phase
    , progress : Progress
    , gesture : Maybe GestureState
    }


{-| Basic actions triggered from the modal.
-}
type alias Actions msg =
    { onSave : { dataUrl : String, filename : String } -> msg
    , onClose : msg
    }


{-| Comprehensive actions for framing and rendering.
-}
type alias FramingActions msg =
    { onSave : { dataUrl : String, filename : String } -> msg
    , onClose : msg
    , onStartRender : CropRect -> msg
    , onReframe : msg
    , onSetCrop : CropRect -> msg
    , onSetAspect : AspectPreset -> msg
    , onResetCrop : msg
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
    , animFps = Fps15
    , animDuration = Dur4s
    , phase = Rendering
    , progress = { fraction = 0, samples = 0 }
    , gesture = Nothing
    }


{-| Initialize modal in the pre-render framing phase.
-}
initFraming : { filename : String, previewUrl : Maybe String } -> Model
initFraming config =
    { filename = config.filename
    , previewUrl = config.previewUrl
    , crop = defaultCrop
    , aspect = AspectFree
    , shader = ShaderGlossy
    , quality = QualityStandard
    , mode = ModeStill
    , animRes = Res480p
    , animFps = Fps15
    , animDuration = Dur4s
    , phase =
        Framing
            { previewUrl = config.previewUrl
            , crop = defaultCrop
            , aspect = AspectFree
            }
    , progress = { fraction = 0, samples = 0 }
    , gesture = Nothing
    }


{-| Whether the modal is in the framing phase.
-}
isFraming : Model -> Bool
isFraming model =
    case model.phase of
        Framing _ ->
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
    { model | phase = Rendering, progress = { fraction = 0, samples = 0 }, gesture = Nothing }


{-| Return from preview back to the framing phase.
-}
reframe : Model -> Model
reframe model =
    let
        pUrl =
            case model.previewUrl of
                Just u ->
                    Just u

                Nothing ->
                    case model.phase of
                        Preview data ->
                            Just data.dataUrl

                        _ ->
                            Nothing
    in
    { model
        | previewUrl = pUrl
        , phase =
            Framing
                { previewUrl = pUrl
                , crop = defaultCrop
                , aspect = AspectFree
                }
        , crop = defaultCrop
        , aspect = AspectFree
        , gesture = Nothing
    }


{-| Get the current crop bounds.
-}
getCrop : Model -> CropRect
getCrop model =
    case model.phase of
        Framing data ->
            data.crop

        _ ->
            model.crop


{-| Set the crop bounds, clamping within [0, 1].
-}
setCrop : CropRect -> Model -> Model
setCrop crop model =
    let
        clampedX =
            clamp 0.0 0.92 crop.x

        clampedY =
            clamp 0.0 0.92 crop.y

        clampedW =
            clamp 0.08 (1.0 - clampedX) crop.width

        clampedH =
            clamp 0.08 (1.0 - clampedY) crop.height

        clamped =
            { x = clampedX
            , y = clampedY
            , width = clampedW
            , height = clampedH
            }
    in
    case model.phase of
        Framing data ->
            { model
                | crop = clamped
                , phase = Framing { data | crop = clamped }
            }

        Preview _ ->
            { model
                | crop = clamped
                , aspect = AspectFree
                , phase =
                    Framing
                        { previewUrl = model.previewUrl
                        , crop = clamped
                        , aspect = AspectFree
                        }
                , gesture = Nothing
            }

        _ ->
            { model | crop = clamped }


{-| Aspect ratio width-to-height ratio in normalized coordinates on a 16:9 canvas.
-}
aspectRatioRatio : AspectPreset -> Maybe Float
aspectRatioRatio aspect =
    case aspect of
        AspectFree ->
            Nothing

        Aspect16_9 ->
            Just 1.0

        Aspect4_3 ->
            Just 0.75

        Aspect1_1 ->
            Just (9.0 / 16.0)


{-| Calculate normalized crop for a specific aspect ratio on a 16:9 canvas.
-}
cropForAspect : AspectPreset -> CropRect -> CropRect
cropForAspect aspect current =
    case aspect of
        AspectFree ->
            current

        Aspect16_9 ->
            defaultCrop

        Aspect4_3 ->
            let
                w =
                    0.75

                h =
                    1.0

                x =
                    (1.0 - w) / 2.0
            in
            { x = x, y = 0.0, width = w, height = h }

        Aspect1_1 ->
            let
                w =
                    9.0 / 16.0

                h =
                    1.0

                x =
                    (1.0 - w) / 2.0
            in
            { x = x, y = 0.0, width = w, height = h }


{-| Set an aspect ratio preset and update the crop box accordingly.
-}
setAspect : AspectPreset -> Model -> Model
setAspect aspect model =
    case model.phase of
        Framing data ->
            let
                newCrop =
                    cropForAspect aspect data.crop
            in
            { model
                | crop = newCrop
                , aspect = aspect
                , phase =
                    Framing
                        { data
                            | aspect = aspect
                            , crop = newCrop
                        }
            }

        Preview _ ->
            let
                newCrop =
                    cropForAspect aspect defaultCrop
            in
            { model
                | crop = newCrop
                , aspect = aspect
                , phase =
                    Framing
                        { previewUrl = model.previewUrl
                        , aspect = aspect
                        , crop = newCrop
                        }
                , gesture = Nothing
            }

        _ ->
            { model | aspect = aspect }


{-| Reset crop to the full viewport frame.
-}
resetCrop : Model -> Model
resetCrop model =
    case model.phase of
        Framing data ->
            { model
                | crop = defaultCrop
                , aspect = AspectFree
                , phase =
                    Framing
                        { data
                            | crop = defaultCrop
                            , aspect = AspectFree
                        }
            }

        Preview _ ->
            { model
                | crop = defaultCrop
                , aspect = AspectFree
                , phase =
                    Framing
                        { previewUrl = model.previewUrl
                        , crop = defaultCrop
                        , aspect = AspectFree
                        }
                , gesture = Nothing
            }

        _ ->
            { model | crop = defaultCrop, aspect = AspectFree }


{-| Record the path tracer's progress.
-}
setProgress : Progress -> Model -> Model
setProgress progress model =
    case model.phase of
        Preview _ ->
            model

        _ ->
            { model
                | progress =
                    { fraction = clamp 0 1 progress.fraction
                    , samples = max 0 progress.samples
                    }
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


{-| Transition to the preview phase when the render is ready.
If the modal is in the Framing phase, the fast rough preview has arrived, so
we retain the Framing phase and save the preview image.
-}
setReady : { dataUrl : String, filename : String } -> Model -> Model
setReady { dataUrl, filename } model =
    case model.phase of
        Framing data ->
            { model
                | filename = filename
                , previewUrl = Just dataUrl
                , phase = Framing { data | previewUrl = Just dataUrl }
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
                , phase =
                    Framing
                        { previewUrl = model.previewUrl
                        , crop = model.crop
                        , aspect = model.aspect
                        }
                , gesture = Nothing
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
                , phase =
                    Framing
                        { previewUrl = model.previewUrl
                        , crop = model.crop
                        , aspect = model.aspect
                        }
                , gesture = Nothing
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
                    { model | mode = ModeAnimation, crop = defaultCrop, aspect = Aspect16_9 }

                ModeStill ->
                    { model | mode = ModeStill }
    in
    case model.phase of
        Preview _ ->
            { updated
                | phase =
                    Framing
                        { previewUrl = model.previewUrl
                        , crop = updated.crop
                        , aspect = updated.aspect
                        }
                , gesture = Nothing
            }

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
            { updated
                | phase =
                    Framing
                        { previewUrl = model.previewUrl
                        , crop = model.crop
                        , aspect = model.aspect
                        }
                , gesture = Nothing
            }

        _ ->
            updated


{-| Get the active animation FPS preset.
-}
getAnimFps : Model -> AnimFps
getAnimFps model =
    model.animFps


{-| Set the active animation FPS preset.
-}
setAnimFps : AnimFps -> Model -> Model
setAnimFps fps model =
    let
        updated =
            { model | animFps = fps }
    in
    case model.phase of
        Preview _ ->
            { updated
                | phase =
                    Framing
                        { previewUrl = model.previewUrl
                        , crop = model.crop
                        , aspect = model.aspect
                        }
                , gesture = Nothing
            }

        _ ->
            updated


{-| Get the active animation duration preset.
-}
getAnimDuration : Model -> AnimDuration
getAnimDuration model =
    model.animDuration


{-| Set the active animation duration preset.
-}
setAnimDuration : AnimDuration -> Model -> Model
setAnimDuration dur model =
    let
        updated =
            { model | animDuration = dur }
    in
    case model.phase of
        Preview _ ->
            { updated
                | phase =
                    Framing
                        { previewUrl = model.previewUrl
                        , crop = model.crop
                        , aspect = model.aspect
                        }
                , gesture = Nothing
            }

        _ ->
            updated


{-| Effective rendering dimensions and time budget based on current mode and settings.
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

                totalFrames =
                    round (animSecondsValue model.animDuration * toFloat (animFpsValue model.animFps))

                budget =
                    clamp 6000 35000 (totalFrames * 250)
            in
            { width = dims.width
            , height = dims.height
            , timeBudgetMs = budget
            }


{-| Get optional animation parameters for the render request.
-}
getAnimation : Model -> Maybe { fps : Int, durationSeconds : Float }
getAnimation model =
    case model.mode of
        ModeStill ->
            Nothing

        ModeAnimation ->
            Just
                { fps = animFpsValue model.animFps
                , durationSeconds = animSecondsValue model.animDuration
                }


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


{-| Handle interactive gesture, drag, resize, pinch, and wheel zoom on the framing canvas.
-}
handleInteraction : InteractionMsg -> Model -> Model
handleInteraction msg model =
    case model.phase of
        Preview _ ->
            case msg of
                SelectShader preset ->
                    setShader preset model

                SelectQuality preset ->
                    setQuality preset model

                SelectOutputMode mode ->
                    setMode mode model

                SelectAnimResolution res ->
                    setAnimResolution res model

                SelectAnimFps fps ->
                    setAnimFps fps model

                SelectAnimDuration dur ->
                    setAnimDuration dur model

                _ ->
                    model

        Framing data ->
            case msg of
                SelectShader preset ->
                    setShader preset model

                SelectQuality preset ->
                    setQuality preset model

                SelectOutputMode mode ->
                    setMode mode model

                SelectAnimResolution res ->
                    setAnimResolution res model

                SelectAnimFps fps ->
                    setAnimFps fps model

                SelectAnimDuration dur ->
                    setAnimDuration dur model

                Wheel deltaY pointer ->
                    if model.mode == ModeAnimation then
                        let
                            scale =
                                clamp 0.85 1.18 (1.0 + deltaY * 0.0015)

                            newDim =
                                clamp 0.2 1.0 (data.crop.width * scale)

                            newX =
                                (1.0 - newDim) / 2.0

                            newY =
                                (1.0 - newDim) / 2.0

                            newCrop =
                                { x = newX, y = newY, width = newDim, height = newDim }
                        in
                        { model
                            | crop = newCrop
                            , phase = Framing { data | crop = newCrop }
                        }

                    else
                        let
                            scale =
                                clamp 0.85 1.18 (1.0 + deltaY * 0.0015)

                            maybeRatio =
                                aspectRatioRatio data.aspect

                            ( targetW, targetH ) =
                                case maybeRatio of
                                    Nothing ->
                                        ( clamp 0.08 1.0 (data.crop.width * scale)
                                        , clamp 0.08 1.0 (data.crop.height * scale)
                                        )

                                    Just ratio ->
                                        let
                                            rawW =
                                                clamp 0.08 (min 1.0 ratio) (data.crop.width * scale)

                                            rawH =
                                                rawW / ratio
                                        in
                                        if rawH > 1.0 then
                                            ( 1.0 * ratio, 1.0 )

                                        else
                                            ( rawW, rawH )

                            relX =
                                clamp 0.0 1.0 ((pointer.x - data.crop.x) / max 0.001 data.crop.width)

                            relY =
                                clamp 0.0 1.0 ((pointer.y - data.crop.y) / max 0.001 data.crop.height)

                            newX =
                                clamp 0.0 (1.0 - targetW) (pointer.x - relX * targetW)

                            newY =
                                clamp 0.0 (1.0 - targetH) (pointer.y - relY * targetH)

                            newCrop =
                                { x = newX, y = newY, width = targetW, height = targetH }
                        in
                        { model
                            | crop = newCrop
                            , phase = Framing { data | crop = newCrop }
                        }

                PointerDown pointerId point ->
                    if model.mode == ModeAnimation then
                        model

                    else
                        case model.gesture of
                        Just (SinglePointer id1 _) ->
                            if pointerId /= id1 then
                                let
                                    p1 =
                                        pointFromCrop data.crop point

                                    dist0 =
                                        max 0.02 (pointDistance p1 point)

                                    center0 =
                                        Point ((p1.x + point.x) / 2.0) ((p1.y + point.y) / 2.0)

                                    two =
                                        TwoPointer
                                            { id1 = id1
                                            , p1 = p1
                                            , id2 = pointerId
                                            , p2 = point
                                            , initDist = dist0
                                            , initCenter = center0
                                            , initCrop = data.crop
                                            }
                                in
                                { model | gesture = Just two }

                            else
                                model

                        _ ->
                            case hitHandle data.crop point of
                                Just handle ->
                                    { model | gesture = Just (SinglePointer pointerId (HandleDrag handle point data.crop)) }

                                Nothing ->
                                    if insideCrop data.crop point then
                                        { model | gesture = Just (SinglePointer pointerId (PanDrag point data.crop)) }

                                    else
                                        { model | gesture = Just (SinglePointer pointerId (DrawDrag point)) }

                PointerMove pointerId point ->
                    case model.gesture of
                        Just (SinglePointer id drag) ->
                            if pointerId == id then
                                case drag of
                                    PanDrag startPoint initCrop ->
                                        let
                                            dx =
                                                point.x - startPoint.x

                                            dy =
                                                point.y - startPoint.y

                                            newX =
                                                clamp 0.0 (1.0 - initCrop.width) (initCrop.x + dx)

                                            newY =
                                                clamp 0.0 (1.0 - initCrop.height) (initCrop.y + dy)

                                            newCrop =
                                                { initCrop | x = newX, y = newY }
                                        in
                                        { model
                                            | crop = newCrop
                                            , phase = Framing { data | crop = newCrop }
                                        }

                                    HandleDrag handle _ initCrop ->
                                        let
                                            newCrop =
                                                resizeCrop handle data.aspect initCrop point
                                        in
                                        { model
                                            | crop = newCrop
                                            , phase = Framing { data | crop = newCrop }
                                        }

                                    DrawDrag startPoint ->
                                        let
                                            minX =
                                                min startPoint.x point.x

                                            maxX =
                                                max startPoint.x point.x

                                            minY =
                                                min startPoint.y point.y

                                            maxY =
                                                max startPoint.y point.y

                                            rawW =
                                                maxX - minX

                                            rawH =
                                                maxY - minY
                                        in
                                        if rawW >= 0.04 && rawH >= 0.04 then
                                            let
                                                maybeRatio =
                                                    aspectRatioRatio data.aspect

                                                newCrop =
                                                    case maybeRatio of
                                                        Nothing ->
                                                            { x = clamp 0.0 0.92 minX
                                                            , y = clamp 0.0 0.92 minY
                                                            , width = clamp 0.08 (1.0 - minX) rawW
                                                            , height = clamp 0.08 (1.0 - minY) rawH
                                                            }

                                                        Just ratio ->
                                                            let
                                                                adjH =
                                                                    rawW / ratio

                                                                finalW =
                                                                    if minY + adjH > 1.0 then
                                                                        (1.0 - minY) * ratio

                                                                    else
                                                                        rawW

                                                                finalH =
                                                                    finalW / ratio
                                                            in
                                                            { x = clamp 0.0 (1.0 - finalW) minX
                                                            , y = clamp 0.0 (1.0 - finalH) minY
                                                            , width = clamp 0.08 (1.0 - minX) finalW
                                                            , height = clamp 0.08 (1.0 - minY) finalH
                                                            }
                                            in
                                            { model
                                                | crop = newCrop
                                                , phase = Framing { data | crop = newCrop }
                                            }

                                        else
                                            model

                            else
                                model

                        Just (TwoPointer two) ->
                            if pointerId == two.id1 || pointerId == two.id2 then
                                let
                                    updatedP1 =
                                        if pointerId == two.id1 then
                                            point

                                        else
                                            two.p1

                                    updatedP2 =
                                        if pointerId == two.id2 then
                                            point

                                        else
                                            two.p2

                                    currDist =
                                        max 0.02 (pointDistance updatedP1 updatedP2)

                                    scale =
                                        two.initDist / currDist

                                    maybeRatio =
                                        aspectRatioRatio data.aspect

                                    ( targetW, targetH ) =
                                        case maybeRatio of
                                            Nothing ->
                                                ( clamp 0.08 1.0 (two.initCrop.width * scale)
                                                , clamp 0.08 1.0 (two.initCrop.height * scale)
                                                )

                                            Just ratio ->
                                                let
                                                    w =
                                                        clamp 0.08 (min 1.0 ratio) (two.initCrop.width * scale)

                                                    h =
                                                        w / ratio
                                                in
                                                if h > 1.0 then
                                                    ( 1.0 * ratio, 1.0 )

                                                else
                                                    ( w, h )

                                    currCenter =
                                        Point ((updatedP1.x + updatedP2.x) / 2.0) ((updatedP1.y + updatedP2.y) / 2.0)

                                    dCenter =
                                        Point (currCenter.x - two.initCenter.x) (currCenter.y - two.initCenter.y)

                                    initCropCenter =
                                        Point (two.initCrop.x + two.initCrop.width / 2.0) (two.initCrop.y + two.initCrop.height / 2.0)

                                    newCropCenter =
                                        Point (initCropCenter.x + dCenter.x) (initCropCenter.y + dCenter.y)

                                    newX =
                                        clamp 0.0 (1.0 - targetW) (newCropCenter.x - targetW / 2.0)

                                    newY =
                                        clamp 0.0 (1.0 - targetH) (newCropCenter.y - targetH / 2.0)

                                    newCrop =
                                        { x = newX, y = newY, width = targetW, height = targetH }

                                    updatedTwo =
                                        TwoPointer { two | p1 = updatedP1, p2 = updatedP2 }
                                in
                                { model
                                    | crop = newCrop
                                    , phase = Framing { data | crop = newCrop }
                                    , gesture = Just updatedTwo
                                }

                            else
                                model

                        Nothing ->
                            model

                PointerUp pointerId ->
                    case model.gesture of
                        Just (SinglePointer id _) ->
                            if pointerId == id then
                                { model | gesture = Nothing }

                            else
                                model

                        Just (TwoPointer two) ->
                            if pointerId == two.id1 then
                                { model | gesture = Just (SinglePointer two.id2 (PanDrag two.p2 data.crop)) }

                            else if pointerId == two.id2 then
                                { model | gesture = Just (SinglePointer two.id1 (PanDrag two.p1 data.crop)) }

                            else
                                model

                        Nothing ->
                            model

                PointerCancel pointerId ->
                    handleInteraction (PointerUp pointerId) model

        _ ->
            case msg of
                SelectShader preset ->
                    setShader preset model

                SelectQuality preset ->
                    setQuality preset model

                SelectOutputMode mode ->
                    setMode mode model

                SelectAnimResolution res ->
                    setAnimResolution res model

                SelectAnimFps fps ->
                    setAnimFps fps model

                SelectAnimDuration dur ->
                    setAnimDuration dur model

                _ ->
                    model


pointDistance : Point -> Point -> Float
pointDistance p1 p2 =
    sqrt ((p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2)


pointFromCrop : CropRect -> Point -> Point
pointFromCrop crop p =
    Point (crop.x + crop.width - (p.x - crop.x)) (crop.y + crop.height - (p.y - crop.y))


hitThreshold : Float
hitThreshold =
    0.045


hitHandle : CropRect -> Point -> Maybe Handle
hitHandle crop p =
    let
        r =
            crop.x + crop.width

        b =
            crop.y + crop.height
    in
    if pointDistance p (Point crop.x crop.y) <= hitThreshold then
        Just CornerNW

    else if pointDistance p (Point r crop.y) <= hitThreshold then
        Just CornerNE

    else if pointDistance p (Point crop.x b) <= hitThreshold then
        Just CornerSW

    else if pointDistance p (Point r b) <= hitThreshold then
        Just CornerSE

    else if abs (p.y - crop.y) <= hitThreshold && p.x >= crop.x - hitThreshold && p.x <= r + hitThreshold then
        Just EdgeN

    else if abs (p.y - b) <= hitThreshold && p.x >= crop.x - hitThreshold && p.x <= r + hitThreshold then
        Just EdgeS

    else if abs (p.x - crop.x) <= hitThreshold && p.y >= crop.y - hitThreshold && p.y <= b + hitThreshold then
        Just EdgeW

    else if abs (p.x - r) <= hitThreshold && p.y >= crop.y - hitThreshold && p.y <= b + hitThreshold then
        Just EdgeE

    else
        Nothing


insideCrop : CropRect -> Point -> Bool
insideCrop crop p =
    p.x >= crop.x && p.x <= (crop.x + crop.width) && p.y >= crop.y && p.y <= (crop.y + crop.height)


resizeCrop : Handle -> AspectPreset -> CropRect -> Point -> CropRect
resizeCrop handle aspect initCrop point =
    let
        initL =
            initCrop.x

        initT =
            initCrop.y

        initR =
            initCrop.x + initCrop.width

        initB =
            initCrop.y + initCrop.height

        clampedPx =
            clamp 0.0 1.0 point.x

        clampedPy =
            clamp 0.0 1.0 point.y

        maybeRatio =
            aspectRatioRatio aspect
    in
    case maybeRatio of
        Nothing ->
            case handle of
                CornerNW ->
                    let
                        newL =
                            clamp 0.0 (initR - 0.08) clampedPx

                        newT =
                            clamp 0.0 (initB - 0.08) clampedPy
                    in
                    { x = newL, y = newT, width = initR - newL, height = initB - newT }

                CornerNE ->
                    let
                        newR =
                            clamp (initL + 0.08) 1.0 clampedPx

                        newT =
                            clamp 0.0 (initB - 0.08) clampedPy
                    in
                    { x = initL, y = newT, width = newR - initL, height = initB - newT }

                CornerSW ->
                    let
                        newL =
                            clamp 0.0 (initR - 0.08) clampedPx

                        newB =
                            clamp (initT + 0.08) 1.0 clampedPy
                    in
                    { x = newL, y = initT, width = initR - newL, height = newB - initT }

                CornerSE ->
                    let
                        newR =
                            clamp (initL + 0.08) 1.0 clampedPx

                        newB =
                            clamp (initT + 0.08) 1.0 clampedPy
                    in
                    { x = initL, y = initT, width = newR - initL, height = newB - initT }

                EdgeN ->
                    let
                        newT =
                            clamp 0.0 (initB - 0.08) clampedPy
                    in
                    { initCrop | y = newT, height = initB - newT }

                EdgeS ->
                    let
                        newB =
                            clamp (initT + 0.08) 1.0 clampedPy
                    in
                    { initCrop | height = newB - initT }

                EdgeW ->
                    let
                        newL =
                            clamp 0.0 (initR - 0.08) clampedPx
                    in
                    { initCrop | x = newL, width = initR - newL }

                EdgeE ->
                    let
                        newR =
                            clamp (initL + 0.08) 1.0 clampedPx
                    in
                    { initCrop | width = newR - initL }

        Just ratio ->
            case handle of
                CornerSE ->
                    let
                        w0 =
                            max 0.08 (clampedPx - initL)

                        h0 =
                            w0 / ratio

                        ( w, h ) =
                            if initT + h0 > 1.0 then
                                let
                                    maxH =
                                        1.0 - initT
                                in
                                ( maxH * ratio, maxH )

                            else if initL + w0 > 1.0 then
                                let
                                    maxW =
                                        1.0 - initL
                                in
                                ( maxW, maxW / ratio )

                            else
                                ( w0, h0 )
                    in
                    { x = initL, y = initT, width = max 0.08 w, height = max 0.08 h }

                CornerNW ->
                    let
                        w0 =
                            max 0.08 (initR - clampedPx)

                        h0 =
                            w0 / ratio

                        ( w, h ) =
                            if initB - h0 < 0.0 then
                                let
                                    maxH =
                                        initB
                                in
                                ( maxH * ratio, maxH )

                            else if initR - w0 < 0.0 then
                                let
                                    maxW =
                                        initR
                                in
                                ( maxW, maxW / ratio )

                            else
                                ( w0, h0 )
                    in
                    { x = initR - max 0.08 w, y = initB - max 0.08 h, width = max 0.08 w, height = max 0.08 h }

                CornerNE ->
                    let
                        w0 =
                            max 0.08 (clampedPx - initL)

                        h0 =
                            w0 / ratio

                        ( w, h ) =
                            if initB - h0 < 0.0 then
                                let
                                    maxH =
                                        initB
                                in
                                ( maxH * ratio, maxH )

                            else if initL + w0 > 1.0 then
                                let
                                    maxW =
                                        1.0 - initL
                                in
                                ( maxW, maxW / ratio )

                            else
                                ( w0, h0 )
                    in
                    { x = initL, y = initB - max 0.08 h, width = max 0.08 w, height = max 0.08 h }

                CornerSW ->
                    let
                        w0 =
                            max 0.08 (initR - clampedPx)

                        h0 =
                            w0 / ratio

                        ( w, h ) =
                            if initT + h0 > 1.0 then
                                let
                                    maxH =
                                        1.0 - initT
                                in
                                ( maxH * ratio, maxH )

                            else if initR - w0 < 0.0 then
                                let
                                    maxW =
                                        initR
                                in
                                ( maxW, maxW / ratio )

                            else
                                ( w0, h0 )
                    in
                    { x = initR - max 0.08 w, y = initT, width = max 0.08 w, height = max 0.08 h }

                EdgeN ->
                    let
                        h0 =
                            max 0.08 (initB - clampedPy)

                        w0 =
                            h0 * ratio

                        ( w, h ) =
                            if w0 > 1.0 then
                                ( 1.0, 1.0 / ratio )

                            else
                                ( w0, h0 )

                        cx =
                            initL + initCrop.width / 2.0

                        x =
                            clamp 0.0 (1.0 - w) (cx - w / 2.0)
                    in
                    { x = x, y = initB - h, width = w, height = h }

                EdgeS ->
                    let
                        h0 =
                            max 0.08 (clampedPy - initT)

                        w0 =
                            h0 * ratio

                        ( w, h ) =
                            if w0 > 1.0 then
                                ( 1.0, 1.0 / ratio )

                            else
                                ( w0, h0 )

                        cx =
                            initL + initCrop.width / 2.0

                        x =
                            clamp 0.0 (1.0 - w) (cx - w / 2.0)
                    in
                    { x = x, y = initT, width = w, height = h }

                EdgeW ->
                    let
                        w0 =
                            max 0.08 (initR - clampedPx)

                        h0 =
                            w0 / ratio

                        ( w, h ) =
                            if h0 > 1.0 then
                                ( 1.0 * ratio, 1.0 )

                            else
                                ( w0, h0 )

                        cy =
                            initT + initCrop.height / 2.0

                        y =
                            clamp 0.0 (1.0 - h) (cy - h / 2.0)
                    in
                    { x = initR - w, y = y, width = w, height = h }

                EdgeE ->
                    let
                        w0 =
                            max 0.08 (clampedPx - initL)

                        h0 =
                            w0 / ratio

                        ( w, h ) =
                            if h0 > 1.0 then
                                ( 1.0 * ratio, 1.0 )

                            else
                                ( w0, h0 )

                        cy =
                            initT + initCrop.height / 2.0

                        y =
                            clamp 0.0 (1.0 - h) (cy - h / 2.0)
                    in
                    { x = initL, y = y, width = w, height = h }


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
        , onStartRender = \_ -> actions.onClose
        , onReframe = actions.onClose
        , onSetCrop = \_ -> actions.onClose
        , onSetAspect = \_ -> actions.onClose
        , onResetCrop = actions.onClose
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
                        Framing _ ->
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
                        Framing _ ->
                            I18n.snapshotFramingDesc lang

                        _ ->
                            I18n.snapshotSubtitle lang
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
          viewStage lang actions model
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


viewToolbar : Language -> FramingActions msg -> Model -> Html msg
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

        activeFps =
            model.animFps

        activeDur =
            model.animDuration
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
                              aspectPill (actions.onSetAspect AspectFree) (I18n.snapshotAspectFree lang) (activeAspect == AspectFree)
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
                            , optionPill "anim-res-360p" (actions.onInteraction (SelectAnimResolution Res360p)) (I18n.snapshotAnimRes360p lang) (activeRes == Res360p)
                            , toolbarDivider

                            -- 3. FPS
                            , optionPill "anim-fps-10" (actions.onInteraction (SelectAnimFps Fps10)) (I18n.snapshotAnimFps10 lang) (activeFps == Fps10)
                            , optionPill "anim-fps-15" (actions.onInteraction (SelectAnimFps Fps15)) (I18n.snapshotAnimFps15 lang) (activeFps == Fps15)
                            , optionPill "anim-fps-30" (actions.onInteraction (SelectAnimFps Fps30)) (I18n.snapshotAnimFps30 lang) (activeFps == Fps30)
                            , toolbarDivider

                            -- 4. Duration
                            , optionPill "anim-dur-2s" (actions.onInteraction (SelectAnimDuration Dur2s)) (I18n.snapshotAnimDur2s lang) (activeDur == Dur2s)
                            , optionPill "anim-dur-4s" (actions.onInteraction (SelectAnimDuration Dur4s)) (I18n.snapshotAnimDur4s lang) (activeDur == Dur4s)
                            , optionPill "anim-dur-6s" (actions.onInteraction (SelectAnimDuration Dur6s)) (I18n.snapshotAnimDur6s lang) (activeDur == Dur6s)
                            , toolbarDivider

                            -- 5. Shader presets
                            , optionPill "shader-preset-glossy" (actions.onInteraction (SelectShader ShaderGlossy)) (I18n.snapshotShaderGlossy lang) (activeShader == ShaderGlossy)
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


viewStage : Language -> FramingActions msg -> Model -> Html msg
viewStage lang actions model =
    let
        cursorStyle =
            case model.phase of
                Preview _ ->
                    "default"

                Rendering ->
                    "wait"

                _ ->
                    case model.mode of
                        ModeAnimation ->
                            "default"

                        ModeStill ->
                            case model.gesture of
                                Just (SinglePointer _ (PanDrag _ _)) ->
                                    "move"

                                Just (SinglePointer _ (HandleDrag _ _ _)) ->
                                    "grabbing"

                                _ ->
                                    "crosshair"

        eventListeners =
            case model.phase of
                Preview _ ->
                    []

                _ ->
                    [ Html.Events.on "pointerdown" (Decode.map actions.onInteraction pointerDownDecoder)
                    , Html.Events.on "pointermove" (Decode.map actions.onInteraction pointerMoveDecoder)
                    , Html.Events.on "pointerup" (Decode.map actions.onInteraction (pointerIdDecoder PointerUp))
                    , Html.Events.on "pointercancel" (Decode.map actions.onInteraction (pointerIdDecoder PointerCancel))
                    , Html.Events.on "pointerleave" (Decode.map actions.onInteraction (pointerIdDecoder PointerUp))
                    , Html.Events.preventDefaultOn "wheel" (Decode.map (\( msg, stop ) -> ( actions.onInteraction msg, stop )) wheelDecoder)
                    ]
    in
    div
        ([ Attr.style "position" "relative"
         , Attr.style "width" "100%"
         , Attr.style "aspect-ratio" "16 / 9"
         , Attr.style "background" previewBackground
         , Attr.style "background-image" "radial-gradient(rgba(255, 255, 255, 0.08) 1px, transparent 1px)"
         , Attr.style "background-size" "16px 16px"
         , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
         , Attr.style "border-radius" "8px"
         , Attr.style "overflow" "hidden"
         , Attr.style "user-select"
            (case model.phase of
                Preview _ ->
                    "auto"

                _ ->
                    "none"
            )
         , Attr.style "touch-action"
            (case model.phase of
                Preview _ ->
                    "auto"

                _ ->
                    "none"
            )
         , Attr.style "cursor" cursorStyle
         ]
            ++ eventListeners
        )
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
                    , Attr.style "user-select" "auto"
                    , Attr.attribute "draggable" "true"
                    ]
                    []
                ]

            Rendering ->
                [ viewRoughPreview model.previewUrl lang
                , viewCropBox model.mode model.crop
                , viewProgressOverlay lang model.progress
                ]

            _ ->
                [ viewRoughPreview model.previewUrl lang
                , viewCropBox model.mode model.crop
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


viewCropBox : OutputMode -> CropRect -> Html msg
viewCropBox mode crop =
    case mode of
        ModeAnimation ->
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

        ModeStill ->
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
                [ -- Grid line 1/3 horizontal
                  div
                    [ Attr.style "position" "absolute"
                    , Attr.style "left" "0"
                    , Attr.style "right" "0"
                    , Attr.style "top" "33.33%"
                    , Attr.style "border-top" "1px dashed rgba(255, 255, 255, 0.3)"
                    , Attr.style "pointer-events" "none"
                    ]
                    []
                , -- Grid line 2/3 horizontal
                  div
                    [ Attr.style "position" "absolute"
                    , Attr.style "left" "0"
                    , Attr.style "right" "0"
                    , Attr.style "top" "66.66%"
                    , Attr.style "border-top" "1px dashed rgba(255, 255, 255, 0.3)"
                    , Attr.style "pointer-events" "none"
                    ]
                    []
                , -- Grid line 1/3 vertical
                  div
                    [ Attr.style "position" "absolute"
                    , Attr.style "top" "0"
                    , Attr.style "bottom" "0"
                    , Attr.style "left" "33.33%"
                    , Attr.style "border-left" "1px dashed rgba(255, 255, 255, 0.3)"
                    , Attr.style "pointer-events" "none"
                    ]
                    []
                , -- Grid line 2/3 vertical
                  div
                    [ Attr.style "position" "absolute"
                    , Attr.style "top" "0"
                    , Attr.style "bottom" "0"
                    , Attr.style "left" "66.66%"
                    , Attr.style "border-left" "1px dashed rgba(255, 255, 255, 0.3)"
                    , Attr.style "pointer-events" "none"
                    ]
                    []
                , -- 8 handles (4 corners + 4 edge centers)
                  viewHandle "top: -5px; left: -5px;"
                , viewHandle "top: -5px; right: -5px;"
                , viewHandle "bottom: -5px; left: -5px;"
                , viewHandle "bottom: -5px; right: -5px;"
                , viewHandle "top: -5px; left: calc(50% - 5px);"
                , viewHandle "bottom: -5px; left: calc(50% - 5px);"
                , viewHandle "top: calc(50% - 5px); left: -5px;"
                , viewHandle "top: calc(50% - 5px); right: -5px;"
                ]


viewProgressOverlay : Language -> Progress -> Html msg
viewProgressOverlay lang progress =
    let
        percent =
            round (progress.fraction * 100)
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
        , Attr.style "gap" "12px"
        , Attr.style "padding" "20px"
        , Attr.style "z-index" "20"
        , Attr.style "pointer-events" "none"
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
            ]
            [ text (I18n.snapshotPathTracing lang progress.samples) ]
        , div
            [ Attr.attribute "role" "progressbar"
            , Attr.attribute "aria-label" "Path tracing progress"
            , Attr.attribute "aria-valuemin" "0"
            , Attr.attribute "aria-valuemax" "100"
            , Attr.attribute "aria-valuenow" (String.fromInt percent)
            , Attr.style "width" "100%"
            , Attr.style "max-width" "420px"
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
        , p
            [ Attr.style "margin" "0"
            , Attr.style "font-size" "12px"
            , Attr.style "color" "rgba(255, 255, 255, 0.85)"
            ]
            [ text (I18n.snapshotRenderNotice lang) ]
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


viewHandle : String -> Html msg
viewHandle customStyle =
    div
        [ Attr.attribute "style"
            ("position: absolute; width: 10px; height: 10px; background: #ffffff; border: 2px solid "
                ++ Theme.brandYellow
                ++ "; border-radius: 2px; box-shadow: 0 1px 3px rgba(0,0,0,0.5); pointer-events: none; "
                ++ customStyle
            )
        ]
        []


pointerDownDecoder : Decode.Decoder InteractionMsg
pointerDownDecoder =
    Decode.map2 PointerDown
        (Decode.oneOf [ Decode.field "pointerId" Decode.int, Decode.succeed 0 ])
        (Decode.map2 Point
            (Decode.map2 (/) (Decode.field "offsetX" Decode.float) (Decode.at [ "currentTarget", "clientWidth" ] Decode.float))
            (Decode.map2 (/) (Decode.field "offsetY" Decode.float) (Decode.at [ "currentTarget", "clientHeight" ] Decode.float))
        )


pointerMoveDecoder : Decode.Decoder InteractionMsg
pointerMoveDecoder =
    Decode.map2 PointerMove
        (Decode.oneOf [ Decode.field "pointerId" Decode.int, Decode.succeed 0 ])
        (Decode.map2 Point
            (Decode.map2 (/) (Decode.field "offsetX" Decode.float) (Decode.at [ "currentTarget", "clientWidth" ] Decode.float))
            (Decode.map2 (/) (Decode.field "offsetY" Decode.float) (Decode.at [ "currentTarget", "clientHeight" ] Decode.float))
        )


pointerIdDecoder : (Int -> InteractionMsg) -> Decode.Decoder InteractionMsg
pointerIdDecoder toMsg =
    Decode.map toMsg
        (Decode.oneOf [ Decode.field "pointerId" Decode.int, Decode.succeed 0 ])


wheelDecoder : Decode.Decoder ( InteractionMsg, Bool )
wheelDecoder =
    Decode.map4
        (\deltaY ox cw ( oy, ch ) ->
            let
                u =
                    if cw > 0 then
                        clamp 0.0 1.0 (ox / cw)

                    else
                        0.5

                v =
                    if ch > 0 then
                        clamp 0.0 1.0 (oy / ch)

                    else
                        0.5
            in
            ( Wheel deltaY (Point u v), True )
        )
        (Decode.field "deltaY" Decode.float)
        (Decode.field "offsetX" Decode.float)
        (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)
        (Decode.map2 Tuple.pair
            (Decode.field "offsetY" Decode.float)
            (Decode.at [ "currentTarget", "clientHeight" ] Decode.float)
        )


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
            Framing _ ->
                [ renderButton lang actions model.crop
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


renderButton : Language -> FramingActions msg -> CropRect -> Html msg
renderButton lang actions crop =
    button
        [ Attr.type_ "button"
        , Html.Events.onClick (actions.onStartRender crop)
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
