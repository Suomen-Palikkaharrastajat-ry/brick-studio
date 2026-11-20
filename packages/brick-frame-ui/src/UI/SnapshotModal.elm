module UI.SnapshotModal exposing
    ( Actions
    , AspectPreset(..)
    , CropRect
    , FramingActions
    , FramingData
    , Model
    , Phase(..)
    , PreviewData
    , Progress
    , defaultCrop
    , getCrop
    , init
    , initFraming
    , isFraming
    , isRendering
    , markSaved
    , reframe
    , resetCrop
    , setAspect
    , setCrop
    , setFailed
    , setProgress
    , setReady
    , startRendering
    , view
    , viewFramingWithLanguage
    , viewWithLanguage
    )

{-| Full-screen modal for framing, capturing, previewing, and saving a snapshot of
the canvas as a PNG.

The modal provides an interactive framing phase where the user can crop the scene
or select an aspect ratio (Free, 16:9, 4:3, 1:1) before path tracing. During
rendering, it displays a progress bar and sample count. Once ready, the preview
allows saving the PNG or reframing.

-}

import Html exposing (Html, button, div, h2, img, p, span, text)
import Html.Attributes as Attr
import Html.Events
import I18n exposing (Language(..))
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


{-| Snapshot modal state.
-}
type alias Model =
    { filename : String
    , phase : Phase
    , progress : Progress
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
    }


{-| Initialize modal directly in the rendering phase (backward compatible).
-}
init : { filename : String } -> Model
init config =
    { filename = config.filename
    , phase = Rendering
    , progress = { fraction = 0, samples = 0 }
    }


{-| Initialize modal in the pre-render framing phase.
-}
initFraming : { filename : String, previewUrl : Maybe String } -> Model
initFraming config =
    { filename = config.filename
    , phase =
        Framing
            { previewUrl = config.previewUrl
            , crop = defaultCrop
            , aspect = AspectFree
            }
    , progress = { fraction = 0, samples = 0 }
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
    { model | phase = Rendering, progress = { fraction = 0, samples = 0 } }


{-| Return from preview back to the framing phase.
-}
reframe : Model -> Model
reframe model =
    case model.phase of
        Preview data ->
            { model
                | phase =
                    Framing
                        { previewUrl = Just data.dataUrl
                        , crop = defaultCrop
                        , aspect = AspectFree
                        }
            }

        _ ->
            model


{-| Get the current crop bounds.
-}
getCrop : Model -> CropRect
getCrop model =
    case model.phase of
        Framing data ->
            data.crop

        _ ->
            defaultCrop


{-| Set the crop bounds, clamping within [0, 1].
-}
setCrop : CropRect -> Model -> Model
setCrop crop model =
    case model.phase of
        Framing data ->
            let
                clampedX =
                    clamp 0.0 0.9 crop.x

                clampedY =
                    clamp 0.0 0.9 crop.y

                clampedW =
                    clamp 0.1 (1.0 - clampedX) crop.width

                clampedH =
                    clamp 0.1 (1.0 - clampedY) crop.height
            in
            { model
                | phase =
                    Framing
                        { data
                            | crop =
                                { x = clampedX
                                , y = clampedY
                                , width = clampedW
                                , height = clampedH
                                }
                        }
            }

        _ ->
            model


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
            -- In a 16:9 canvas, 4:3 width ratio = (4/3) / (16/9) = 0.75
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
            -- In a 16:9 canvas, 1:1 width ratio = 1.0 / (16/9) = 0.5625
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
                | phase =
                    Framing
                        { data
                            | aspect = aspect
                            , crop = newCrop
                        }
            }

        _ ->
            model


{-| Reset crop to the full viewport frame.
-}
resetCrop : Model -> Model
resetCrop model =
    case model.phase of
        Framing data ->
            { model
                | phase =
                    Framing
                        { data
                            | crop = defaultCrop
                            , aspect = AspectFree
                        }
            }

        _ ->
            model


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
-}
setReady : { dataUrl : String, filename : String } -> Model -> Model
setReady { dataUrl, filename } model =
    { model
        | filename = filename
        , phase =
            Preview
                { dataUrl = dataUrl
                , filename = filename
                , hasSaved = False
                }
    }


{-| Mark the image as saved.
-}
markSaved : Model -> Model
markSaved model =
    case model.phase of
        Preview data ->
            { model | phase = Preview { data | hasSaved = True } }

        _ ->
            model


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
            [ text "PNG" ]
        ]


viewBody : Language -> FramingActions msg -> Model -> Html msg
viewBody lang actions model =
    div
        [ Attr.style "padding" "20px"
        , Attr.style "overflow-y" "auto"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "16px"
        ]
        (case model.phase of
            Framing data ->
                [ viewFraming lang actions data ]

            Rendering ->
                [ viewRendering lang model.progress ]

            Preview data ->
                [ viewPreview lang data ]

            Failed message ->
                [ viewFailed message ]
        )


viewFraming : Language -> FramingActions msg -> FramingData -> Html msg
viewFraming lang actions data =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "14px"
        ]
        [ -- Toolbar for aspect ratio presets and zoom/framing presets
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
                , Attr.style "gap" "6px"
                ]
                [ aspectPill (actions.onSetAspect AspectFree) (I18n.snapshotAspectFree lang) (data.aspect == AspectFree)
                , aspectPill (actions.onSetAspect Aspect16_9) (I18n.snapshotAspect16_9 lang) (data.aspect == Aspect16_9)
                , aspectPill (actions.onSetAspect Aspect4_3) (I18n.snapshotAspect4_3 lang) (data.aspect == Aspect4_3)
                , aspectPill (actions.onSetAspect Aspect1_1) (I18n.snapshotAspect1_1 lang) (data.aspect == Aspect1_1)
                ]
            , div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "6px"
                ]
                [ quickCropPill (actions.onSetCrop { x = 0.1, y = 0.1, width = 0.8, height = 0.8 }) "80%"
                , quickCropPill (actions.onSetCrop { x = 0.2, y = 0.2, width = 0.6, height = 0.6 }) "60%"
                , button
                    [ Attr.type_ "button"
                    , Html.Events.onClick actions.onResetCrop
                    , Attr.class "ui-button"
                    , Attr.style "padding" "4px 10px"
                    , Attr.style "font-size" "12px"
                    , Attr.style "font-weight" "500"
                    , Attr.style "background" Theme.panelSubtleBackground
                    , Attr.style "color" Theme.textPrimary
                    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                    , Attr.style "border-radius" "6px"
                    , Attr.style "cursor" "pointer"
                    ]
                    [ text (I18n.snapshotResetCrop lang) ]
                ]
            ]
        , -- Interactive framing canvas / viewport display
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
            , Attr.style "user-select" "none"
            ]
            [ case data.previewUrl of
                Just url ->
                    img
                        [ Attr.src url
                        , Attr.alt "Framing preview"
                        , Attr.style "width" "100%"
                        , Attr.style "height" "100%"
                        , Attr.style "object-fit" "contain"
                        , Attr.style "display" "block"
                        ]
                        []

                Nothing ->
                    div
                        [ Attr.style "width" "100%"
                        , Attr.style "height" "100%"
                        , Attr.style "display" "flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "center"
                        , Attr.style "color" Theme.textMuted
                        , Attr.style "font-size" "13px"
                        ]
                        [ text (I18n.snapshotSubtitle lang) ]
            , -- Crop box with rule-of-thirds grid and outer shadow scrim
              div
                [ Attr.style "position" "absolute"
                , Attr.style "left" (String.fromFloat (data.crop.x * 100.0) ++ "%")
                , Attr.style "top" (String.fromFloat (data.crop.y * 100.0) ++ "%")
                , Attr.style "width" (String.fromFloat (data.crop.width * 100.0) ++ "%")
                , Attr.style "height" (String.fromFloat (data.crop.height * 100.0) ++ "%")
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
                    ]
                    []
                , -- Grid line 2/3 horizontal
                  div
                    [ Attr.style "position" "absolute"
                    , Attr.style "left" "0"
                    , Attr.style "right" "0"
                    , Attr.style "top" "66.66%"
                    , Attr.style "border-top" "1px dashed rgba(255, 255, 255, 0.3)"
                    ]
                    []
                , -- Grid line 1/3 vertical
                  div
                    [ Attr.style "position" "absolute"
                    , Attr.style "top" "0"
                    , Attr.style "bottom" "0"
                    , Attr.style "left" "33.33%"
                    , Attr.style "border-left" "1px dashed rgba(255, 255, 255, 0.3)"
                    ]
                    []
                , -- Grid line 2/3 vertical
                  div
                    [ Attr.style "position" "absolute"
                    , Attr.style "top" "0"
                    , Attr.style "bottom" "0"
                    , Attr.style "left" "66.66%"
                    , Attr.style "border-left" "1px dashed rgba(255, 255, 255, 0.3)"
                    ]
                    []
                ]
            ]
        , -- Nudge / positioning row if cropped
          if data.crop.width < 0.99 || data.crop.height < 0.99 then
            div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "gap" "8px"
                ]
                [ nudgeButton (actions.onSetCrop (nudgeCrop -0.05 0.0 data.crop)) "←"
                , nudgeButton (actions.onSetCrop (nudgeCrop 0.0 -0.05 data.crop)) "↑"
                , nudgeButton (actions.onSetCrop (nudgeCrop 0.0 0.05 data.crop)) "↓"
                , nudgeButton (actions.onSetCrop (nudgeCrop 0.05 0.0 data.crop)) "→"
                ]

          else
            text ""
        ]


nudgeCrop : Float -> Float -> CropRect -> CropRect
nudgeCrop dx dy crop =
    { x = clamp 0.0 (1.0 - crop.width) (crop.x + dx)
    , y = clamp 0.0 (1.0 - crop.height) (crop.y + dy)
    , width = crop.width
    , height = crop.height
    }


nudgeButton : msg -> String -> Html msg
nudgeButton msg label =
    button
        [ Attr.type_ "button"
        , Html.Events.onClick msg
        , Attr.class "ui-button"
        , Attr.style "padding" "4px 10px"
        , Attr.style "font-size" "12px"
        , Attr.style "font-weight" "600"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "color" Theme.textPrimary
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "4px"
        , Attr.style "cursor" "pointer"
        ]
        [ text label ]


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


quickCropPill : msg -> String -> Html msg
quickCropPill msg label =
    button
        [ Attr.type_ "button"
        , Html.Events.onClick msg
        , Attr.class "ui-button"
        , Attr.style "padding" "4px 8px"
        , Attr.style "font-size" "12px"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "color" Theme.textMuted
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "6px"
        , Attr.style "cursor" "pointer"
        ]
        [ text label ]


viewRendering : Language -> Progress -> Html msg
viewRendering lang progress =
    let
        percent =
            round (progress.fraction * 100)
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "padding" "32px 16px"
        , Attr.style "gap" "16px"
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
            , Attr.style "background" Theme.panelSubtleBackground
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
            , Attr.style "font-size" "13px"
            , Attr.style "color" Theme.textMuted
            ]
            [ text (I18n.snapshotRenderNotice lang) ]
        ]


viewFailed : String -> Html msg
viewFailed message =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "padding" "32px 16px"
        , Attr.style "gap" "12px"
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


viewPreview : Language -> PreviewData -> Html msg
viewPreview lang data =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "12px"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "background" previewBackground
            , Attr.style "background-image" "radial-gradient(rgba(255, 255, 255, 0.08) 1px, transparent 1px)"
            , Attr.style "background-size" "16px 16px"
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "8px"
            , Attr.style "overflow" "hidden"
            ]
            [ img
                [ Attr.src data.dataUrl
                , Attr.alt "Snapshot preview"
                , Attr.style "max-height" "460px"
                , Attr.style "width" "100%"
                , Attr.style "object-fit" "contain"
                , Attr.style "display" "block"
                ]
                []
            ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "space-between"
            , Attr.style "font-size" "12px"
            , Attr.style "color" Theme.textMuted
            ]
            [ div []
                [ text (data.filename ++ " • PNG") ]
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
            Framing data ->
                [ Button.secondary actions.onClose (I18n.snapshotClose lang)
                , renderButton lang actions data.crop
                ]

            Rendering ->
                [ div
                    [ Attr.style "font-size" "13px"
                    , Attr.style "color" Theme.textMuted
                    ]
                    [ text
                        (case lang of
                            Fi ->
                                "Kuva tulee näkyviin tähän, kun se on valmis."

                            En ->
                                "The image will appear here when it is ready."
                        )
                    ]
                , Button.secondary actions.onClose (I18n.snapshotClose lang)
                ]

            Preview data ->
                [ Button.secondary actions.onReframe (I18n.snapshotReframe lang)
                , div
                    [ Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "10px"
                    ]
                    [ saveButton lang actions data
                    , Button.secondary actions.onClose (I18n.snapshotClose lang)
                    ]
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
