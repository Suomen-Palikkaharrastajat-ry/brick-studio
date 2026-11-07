module UI.HamburgerMenu exposing (Config, Item, view, viewWithLanguage)

{-| The top-left menu of the minimal apps (Brick Play): one
hamburger button that opens a small dropdown of actions, each with its keyboard
shortcut shown on the right.

The host owns the open flag and decides what the messages do. `onToggle` opens
or closes the menu, `onClose` closes it (a click on the page outside the panel,
which is covered by a transparent backdrop). Choosing an item sends its
`onSelect`; the host should close the menu in that same update. Escape is the
host's keyboard shortcut, not this view's.

Items are real buttons with `role="menuitem"`, so they take focus with Tab after
the hamburger. The wrapper has no transform or backdrop filter, so the backdrop
covers the whole window.

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import I18n exposing (Language(..))
import UI.Icon as Icon


{-| One row of the menu: its label, the shortcut shown on the right (`""` for
none), whether it can be chosen, and the message it sends.
-}
type alias Item msg =
    { label : String
    , shortcut : String
    , enabled : Bool
    , onSelect : msg
    }


{-| The menu's state and messages. `id` names the panel, for `aria-controls`.
-}
type alias Config msg =
    { id : String
    , open : Bool
    , onToggle : msg
    , onClose : msg
    , languageToggle : Maybe { current : Language, onSelect : Language -> msg }
    , items : List (Item msg)
    }


{-| The hamburger button, and the panel and backdrop while the menu is open, using the specified language.
-}
viewWithLanguage : Language -> Config msg -> Html msg
viewWithLanguage lang config =
    div
        [ Attr.style "position" "relative"
        , Attr.style "pointer-events" "auto"
        ]
        (hamburgerButton lang config
            :: (if config.open then
                    [ backdrop config.onClose, panel lang config ]

                else
                    []
               )
        )


{-| The hamburger button, and the panel and backdrop while the menu is open. Defaults to English.
-}
view : Config msg -> Html msg
view =
    viewWithLanguage En


hamburgerButton : Language -> Config msg -> Html msg
hamburgerButton lang config =
    button
        [ onClick config.onToggle
        , Attr.attribute "aria-label" (I18n.headerMenu lang)
        , Attr.title (I18n.headerMenu lang)
        , Attr.class "touch-target"
        , Attr.attribute "aria-haspopup" "menu"
        , Attr.attribute "aria-expanded"
            (if config.open then
                "true"

             else
                "false"
            )
        , Attr.attribute "aria-controls" config.id
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "2.25rem"
        , Attr.style "height" "2.25rem"
        , Attr.style "padding" "0"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        ]
        [ Icon.featherSized 18 "menu" ]


{-| The transparent layer behind an open menu: a click on it closes the menu.
-}
backdrop : msg -> Html msg
backdrop onClose =
    div
        [ onClick onClose
        , Attr.attribute "aria-hidden" "true"
        , Attr.style "position" "fixed"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "pointer-events" "auto"
        ]
        []


panel : Language -> Config msg -> Html msg
panel lang config =
    div
        [ Attr.id config.id
        , Attr.attribute "role" "menu"
        , Attr.attribute "aria-label" (I18n.headerMenu lang)
        , Attr.style "position" "absolute"
        , Attr.style "top" "calc(100% + 0.375rem)"
        , Attr.style "left" "0"
        , Attr.style "z-index" "1"
        , Attr.style "min-width" "13rem"
        , Attr.style "padding" "0.25rem"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "0.125rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.95)"
        , Attr.style "backdrop-filter" "blur(12px)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "0.625rem"
        , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
        ]
        (case config.languageToggle of
            Just toggle ->
                viewLanguageToggle toggle :: List.map item config.items

            Nothing ->
                List.map item config.items
        )


viewLanguageToggle : { current : Language, onSelect : Language -> msg } -> Html msg
viewLanguageToggle toggle =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Language"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "padding" "0.125rem 0.25rem 0.25rem 0.25rem"
        , Attr.style "margin-bottom" "0.125rem"
        , Attr.style "border-bottom" "1px solid rgba(255, 255, 255, 0.1)"
        ]
        [ div
            [ Attr.style "display" "inline-flex"
            , Attr.style "width" "100%"
            , Attr.style "background" "rgba(255, 255, 255, 0.08)"
            , Attr.style "border-radius" "0.375rem"
            , Attr.style "padding" "2px"
            , Attr.style "gap" "2px"
            ]
            [ languageSegment (toggle.current == Fi) "Suomi" (toggle.onSelect Fi)
            , languageSegment (toggle.current == En) "In English" (toggle.onSelect En)
            ]
        ]


languageSegment : Bool -> String -> msg -> Html msg
languageSegment active label onSelect =
    button
        [ onClick onSelect
        , Attr.type_ "button"
        , Attr.attribute "aria-pressed"
            (if active then
                "true"

             else
                "false"
            )
        , Attr.style "flex" "1"
        , Attr.style "padding" "0.3125rem 0.5rem"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.25rem"
        , Attr.style "font-size" "0.75rem"
        , Attr.style "font-weight"
            (if active then
                "600"

             else
                "500"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        , Attr.style "background"
            (if active then
                "rgba(255, 255, 255, 0.22)"

             else
                "transparent"
            )
        , Attr.style "color"
            (if active then
                "var(--color-text-on-dark)"

             else
                "rgba(255, 255, 255, 0.6)"
            )
        , Attr.style "box-shadow"
            (if active then
                "0 1px 2px rgba(0, 0, 0, 0.25)"

             else
                "none"
            )
        ]
        [ text label ]


item : Item msg -> Html msg
item entry =
    button
        [ Attr.attribute "role" "menuitem"
        , Attr.class "touch-target"
        , Attr.disabled (not entry.enabled)
        , onClick entry.onSelect
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "gap" "1.5rem"
        , Attr.style "width" "100%"
        , Attr.style "padding" "0.4375rem 0.625rem"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background" "transparent"
        , Attr.style "color"
            (if entry.enabled then
                "var(--color-text-on-dark)"

             else
                "rgba(255, 255, 255, 0.3)"
            )
        , Attr.style "font-size" "0.8125rem"
        , Attr.style "font-weight" "500"
        , Attr.style "text-align" "left"
        , Attr.style "white-space" "nowrap"
        , Attr.style "cursor"
            (if entry.enabled then
                "pointer"

             else
                "not-allowed"
            )
        ]
        [ span [] [ text entry.label ]
        , if String.isEmpty entry.shortcut then
            text ""

          else
            span
                [ Attr.style "font-size" "0.6875rem"
                , Attr.style "color" "var(--color-text-subtle)"
                , Attr.style "font-variant-numeric" "tabular-nums"
                ]
                [ text entry.shortcut ]
        ]
