module Game.MapPicker exposing
    ( Config
    , Labels
    , view
    )

{-| The main-screen map picker shared by Brick Racer and Brick Sails.

It shows one row of chips each for biome, season and weather (each with an
"Any" chip), a seed field with a dice button, an optional "previous map"
button, and the map code that the current choice resolves to. Weather chips
that the chosen biome and season cannot have are greyed out.

The view is plain Html without any styling classes, so each game can wrap it
in its own layout.

-}

import Game.MapParams as Map exposing (Biome, Pick(..), Season, Selection, WeatherKind)
import Html exposing (Html, button, div, input, p, span, text)
import Html.Attributes as Attr
import Html.Events as Events


{-| Words the picker shows, so each game can translate them.
-}
type alias Labels =
    { biome : String
    , season : String
    , weather : String
    , seed : String
    , random : String
    , previous : String
    , any : String
    , code : String
    , biomeName : Biome -> String
    , seasonName : Season -> String
    , weatherName : WeatherKind -> String
    }


{-| The picker's state and the messages it sends when the player changes it.
`onPrevious` is `Nothing` when there is no earlier map to reuse.
-}
type alias Config msg =
    { labels : Labels
    , selection : Selection
    , seedText : String
    , onBiome : Pick Biome -> msg
    , onSeason : Pick Season -> msg
    , onWeather : Pick WeatherKind -> msg
    , onSeedInput : String -> msg
    , onRandomSeed : msg
    , onPrevious : Maybe msg
    }


view : Config msg -> Html msg
view config =
    let
        labels =
            config.labels

        selection =
            config.selection

        candidateBiomes =
            case selection.biome of
                Exactly b ->
                    [ b ]

                Any ->
                    Map.allBiomes

        candidateSeasons =
            case selection.season of
                Exactly s ->
                    [ s ]

                Any ->
                    Map.allSeasons

        weatherPossible w =
            List.any
                (\b ->
                    List.any (\s -> List.member w (Map.allowedWeathers b s)) candidateSeasons
                )
                candidateBiomes

        previousButton =
            case config.onPrevious of
                Just msg ->
                    [ button [ Attr.type_ "button", Events.onClick msg ] [ text labels.previous ] ]

                Nothing ->
                    []
    in
    div []
        [ row labels.biome
            (anyChip config.onBiome (selection.biome == Any) labels.any
                :: List.map
                    (\b ->
                        chip (selection.biome == Exactly b) False (config.onBiome (Exactly b)) (labels.biomeName b)
                    )
                    Map.allBiomes
            )
        , row labels.season
            (anyChip config.onSeason (selection.season == Any) labels.any
                :: List.map
                    (\s ->
                        chip (selection.season == Exactly s) False (config.onSeason (Exactly s)) (labels.seasonName s)
                    )
                    Map.allSeasons
            )
        , row labels.weather
            (anyChip config.onWeather (selection.weather == Any) labels.any
                :: List.map
                    (\w ->
                        chip
                            (selection.weather == Exactly w)
                            (not (weatherPossible w))
                            (config.onWeather (Exactly w))
                            (labels.weatherName w)
                    )
                    Map.allWeathers
            )
        , div []
            ([ text labels.seed
             , input
                [ Attr.type_ "text"
                , Attr.attribute "inputmode" "numeric"
                , Attr.value config.seedText
                , Events.onInput config.onSeedInput
                ]
                []
             , button [ Attr.type_ "button", Events.onClick config.onRandomSeed ] [ text labels.random ]
             ]
                ++ previousButton
            )
        , p []
            [ span [] [ text (labels.code ++ ": ") ]
            , span [] [ text (Map.toCode (Map.resolve selection)) ]
            ]
        ]


row : String -> List (Html msg) -> Html msg
row title chips =
    div []
        [ span [] [ text title ]
        , div [] chips
        ]


anyChip : msg -> Bool -> String -> Html msg
anyChip msg active label =
    chip active False msg label


chip : Bool -> Bool -> msg -> String -> Html msg
chip active disabled msg label =
    button
        [ Attr.type_ "button"
        , Attr.disabled disabled
        , Attr.attribute "aria-pressed"
            (if active then
                "true"

             else
                "false"
            )
        , Events.onClick msg
        ]
        [ text label ]
