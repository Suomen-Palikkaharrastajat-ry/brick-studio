module Game.MapParams exposing
    ( Biome(..)
    , MapParams
    , Pick(..)
    , Season(..)
    , Selection
    , WeatherKind(..)
    , allBiomes
    , allSeasons
    , allWeathers
    , allowedWeathers
    , anySelection
    , biomeFromString
    , biomeToString
    , coerce
    , fromCode
    , generatorVersion
    , leaderboardKey
    , resolve
    , seasonFromString
    , seasonToString
    , streamSeed
    , toCode
    , weatherFromString
    , weatherToString
    )

{-| Map parameters shared by Brick Racer and Brick Sails.

A map is a biome, a season, a weather and a seed. Its map code is the four
parts joined by dashes, for example `boreal-winter-snow-48213`, and the same
code means the same map in both games.

The main screen lets the player pick each part or leave it at `Any`.
`resolve` turns a `Selection` into concrete `MapParams`: an `Any` part is
chosen from the seed, so one selection always gives the same map and the
code it produces can be shared.

Not every biome, season and weather combination makes sense (no snow in
summer, no fog on a desert afternoon), so `allowedWeathers` lists the
weathers each biome and season can have, and `coerce` moves an impossible
choice to the nearest allowed one. `fromCode` coerces too, so an edited
code cannot produce an impossible map.

-}


{-| The landscape of a map.
-}
type Biome
    = Farmland
    | Boreal
    | Fell
    | Desert


{-| The season of a map.
-}
type Season
    = Spring
    | Summer
    | Autumn
    | Winter


{-| The weather of a map.
-}
type WeatherKind
    = Clear
    | Overcast
    | Rain
    | Storm
    | Fog
    | Snow


{-| A concrete map: every part chosen, with a seed from 1 to 999999.
-}
type alias MapParams =
    { biome : Biome
    , season : Season
    , weather : WeatherKind
    , seed : Int
    }


{-| One choice on the main screen: a concrete value or `Any`, which the
seed decides when the game starts.
-}
type Pick a
    = Any
    | Exactly a


{-| What the player picked on the main screen, before `resolve`.
-}
type alias Selection =
    { biome : Pick Biome
    , season : Pick Season
    , weather : Pick WeatherKind
    , seed : Int
    }


{-| A selection where every part is `Any` and the seed is 1.
-}
anySelection : Selection
anySelection =
    { biome = Any
    , season = Any
    , weather = Any
    , seed = 1
    }


allBiomes : List Biome
allBiomes =
    [ Farmland, Boreal, Fell, Desert ]


allSeasons : List Season
allSeasons =
    [ Spring, Summer, Autumn, Winter ]


allWeathers : List WeatherKind
allWeathers =
    [ Clear, Overcast, Rain, Storm, Fog, Snow ]


biomeToString : Biome -> String
biomeToString biome =
    case biome of
        Farmland ->
            "farmland"

        Boreal ->
            "boreal"

        Fell ->
            "fell"

        Desert ->
            "desert"


biomeFromString : String -> Maybe Biome
biomeFromString text =
    firstMatch (\b -> biomeToString b == text) allBiomes


seasonToString : Season -> String
seasonToString season =
    case season of
        Spring ->
            "spring"

        Summer ->
            "summer"

        Autumn ->
            "autumn"

        Winter ->
            "winter"


seasonFromString : String -> Maybe Season
seasonFromString text =
    firstMatch (\s -> seasonToString s == text) allSeasons


weatherToString : WeatherKind -> String
weatherToString weather =
    case weather of
        Clear ->
            "clear"

        Overcast ->
            "overcast"

        Rain ->
            "rain"

        Storm ->
            "storm"

        Fog ->
            "fog"

        Snow ->
            "snow"


weatherFromString : String -> Maybe WeatherKind
weatherFromString text =
    firstMatch (\w -> weatherToString w == text) allWeathers


{-| The weathers a biome can have in a season.

Overcast is allowed in every cell, which makes it the end of every
`coerce` fallback chain.

-}
allowedWeathers : Biome -> Season -> List WeatherKind
allowedWeathers biome season =
    case ( biome, season ) of
        ( Farmland, Spring ) ->
            [ Clear, Overcast, Rain, Fog ]

        ( Farmland, Summer ) ->
            [ Clear, Overcast, Rain, Storm ]

        ( Farmland, Autumn ) ->
            [ Clear, Overcast, Rain, Storm, Fog ]

        ( Farmland, Winter ) ->
            [ Clear, Overcast, Fog, Snow ]

        ( Boreal, Spring ) ->
            [ Clear, Overcast, Rain, Fog ]

        ( Boreal, Summer ) ->
            [ Clear, Overcast, Rain, Storm ]

        ( Boreal, Autumn ) ->
            [ Overcast, Rain, Storm, Fog ]

        ( Boreal, Winter ) ->
            [ Clear, Overcast, Fog, Snow, Storm ]

        ( Fell, Spring ) ->
            [ Clear, Overcast, Fog, Snow ]

        ( Fell, Summer ) ->
            [ Clear, Overcast, Rain, Fog ]

        ( Fell, Autumn ) ->
            [ Overcast, Rain, Fog, Snow ]

        ( Fell, Winter ) ->
            [ Clear, Overcast, Snow, Storm ]

        ( Desert, Spring ) ->
            [ Clear, Overcast, Rain, Storm ]

        ( Desert, Summer ) ->
            [ Clear, Overcast, Storm ]

        ( Desert, Autumn ) ->
            [ Clear, Overcast, Rain, Fog ]

        ( Desert, Winter ) ->
            [ Clear, Overcast, Fog ]


{-| Move a weather that the biome and season do not allow to the nearest
allowed one: Snow to Rain, Storm to Rain, Rain and Clear to Overcast and Fog
to Overcast, repeating until an allowed weather is found.
-}
coerce : Biome -> Season -> WeatherKind -> WeatherKind
coerce biome season weather =
    if List.member weather (allowedWeathers biome season) then
        weather

    else
        coerce biome season (fallbackWeather weather)


fallbackWeather : WeatherKind -> WeatherKind
fallbackWeather weather =
    case weather of
        Snow ->
            Rain

        Storm ->
            Rain

        Rain ->
            Overcast

        Fog ->
            Overcast

        Clear ->
            Overcast

        Overcast ->
            Clear


{-| Turn a selection into concrete map parameters.

`Any` biome and season come from the seed, and an `Any` weather is picked
from the weathers that the resolved biome and season allow. An `Exactly`
weather is coerced the same way.

-}
resolve : Selection -> MapParams
resolve selection =
    let
        seed =
            clampSeed selection.seed

        biome =
            pickOr "biome" seed Farmland allBiomes selection.biome

        season =
            pickOr "season" seed Spring allSeasons selection.season

        weather =
            case selection.weather of
                Exactly w ->
                    coerce biome season w

                Any ->
                    pickFrom "weather" seed Overcast (allowedWeathers biome season)
    in
    { biome = biome
    , season = season
    , weather = weather
    , seed = seed
    }


pickOr : String -> Int -> a -> List a -> Pick a -> a
pickOr salt seed fallback options pick =
    case pick of
        Exactly value ->
            value

        Any ->
            pickFrom salt seed fallback options


{-| Choose one of the options from the seed. `fallback` is only returned if
the list is empty, which no call site allows.
-}
pickFrom : String -> Int -> a -> List a -> a
pickFrom salt seed fallback options =
    let
        index =
            modBy (max 1 (List.length options)) (hashText (salt ++ ":" ++ String.fromInt seed))
    in
    case List.drop index options of
        value :: _ ->
            value

        [] ->
            fallback


firstMatch : (a -> Bool) -> List a -> Maybe a
firstMatch predicate items =
    items
        |> List.filter predicate
        |> List.head


{-| The map code: `biome-season-weather-seed`, in lower case.
-}
toCode : MapParams -> String
toCode p =
    String.join "-"
        [ biomeToString p.biome
        , seasonToString p.season
        , weatherToString p.weather
        , String.fromInt p.seed
        ]


{-| Read a map code made by `toCode`. Case and surrounding spaces are
ignored, the seed is clamped to 1..999999, and an impossible weather is
coerced. Returns `Nothing` for anything that is not four parts of a known
biome, season, weather and a whole-number seed.
-}
fromCode : String -> Maybe MapParams
fromCode text =
    case String.split "-" (String.toLower (String.trim text)) of
        [ b, s, w, n ] ->
            Maybe.map4
                (\biome season weather seed ->
                    { biome = biome
                    , season = season
                    , weather = coerce biome season weather
                    , seed = clampSeed seed
                    }
                )
                (biomeFromString b)
                (seasonFromString s)
                (weatherFromString w)
                (String.toInt n)

        _ ->
            Nothing


clampSeed : Int -> Int
clampSeed seed =
    clamp 1 999999 seed


{-| Version of the map generators. Bump it whenever a generator changes what
a map code produces, so that old leaderboard entries are no longer compared
with new maps.
-}
generatorVersion : String
generatorVersion =
    "g1"


{-| The leaderboard key of a map: one board per exact map code.
-}
leaderboardKey : MapParams -> String
leaderboardKey p =
    generatorVersion ++ ":" ++ toCode p


{-| A seed for one sub-generator of a map. Each stream name gives its own
seed, so adding a stream does not change the others.
-}
streamSeed : String -> MapParams -> Int
streamSeed stream p =
    hashText (stream ++ "|" ++ toCode p)


{-| The text hash used for seeds, the same fold that `Sails.Weather.seedFrom`
uses, kept within 1..2147483646.
-}
hashText : String -> Int
hashText text =
    String.foldl (\c h -> modBy 2147483647 (h * 31 + Char.toCode c)) 7 text
        |> max 1
