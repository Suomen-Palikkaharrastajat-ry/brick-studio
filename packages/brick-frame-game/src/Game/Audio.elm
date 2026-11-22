module Game.Audio exposing
    ( AudioTelemetry
    , OneShotSound(..)
    , encodeOneShot
    , encodeTelemetry
    )

{-| Universal procedural Web Audio synthesis parameters and sound effect commands.
-}

import Json.Encode as Encode


{-| Continuous synthesizer parameters streamed to Web Audio graph on frame updates.
-}
type alias AudioTelemetry =
    { active : Bool
    , windGain : Float
    , windHz : Float
    , washGain : Float
    , washHz : Float
    , hullGain : Float
    , toneHz : Float
    , toneGain : Float
    }


{-| One-shot discrete sound effect triggers.
-}
type OneShotSound
    = Impact Float
    | Checkpoint
    | Finish
    | Bell
    | Splash Float
    | Horn


{-| Encode continuous audio telemetry for port transfer.
-}
encodeTelemetry : AudioTelemetry -> Encode.Value
encodeTelemetry t =
    Encode.object
        [ ( "active", Encode.bool t.active )
        , ( "windGain", Encode.float t.windGain )
        , ( "windHz", Encode.float t.windHz )
        , ( "washGain", Encode.float t.washGain )
        , ( "washHz", Encode.float t.washHz )
        , ( "hullGain", Encode.float t.hullGain )
        , ( "toneHz", Encode.float t.toneHz )
        , ( "toneGain", Encode.float t.toneGain )
        ]


{-| Encode a one-shot sound trigger for port transfer.
-}
encodeOneShot : OneShotSound -> Encode.Value
encodeOneShot snd =
    case snd of
        Impact strength ->
            Encode.object
                [ ( "type", Encode.string "impact" )
                , ( "strength", Encode.float strength )
                ]

        Checkpoint ->
            Encode.object
                [ ( "type", Encode.string "checkpoint" )
                ]

        Finish ->
            Encode.object
                [ ( "type", Encode.string "finish" )
                ]

        Bell ->
            Encode.object
                [ ( "type", Encode.string "bell" )
                ]

        Splash strength ->
            Encode.object
                [ ( "type", Encode.string "splash" )
                , ( "strength", Encode.float strength )
                ]

        Horn ->
            Encode.object
                [ ( "type", Encode.string "horn" )
                ]
