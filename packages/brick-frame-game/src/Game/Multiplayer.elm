module Game.Multiplayer exposing
    ( RemotePlayer
    , Role(..)
    , RoomState
    , RoomStatus(..)
    , decodeRoomState
    , emptyRoomState
    , encodeTelemetry
    , roleFromString
    , roleToString
    , slotBadgeClass
    , slotBorderClass
    , slotColor
    , statusFromString
    , statusToString
    )

{-| Universal 2-4 player online multiplayer data structures, state machine,
and WebRTC telemetry decoders/encoders.
-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Math.Vector3 exposing (Vec3, vec3)


{-| Connected player role in the multiplayer session.
-}
type Role
    = HostCommentator
    | JoinedPlayer


roleToString : Role -> String
roleToString role =
    case role of
        HostCommentator ->
            "host"

        JoinedPlayer ->
            "player"


roleFromString : String -> Role
roleFromString str =
    case str of
        "host" ->
            HostCommentator

        _ ->
            JoinedPlayer


{-| Synchronization status of the multiplayer room.
-}
type RoomStatus
    = Lobby
    | Countdown Int
    | InRace
    | Finished


statusToString : RoomStatus -> String
statusToString st =
    case st of
        Lobby ->
            "lobby"

        Countdown n ->
            "countdown"

        InRace ->
            "in_race"

        Finished ->
            "finished"


statusFromString : String -> Int -> RoomStatus
statusFromString str cd =
    case str of
        "countdown" ->
            Countdown cd

        "in_race" ->
            InRace

        "finished" ->
            Finished

        _ ->
            Lobby


{-| Real-time state of a remote participant in the room.

`vehicleId` names the player's vehicle model by content (16 hex characters,
empty when the player has none). Room updates never carry the model itself:
`runtime/multiplayer.js` fetches it once and hands it to Elm through the
`multiplayerVehicleLoaded` port as `{ id, payload }`. `vehicleLoaded` says
whether this page already holds that payload; `vehicleReady` whether the host
does.

-}
type alias RemotePlayer =
    { slot : Int -- 1..4
    , id : String
    , name : String
    , vehicleId : String
    , vehicleReady : Bool
    , vehicleLoaded : Bool
    , position : Vec3
    , rotation : Vec3 -- yaw, pitch, roll in radians or degrees
    , speed : Float
    , progress : Float
    , finished : Bool
    , finishTime : Maybe Float
    }


{-| Synchronized room state received from `runtime/multiplayer.js`.
-}
type alias RoomState =
    { roomId : String
    , role : Role
    , mySlot : Int
    , status : RoomStatus
    , countdown : Int
    , players : List RemotePlayer
    }


emptyRoomState : RoomState
emptyRoomState =
    { roomId = ""
    , role = JoinedPlayer
    , mySlot = 1
    , status = Lobby
    , countdown = 3
    , players = []
    }


{-| Distinct thematic colors for the 4 player slots (matching classic LEGO themes).
Slot 1: Classic Blue
Slot 2: Racing Red
Slot 3: Champion Yellow
Slot 4: Forest Green
-}
slotColor : Int -> String
slotColor slot =
    case slot of
        1 ->
            "#0284c7"

        -- Sky Blue
        2 ->
            "#ef4444"

        -- Red
        3 ->
            "#eab308"

        -- Yellow
        4 ->
            "#10b981"

        -- Emerald Green
        _ ->
            "#94a3b8"



-- Slate Gray


slotBadgeClass : Int -> String
slotBadgeClass slot =
    case slot of
        1 ->
            "bg-sky-500/20 text-sky-400 border-sky-500/40"

        2 ->
            "bg-red-500/20 text-red-400 border-red-500/40"

        3 ->
            "bg-amber-500/20 text-amber-400 border-amber-500/40"

        4 ->
            "bg-emerald-500/20 text-emerald-400 border-emerald-500/40"

        _ ->
            "bg-slate-500/20 text-slate-400 border-slate-500/40"


slotBorderClass : Int -> String
slotBorderClass slot =
    case slot of
        1 ->
            "border-sky-500"

        2 ->
            "border-red-500"

        3 ->
            "border-amber-500"

        4 ->
            "border-emerald-500"

        _ ->
            "border-slate-500"


{-| JSON Decoder for synchronized RoomState from JS port.
-}
decodeRoomState : Decoder RoomState
decodeRoomState =
    Decode.map6
        (\rId rStr slot sStr cd pls ->
            { roomId = rId
            , role = roleFromString rStr
            , mySlot = slot
            , status = statusFromString sStr cd
            , countdown = cd
            , players = pls
            }
        )
        (Decode.field "roomId" Decode.string)
        (Decode.field "role" Decode.string)
        (Decode.field "mySlot" Decode.int)
        (Decode.field "status" Decode.string)
        (Decode.field "countdown" Decode.int)
        (Decode.field "players" (Decode.list decodePlayer))


decodePlayer : Decoder RemotePlayer
decodePlayer =
    Decode.map8
        (\slot id name ( vId, vReady, vLoaded ) ( px, py, pz ) ( rx, ry, rz ) ( sp, pr ) ( fin, finT ) ->
            { slot = slot
            , id = id
            , name = name
            , vehicleId = vId
            , vehicleReady = vReady
            , vehicleLoaded = vLoaded
            , position = vec3 px py pz
            , rotation = vec3 rx ry rz
            , speed = sp
            , progress = pr
            , finished = fin
            , finishTime = finT
            }
        )
        (Decode.field "slot" Decode.int)
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        decodeVehicleRef
        (Decode.field "position" decodeVec3Tuple)
        (Decode.field "rotation" decodeVec3Tuple)
        (Decode.map2 Tuple.pair
            (Decode.field "speed" Decode.float)
            (Decode.field "progress" Decode.float)
        )
        (Decode.map2 Tuple.pair
            (Decode.field "finished" Decode.bool)
            (Decode.maybe (Decode.field "finishTime" Decode.float))
        )


{-| `vehicleId`, `vehicleReady` and `vehicleLoaded`; all optional so that room
states from older runtimes (which sent `vehicleHash` instead) still decode.
-}
decodeVehicleRef : Decoder ( String, Bool, Bool )
decodeVehicleRef =
    Decode.map3 (\a b c -> ( a, b, c ))
        (optionalField "vehicleId" Decode.string "")
        (optionalField "vehicleReady" Decode.bool False)
        (optionalField "vehicleLoaded" Decode.bool False)


optionalField : String -> Decoder a -> a -> Decoder a
optionalField name decoder fallback =
    Decode.maybe (Decode.field name decoder)
        |> Decode.map (Maybe.withDefault fallback)


decodeVec3Tuple : Decoder ( Float, Float, Float )
decodeVec3Tuple =
    Decode.list Decode.float
        |> Decode.andThen
            (\list ->
                case list of
                    [ x, y, z ] ->
                        Decode.succeed ( x, y, z )

                    _ ->
                        Decode.succeed ( 0.0, 0.0, 0.0 )
            )


{-| Encode outgoing player telemetry for transmission over WebRTC data channel.
-}
encodeTelemetry :
    { pos : ( Float, Float, Float )
    , rot : ( Float, Float, Float )
    , speed : Float
    , progress : Float
    , finished : Bool
    , finishTime : Maybe Float
    }
    -> Encode.Value
encodeTelemetry t =
    let
        ( px, py, pz ) =
            t.pos

        ( rx, ry, rz ) =
            t.rot

        baseList =
            [ ( "pos", Encode.list Encode.float [ px, py, pz ] )
            , ( "rot", Encode.list Encode.float [ rx, ry, rz ] )
            , ( "speed", Encode.float t.speed )
            , ( "progress", Encode.float t.progress )
            , ( "finished", Encode.bool t.finished )
            ]

        withFinish =
            case t.finishTime of
                Just ft ->
                    ( "finishTime", Encode.float ft ) :: baseList

                Nothing ->
                    baseList
    in
    Encode.object withFinish
