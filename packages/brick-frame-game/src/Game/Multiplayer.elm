module Game.Multiplayer exposing
    ( RemotePlayer
    , Role(..)
    , RoomEvent
    , RoomState
    , RoomStatus(..)
    , SessionMode(..)
    , decodeRoomState
    , decodeRoomUpdate
    , defaultCapacity
    , emptyRoomState
    , encodeEvent
    , encodeTelemetry
    , roleFromString
    , roleToString
    , roomCodeFrom
    , sessionCapacity
    , slotBadgeClass
    , slotBorderClass
    , slotColor
    , statusFromString
    , statusToString
    )

{-| Universal 2-4 player online multiplayer data structures, state machine,
and WebRTC telemetry decoders/encoders.
-}

import Char
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


{-| Something another player's game told the room had happened, such as a
smash or a bump: its `kind`, the sender's race time `at` and position, and the
whole event as sent in `data`, for the game to read its own fields from.

`runtime/multiplayer.js` hands each event to Elm once (it deduplicates by
`slot` and `id`, and drops events older than two seconds), so a game applies
the events of a room state as they come.

-}
type alias RoomEvent =
    { slot : Int
    , id : String
    , kind : String
    , at : Float
    , position : Vec3
    , data : Decode.Value
    }


{-| Synchronized room state received from `runtime/multiplayer.js`.

`maxPlayers` is the room's capacity (2 to 8). Rooms from older hosts do not
send it and hold `defaultCapacity` players. `events` are the other players'
events this page has not seen before (see `RoomEvent`).

-}
type alias RoomState =
    { roomId : String
    , courseId : String
    , role : Role
    , mySlot : Int
    , status : RoomStatus
    , countdown : Int
    , players : List RemotePlayer
    , maxPlayers : Int
    , events : List RoomEvent
    }


{-| How a host opens a room: a two-player split screen, or a grid session.
-}
type SessionMode
    = TwoPlayers
    | GridSession


{-| The room code in what a person typed: a bare code, or a room link such as
`https://example.org/#room=ABC123`. Upper case, letters and digits only, at
most twelve characters.
-}
roomCodeFrom : String -> String
roomCodeFrom raw =
    let
        trimmed =
            String.trim raw

        lower =
            String.toLower trimmed

        afterKey key =
            String.indexes key lower
                |> List.head
                |> Maybe.map (\i -> String.dropLeft (i + String.length key) trimmed)

        tail =
            case afterKey "room=" of
                Just rest ->
                    rest

                Nothing ->
                    afterKey "join=" |> Maybe.withDefault trimmed
    in
    tail
        |> String.toUpper
        |> String.filter Char.isAlphaNum
        |> String.left 12


{-| Players a room of this mode holds.
-}
sessionCapacity : SessionMode -> Int
sessionCapacity mode =
    case mode of
        TwoPlayers ->
            2

        GridSession ->
            8


{-| Capacity of a room whose host did not say (the 4-player room of older
hosts).
-}
defaultCapacity : Int
defaultCapacity =
    4


emptyRoomState : RoomState
emptyRoomState =
    { roomId = ""
    , courseId = ""
    , role = JoinedPlayer
    , mySlot = 1
    , status = Lobby
    , countdown = 3
    , players = []
    , maxPlayers = defaultCapacity
    , events = []
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


{-| A `multiplayerRoomStateChanged` message: `Just` the room this page is
in, or `Nothing` when it is in none. `runtime/multiplayer.js` sends a room
state with an empty room id and role after `leaveMultiplayerRoom`;
`decodeRoomState` reads that as a joined room with code "", so a game that
takes it for one shows a room the player has just left, whose Leave button only
brings the same message back.
-}
decodeRoomUpdate : Decoder (Maybe RoomState)
decodeRoomUpdate =
    decodeRoomState
        |> Decode.map
            (\room ->
                if String.isEmpty (String.trim room.roomId) then
                    Nothing

                else
                    Just room
            )


{-| JSON Decoder for synchronized RoomState from JS port.
-}
decodeRoomState : Decoder RoomState
decodeRoomState =
    Decode.map2
        (\room course -> { room | courseId = course })
        decodeRoomCore
        (optionalField "courseId" Decode.string "")


{-| The room's fields without its course, which `decodeRoomState` adds.
-}
decodeRoomCore : Decoder RoomState
decodeRoomCore =
    Decode.map8
        (\rId rStr slot sStr cd pls cap evs ->
            { roomId = rId
            , courseId = ""
            , role = roleFromString rStr
            , mySlot = slot
            , status = statusFromString sStr cd
            , countdown = cd
            , players = pls
            , maxPlayers = cap
            , events = evs
            }
        )
        (Decode.field "roomId" Decode.string)
        (Decode.field "role" Decode.string)
        (Decode.field "mySlot" Decode.int)
        (Decode.field "status" Decode.string)
        (Decode.field "countdown" Decode.int)
        (Decode.field "players" (Decode.list decodePlayer))
        (optionalField "maxPlayers" Decode.int defaultCapacity)
        (optionalField "events" decodeEvents [])


{-| The room's events, skipping any this page cannot read.
-}
decodeEvents : Decoder (List RoomEvent)
decodeEvents =
    Decode.list Decode.value
        |> Decode.map (List.filterMap (Decode.decodeValue decodeEvent >> Result.toMaybe))


decodeEvent : Decoder RoomEvent
decodeEvent =
    Decode.map6 RoomEvent
        (Decode.field "slot" Decode.int)
        (Decode.field "id" (Decode.oneOf [ Decode.string, Decode.map String.fromFloat Decode.float ]))
        (Decode.field "kind" Decode.string)
        (optionalField "at" Decode.float 0.0)
        (optionalField "pos" decodeVec3Tuple ( 0.0, 0.0, 0.0 ) |> Decode.map (\( x, y, z ) -> vec3 x y z))
        Decode.value


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


{-| Encode one of this player's events for `Game.Ports.sendMultiplayerEvents`:
its `kind`, race time `at`, position and the game's own `fields`. The runtime
gives it its `id` and sends it to the room with this player's telemetry.
-}
encodeEvent :
    { kind : String
    , at : Float
    , position : Vec3
    , fields : List ( String, Encode.Value )
    }
    -> Encode.Value
encodeEvent ev =
    let
        p =
            Math.Vector3.toRecord ev.position
    in
    Encode.object
        (( "kind", Encode.string ev.kind )
            :: ( "at", Encode.float ev.at )
            :: ( "pos", Encode.list Encode.float [ p.x, p.y, p.z ] )
            :: ev.fields
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
