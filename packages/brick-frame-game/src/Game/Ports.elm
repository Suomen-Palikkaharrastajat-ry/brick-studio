port module Game.Ports exposing
    ( binaryHashDecoded
    , calibrateTilt
    , closeQrScanner
    , copyShareUrl
    , createMultiplayerRoom
    , decodeBinaryHash
    , fileContentReceived
    , fileLoadError
    , geometryFlattenFailed
    , geometryFlattened
    , hashChanged
    , joinMultiplayerRoom
    , leaveMultiplayerRoom
    , multiplayerError
    , multiplayerRoomStateChanged
    , multiplayerVehicleLoaded
    , openQrScanner
    , qrCodeScanned
    , qrScannerError
    , requestFileUpload
    , requestGeometryFlatten
    , requestTiltPermission
    , saveLeaderboard
    , sendMultiplayerTelemetry
    , setAudioMuted
    , setMultiplayerVehicle
    , setUrlHash
    , startMultiplayerCountdown
    , storeLanguage
    , tiltOrientationReceived
    , tiltStatus
    , tiltSteer
    , viewportResized
    )

{-| Ports every Brick Frame game shares with the JS runtime: the asset and
file bridges of `initBricksRuntime`, binary hash decoding and sharing,
localStorage persistence (`game-runtime.js`), multiplayer rooms
(`multiplayer.js`), the QR scanner (`qr-camera-scanner.js`) and tilt input
(raw orientation from `game-runtime.js`, calibrated steering from
`tilt-steer.js`).

A game keeps its own `Ports` module only for the ports whose payload is its
own: the audio telemetry (`updateAudioState`) and one-shot sounds (`playSfx`).
Port names are global, so `main.js` sees the same `app.ports` either way.

-}

import Json.Decode


{-| Request the browser file picker for `.ldr`, `.mpd`, `.dat`, or `.io`.
-}
port requestFileUpload : () -> Cmd msg


{-| Raw LDraw file text delivered from upload, drag-and-drop, or `.io` extraction.
-}
port fileContentReceived : (String -> msg) -> Sub msg


{-| File load error string from the JS runtime.
-}
port fileLoadError : (String -> msg) -> Sub msg


{-| Geometry flattening worker port required by `initBricksRuntime`.
-}
port requestGeometryFlatten : String -> Cmd msg


{-| Flattened geometry response from `initBricksRuntime`.
-}
port geometryFlattened : (String -> msg) -> Sub msg


{-| Geometry worker failure from `initBricksRuntime`.
-}
port geometryFlattenFailed : (String -> msg) -> Sub msg


{-| Update browser URL hash without triggering page reload.
-}
port setUrlHash : String -> Cmd msg


{-| Copy the full shareable game URL (or a `#...` hash on this page) to the clipboard.
-}
port copyShareUrl : String -> Cmd msg


{-| Ask JS to inflate a `#b/<base64url>` string pasted into the in-app input.
-}
port decodeBinaryHash : String -> Cmd msg


{-| Receive decompressed `#b/...` payload bytes (or `Nothing` on error).
-}
port binaryHashDecoded : (Maybe (List Int) -> msg) -> Sub msg


{-| Notify Elm when the browser address bar hash changes externally.
-}
port hashChanged : (String -> msg) -> Sub msg


{-| Viewport resize notifications from `initBricksRuntime`.
-}
port viewportResized : ({ width : Int, height : Int } -> msg) -> Sub msg


{-| Persist master mute state to `localStorage` and update JS AudioContext gain.
-}
port setAudioMuted : Bool -> Cmd msg


{-| Ask iOS/browser for `DeviceOrientationEvent` tilt permission; the raw
orientation then streams through `tiltOrientationReceived` (`game-runtime.js`).
-}
port requestTiltPermission : () -> Cmd msg


{-| Stream device tilt orientation (`gamma` relative to the screen, `beta`)
once `requestTiltPermission` was granted (`game-runtime.js`).
-}
port tiltOrientationReceived : ({ available : Bool, gamma : Float, beta : Float } -> msg) -> Sub msg


{-| Set the current phone tilt as the neutral (straight ahead) steering point.
Sent when a race starts or restarts; handled in `tilt-steer.js`.
-}
port calibrateTilt : () -> Cmd msg


{-| Tilt steering from `tilt-steer.js`, -1 (left) .. 1 (right), after calibration
and dead zone. Only sent while tilt steering is switched on.
-}
port tiltSteer : (Float -> msg) -> Sub msg


{-| Whether tilt steering can be used on this device and whether it is on.
`tilt-steer.js` sends this at start-up and whenever the player toggles it.
-}
port tiltStatus : ({ available : Bool, enabled : Bool } -> msg) -> Sub msg


{-| Save JSON-encoded local leaderboard records to `localStorage`.
-}
port saveLeaderboard : String -> Cmd msg


{-| Persist chosen language code (`"fi"` or `"en"`) to `localStorage`.
-}
port storeLanguage : String -> Cmd msg


{-| Create a new multiplayer room as host/commentator. `maxPlayers` is the
room's capacity (2 to 8); the runtime holds 4 when it is not given.
-}
port createMultiplayerRoom :
    { roomId : String
    , courseId : String
    , name : String
    , vehicleHash : String
    , maxPlayers : Int
    }
    -> Cmd msg


{-| Join an existing multiplayer room as a player.
-}
port joinMultiplayerRoom : { roomId : String, name : String, vehicleHash : String } -> Cmd msg


{-| Stream the local vehicle's telemetry to room peers over WebRTC.
-}
port sendMultiplayerTelemetry :
    { pos : List Float
    , rot : List Float
    , speed : Float
    , progress : Float
    , finished : Bool
    }
    -> Cmd msg


{-| Trigger the synchronized 3-second race countdown.
-}
port startMultiplayerCountdown : () -> Cmd msg


{-| Disconnect from multiplayer room.
-}
port leaveMultiplayerRoom : () -> Cmd msg


{-| Incoming room state update from WebRTC signaling runtime.
-}
port multiplayerRoomStateChanged : (Json.Decode.Value -> msg) -> Sub msg


{-| Another player's vehicle arrived: `id` is the `vehicleId` the room state
names, `payload` what that player shared (a build link, or a game's own
vehicle name). Sent once per id and room session; the payload never travels
inside room updates.
-}
port multiplayerVehicleLoaded : ({ id : String, payload : String } -> msg) -> Sub msg


{-| Change the local vehicle (what `multiplayerVehicleLoaded` hands to the
others) while in a multiplayer room.
-}
port setMultiplayerVehicle : String -> Cmd msg


{-| Multiplayer error notification.
-}
port multiplayerError : (String -> msg) -> Sub msg


{-| Open the camera QR scanner modal to capture a build link.
-}
port openQrScanner : () -> Cmd msg


{-| Close camera QR scanner modal.
-}
port closeQrScanner : () -> Cmd msg


{-| QR code scanned payload containing a `#b/...` hash.
-}
port qrCodeScanned : (String -> msg) -> Sub msg


{-| QR scanner error or permission denied message.
-}
port qrScannerError : (String -> msg) -> Sub msg
