module Game.MultiplayerUI exposing
    ( CommentatorHeaderConfig
    , CountdownConfig
    , SlotBadgeConfig
    , viewCommentatorHeader
    , viewCountdownOverlay
    , viewPlayerBadge
    , viewWaitingSlotCard
    )

{-| Universal UI components for 2-4 player WebRTC multiplayer sessions:
Host Commentator top bar, player status badges, waiting slot frames, and countdown overlays.
-}

import Game.Multiplayer as Multiplayer exposing (RemotePlayer, RoomState, RoomStatus(..))
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type alias CommentatorHeaderConfig msg =
    { room : RoomState
    , startLabel : String
    , onCopyShareLink : msg
    , onStartCountdown : msg
    }


{-| Floating commentator control bar positioned at top center of host screen.
-}
viewCommentatorHeader : CommentatorHeaderConfig msg -> Html msg
viewCommentatorHeader cfg =
    div
        [ Attr.class "absolute top-3 inset-x-0 mx-auto w-fit z-30 flex items-center gap-3 px-4 py-2 rounded-2xl bg-slate-950/90 backdrop-blur-md border border-white/10 shadow-2xl pointer-events-auto"
        ]
        [ span
            [ Attr.class "font-mono font-extrabold text-sky-400 text-xs px-2.5 py-1 rounded-lg bg-sky-500/10 border border-sky-500/30" ]
            [ text ("ROOM: " ++ cfg.room.roomId) ]
        , button
            [ Attr.type_ "button"
            , onClick cfg.onCopyShareLink
            , Attr.class "text-xs font-bold px-2.5 py-1 rounded-xl bg-slate-800 hover:bg-slate-700 active:scale-95 text-slate-200 transition-all flex items-center gap-1.5 cursor-pointer border border-white/10"
            ]
            [ text "Copy Link" ]
        , case cfg.room.status of
            Lobby ->
                button
                    [ Attr.type_ "button"
                    , onClick cfg.onStartCountdown
                    , Attr.class "text-xs font-black px-3.5 py-1 rounded-xl bg-gradient-to-r from-emerald-500 to-teal-500 hover:brightness-110 active:scale-95 text-slate-950 shadow-lg shadow-emerald-500/25 transition-all flex items-center gap-1.5 cursor-pointer"
                    ]
                    [ text cfg.startLabel ]

            Countdown n ->
                span
                    [ Attr.class "text-xs font-extrabold text-amber-400 uppercase tracking-wider px-2.5 py-1 rounded-lg bg-amber-500/10 border border-amber-500/20" ]
                    [ text ("STARTING IN " ++ String.fromInt n ++ "...") ]

            InRace ->
                span
                    [ Attr.class "text-xs font-extrabold text-emerald-400 uppercase tracking-wider px-2.5 py-1 rounded-lg bg-emerald-500/10 border border-emerald-500/20" ]
                    [ text "RACE IN PROGRESS" ]

            Finished ->
                span
                    [ Attr.class "text-xs font-extrabold text-purple-400 uppercase tracking-wider px-2.5 py-1 rounded-lg bg-purple-500/10 border border-purple-500/20" ]
                    [ text "RACE FINISHED" ]
        ]


type alias SlotBadgeConfig =
    { slot : Int
    , name : String
    , speedLabel : String
    , finished : Bool
    }


{-| Player info pill positioned over a viewport quadrant.
-}
viewPlayerBadge : SlotBadgeConfig -> Html msg
viewPlayerBadge cfg =
    div
        [ Attr.class "absolute top-2 left-2 z-10 flex items-center gap-2 px-2.5 py-1 rounded-xl bg-slate-900/80 backdrop-blur-md border border-white/10 shadow-lg" ]
        [ span
            [ Attr.class ("px-1.5 py-0.5 rounded text-[10px] font-black uppercase " ++ Multiplayer.slotBadgeClass cfg.slot) ]
            [ text ("P" ++ String.fromInt cfg.slot) ]
        , span [ Attr.class "text-xs font-bold text-white truncate max-w-[110px]" ] [ text cfg.name ]
        , span [ Attr.class "text-xs font-mono font-bold text-sky-400" ] [ text cfg.speedLabel ]
        , if cfg.finished then
            span [ Attr.class "text-xs font-black text-emerald-400" ] [ text "FINISHED" ]

          else
            text ""
        ]


{-| Empty slot card when waiting for a remote player to join.
-}
viewWaitingSlotCard : Int -> String -> String -> Html msg
viewWaitingSlotCard slot roomId roleLabel =
    div
        [ Attr.class "relative w-full h-full flex flex-col items-center justify-center border border-dashed border-slate-800 bg-slate-950/60 p-4 text-center select-none" ]
        [ span
            [ Attr.class ("px-2.5 py-0.5 rounded-full text-xs font-black uppercase tracking-wider mb-2 " ++ Multiplayer.slotBadgeClass slot) ]
            [ text ("Slot " ++ String.fromInt slot) ]
        , span [ Attr.class "text-xs font-semibold text-slate-400" ]
            [ text ("Waiting for " ++ roleLabel ++ " to join...") ]
        , span [ Attr.class "text-[11px] font-mono text-slate-500 mt-1" ]
            [ text ("Invite: #room=" ++ roomId) ]
        ]


type alias CountdownConfig =
    { secondsRemaining : Int
    }


{-| Large center-screen countdown display.
-}
viewCountdownOverlay : CountdownConfig -> Html msg
viewCountdownOverlay cfg =
    div
        [ Attr.class "absolute inset-0 z-50 flex flex-col items-center justify-center pointer-events-none bg-black/60" ]
        [ div
            [ Attr.class "text-9xl font-black font-mono tracking-tight select-none"
            , Attr.style "color" "#fbbf24"
            , Attr.style "text-shadow" "0 0 40px rgba(245, 158, 11, 0.7)"
            ]
            [ text
                (if cfg.secondsRemaining > 0 then
                    String.fromInt cfg.secondsRemaining

                 else
                    "GO!"
                )
            ]
        ]
