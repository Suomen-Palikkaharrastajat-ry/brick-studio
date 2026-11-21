module Studio.GearFeedback exposing
    ( GearFeedback
    , GearRelation(..)
    , feedbackText
    , feedbackTextIn
    , gearFeedback
    , pill
    , pillIn
    )

{-| Positioning feedback for a single selected gear: how it sits against its
nearest other gear, judged with the same rules `Gear.Detect.buildGearGraph`
uses (see `Detect.meshFacts`).

`gearFeedback` is pure and works on any `LDraw.Document`, so a host can show it
without a `Studio.Core.Model`. `feedbackText` gives the status line and `pill`
the status pill that Brick Play shows under the canvas in Technic mode.

    gearFeedback [ selectedId ] document
        |> Maybe.map GearFeedback.pill

-}

import Data
import Gear.Detect as Detect
import Gear.Types exposing (GearInstance, GearKind(..), GearSpec)
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import I18n exposing (Language(..))
import LDraw.Document as Document exposing (Document, Placement)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3


{-| How the selected gear relates to its nearest other gear.

  - `Meshes` — the two gears mesh.
  - `TooFar` / `TooClose` — a parallel or worm pair whose axes are right but
    whose centres are `distance` LDU apart where `required` (the sum of the
    pitch radii) is needed.
  - `NotParallel` — a spur-like pair whose axes point different ways.
  - `NotPerpendicular` — a crossed (bevel, crown) or worm pair whose axes are
    not at right angles.
  - `NotSamePlane` — parallel axes, but the gears sit `offset` LDU apart
    along the axle, so their teeth cannot touch.
  - `AxlesMustCross` — a bevel or crown pair at right angles whose axles pass
    each other `gap` LDU apart instead of crossing.
  - `TeethFaceAway` — a bevel or crown pair on crossing axles where a gear's
    teeth point away from the crossing.
  - `AxleOffset` — a bevel or crown pair on crossing axles where the gear
    labelled `gear` sits `actual` LDU from the other gear's axle but needs
    `required`.
  - `CannotMesh` — the two kinds of gear cannot mesh in this arrangement (for
    example a single bevel with a spur gear, or a crown with a double bevel).
  - `NoOtherGear` — the selected gear is the only gear.

-}
type GearRelation
    = Meshes
    | TooFar { distance : Float, required : Float }
    | TooClose { distance : Float, required : Float }
    | NotParallel
    | NotPerpendicular
    | NotSamePlane { offset : Float }
    | AxlesMustCross { gap : Float }
    | TeethFaceAway
    | AxleOffset { gear : String, actual : Float, required : Float }
    | CannotMesh
    | NoOtherGear


{-| Feedback for the selected gear: its short label (`"24T"`, `"24T crown"`,
`"Worm"`, see `gearLabel`), the nearest other gear's label (when there is one)
and their relation.
-}
type alias GearFeedback =
    { gear : String
    , other : Maybe String
    , relation : GearRelation
    }


{-| Positioning feedback for a selection of placement ids. `Nothing` unless the
selection is exactly one placement and that placement is a known gear.

The partner is the nearest (by centre distance) gear the selected gear meshes
with, if any; otherwise simply the nearest other gear in the main model.

    gearFeedback [ selectedId ] document

-}
gearFeedback : List Int -> Document -> Maybe GearFeedback
gearFeedback selectedIds document =
    case selectedIds of
        [ selectedId ] ->
            let
                gears =
                    List.filterMap
                        (\p -> Maybe.map (\g -> ( p.id, g )) (placementGear p))
                        (Document.mainPlacements document)
            in
            case List.filter (\( id, _ ) -> id == selectedId) gears of
                ( _, selected ) :: _ ->
                    let
                        others =
                            List.filter (\( id, _ ) -> id /= selectedId) gears
                                |> List.map Tuple.second
                                |> List.sortBy (\g -> Vec3.distance g.worldPosition selected.worldPosition)

                        partner =
                            case List.filter (Detect.meshing selected) others of
                                meshed :: _ ->
                                    Just meshed

                                [] ->
                                    List.head others
                    in
                    Just
                        { gear = gearLabel selected.spec
                        , other = Maybe.map (.spec >> gearLabel) partner
                        , relation =
                            partner
                                |> Maybe.map (classify selected)
                                |> Maybe.withDefault NoOtherGear
                        }

                [] ->
                    Nothing

        _ ->
            Nothing


{-| Classify one pair with the facts of `Detect.meshFacts`: first whether the
kinds can mesh at all, then the parallel checks or the crossed/worm checks.
-}
classify : GearInstance -> GearInstance -> GearRelation
classify selected other =
    let
        facts =
            Detect.meshFacts selected other

        rimLike spec =
            Detect.isBevelLike spec || Detect.isCrownLike spec
    in
    if Detect.meshing selected other then
        Meshes

    else
        case facts.mode of
            Nothing ->
                CannotMesh

            Just mode ->
                if
                    (mode == Detect.ParallelMesh)
                        && (facts.absDot < 0.5)
                        && (rimLike selected.spec || rimLike other.spec)
                then
                    -- A crown or double bevel at right angles to a gear it
                    -- only meshes with on parallel axles (the mode is
                    -- parallel only when no crossed layout exists for the
                    -- pair). Worm pairs are not caught here: they mesh with
                    -- any rotating gear.
                    CannotMesh

                else
                    case mode of
                        Detect.ParallelMesh ->
                            if facts.absDot < 0.9 then
                                NotParallel

                            else if not facts.axisOk then
                                NotSamePlane { offset = facts.axialOffset }

                            else
                                distanceRelation facts

                        Detect.WormMesh ->
                            if not facts.axisOk then
                                NotPerpendicular

                            else
                                distanceRelation facts

                        Detect.CrossedMesh ->
                            if not facts.axisOk then
                                NotPerpendicular

                            else if not facts.axesCross then
                                AxlesMustCross { gap = facts.skew }

                            else if not facts.facesToward then
                                TeethFaceAway

                            else
                                case facts.crossOffsets of
                                    Just c ->
                                        -- Name the gear that is outside its
                                        -- window (the selected one on a tie).
                                        if c.secondExcess > c.firstExcess + 1.0e-6 then
                                            AxleOffset { gear = gearLabel other.spec, actual = abs c.second, required = c.secondRequired }

                                        else
                                            AxleOffset { gear = gearLabel selected.spec, actual = abs c.first, required = c.firstRequired }

                                    Nothing ->
                                        NotPerpendicular


distanceRelation : Detect.MeshFacts -> GearRelation
distanceRelation facts =
    if facts.distance > facts.expected then
        TooFar { distance = facts.distance, required = facts.expected }

    else
        TooClose { distance = facts.distance, required = facts.expected }


{-| A top-level placement as a gear instance, built exactly as
`Detect.extractGears` builds one for a part at the top of the model.
-}
placementGear : Placement -> Maybe GearInstance
placementGear placement =
    gearSpecFor placement.part
        |> Maybe.map
            (\spec ->
                { id = placement.id
                , spec = spec
                , color = placement.color
                , worldPosition = Mat4.transform placement.matrix (Vec3.vec3 0 0 0)
                , worldMatrix = placement.matrix
                }
            )


{-| Short label for a gear: tooth count, plus the kind when it is not a plain
spur gear.
-}
gearLabel : GearSpec -> String
gearLabel spec =
    if spec.kind == WormGear then
        String.fromInt spec.teeth ++ "-start worm"

    else
        let
            suffix =
                case spec.kind of
                    CrownGear ->
                        "T crown"

                    BevelGear ->
                        "T bevel"

                    DoubleBevelGear ->
                        "T double bevel"

                    TurntableGear ->
                        "T turntable"

                    SpurGear ->
                        "T"

                    WormGear ->
                        "T"
        in
        String.fromInt spec.teeth ++ suffix


gearSpecFor : String -> Maybe GearSpec
gearSpecFor file =
    List.head (List.filter (\spec -> spec.partFile == file) Data.gearParts)


{-| A short status line for the feedback pill with English defaults.
-}
feedbackText : GearFeedback -> String
feedbackText =
    feedbackTextIn En


{-| A short status line for the feedback pill in the requested language, e.g.
`"24T ↔ 24T: 40 LDU apart — needs 60"` or `"Meshes with 24T ✓"`.
-}
feedbackTextIn : Language -> GearFeedback -> String
feedbackTextIn lang feedback =
    let
        other =
            Maybe.withDefault "?" feedback.other

        -- Whole LDU, or half LDU where that matters (a crown needs 12.5).
        ldu x =
            let
                halves =
                    round (abs x * 2)

                sign =
                    if halves > 0 && x < 0 then
                        "-"

                    else
                        ""
            in
            if modBy 2 halves == 0 then
                sign ++ String.fromInt (halves // 2)

            else
                sign ++ String.fromInt (halves // 2) ++ ".5"
    in
    case feedback.relation of
        Meshes ->
            I18n.gearMeshesWith lang other

        TooFar { distance, required } ->
            I18n.gearApartNeeds lang { gear = feedback.gear, other = other, dist = ldu distance, req = ldu required }

        TooClose { distance, required } ->
            I18n.gearApartNeeds lang { gear = feedback.gear, other = other, dist = ldu distance, req = ldu required }

        NotParallel ->
            I18n.gearNotParallel lang { gear = feedback.gear, other = other }

        NotPerpendicular ->
            I18n.gearNotPerpendicular lang { gear = feedback.gear, other = other }

        NotSamePlane { offset } ->
            I18n.gearNotSamePlane lang { gear = feedback.gear, other = other, offset = ldu offset }

        AxlesMustCross { gap } ->
            I18n.gearAxlesMustCross lang { gear = feedback.gear, other = other, gap = ldu gap }

        TeethFaceAway ->
            I18n.gearTeethFaceAway lang { gear = feedback.gear, other = other }

        AxleOffset { gear, actual, required } ->
            I18n.gearAxleOffset lang { gear = feedback.gear, other = other, x = gear, actual = ldu actual, required = ldu required }

        CannotMesh ->
            I18n.gearCannotMesh lang { gear = feedback.gear, other = other }

        NoOtherGear ->
            I18n.gearNoOtherGear lang feedback.gear


{-| The status pill for the feedback with English defaults.
-}
pill : GearFeedback -> Html msg
pill =
    pillIn En


{-| The status pill for the feedback in the requested language.
-}
pillIn : Language -> GearFeedback -> Html msg
pillIn lang feedback =
    let
        meshes =
            feedback.relation == Meshes
    in
    div
        [ Attr.attribute "role" "status"
        , Attr.attribute "aria-live" "polite"
        , Attr.attribute "data-gear-feedback" ""
        , Attr.style "align-self" "center"
        , Attr.style "max-width" "100%"
        , Attr.style "padding" "0.3125rem 0.875rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "backdrop-filter" "blur(8px)"
        , Attr.style "border-radius" "9999px"
        , Attr.style "border"
            (if meshes then
                "1px solid var(--color-brand-yellow)"

             else
                "1px solid rgba(255, 255, 255, 0.15)"
            )
        , Attr.style "color"
            (if meshes then
                "var(--color-brand-yellow)"

             else
                "var(--color-text-on-dark)"
            )
        , Attr.style "font-size" "0.8125rem"
        , Attr.style "font-weight" "600"
        , Attr.style "white-space" "nowrap"
        , Attr.style "overflow" "hidden"
        , Attr.style "text-overflow" "ellipsis"
        , Attr.style "font-variant-numeric" "tabular-nums"
        ]
        [ text (feedbackTextIn lang feedback) ]
