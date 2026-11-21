module Studio.GearFeedbackTest exposing (suite)

{-| Unit tests for `Studio.GearFeedback`: the relation reported for a selected
gear against its nearest partner, judged with the rules of `Gear.Detect`.
-}

import Expect
import I18n exposing (Language(..))
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (vec3)
import Studio.GearFeedback as GearFeedback exposing (GearRelation(..))
import Test exposing (Test, describe, test)


{-| Local +Z onto world +X.
-}
axisX : Mat4
axisX =
    Mat4.makeRotate (pi / 2) (vec3 0 1 0)


at : Float -> Float -> Float -> Mat4 -> Mat4
at x y z rot =
    Mat4.mul (Mat4.makeTranslate3 x y z) rot


{-| A document with the given parts; the first one is selected.
-}
scene : List ( String, Mat4 ) -> ( Document, Int )
scene parts =
    List.foldl
        (\( part, m ) ( doc, ids ) ->
            let
                ( next, id ) =
                    Document.addPlacement "" { part = part, color = 16, matrix = m } doc
            in
            ( next, ids ++ [ id ] )
        )
        ( Document.empty, [] )
        parts
        |> (\( doc, ids ) -> ( doc, List.head ids |> Maybe.withDefault -1 ))


relationOf : List ( String, Mat4 ) -> Maybe GearRelation
relationOf parts =
    let
        ( doc, selected ) =
            scene parts
    in
    GearFeedback.gearFeedback [ selected ] doc
        |> Maybe.map .relation


textOf : List ( String, Mat4 ) -> Maybe String
textOf parts =
    let
        ( doc, selected ) =
            scene parts
    in
    GearFeedback.gearFeedback [ selected ] doc
        |> Maybe.map (GearFeedback.feedbackTextIn En)


suite : Test
suite =
    describe "Studio.GearFeedback"
        [ test "the reported scene (12T 69778 and 24T, 2 studs across and 3 plates up) meshes" <|
            \_ ->
                relationOf
                    [ ( "69778.dat", Mat4.makeTranslate3 -20 -46 -10 )
                    , ( "3648.dat", Mat4.makeTranslate3 20 -70 -10 )
                    ]
                    |> Expect.equal (Just Meshes)
        , test "a crown and an 8T on parallel axles 40 apart mesh" <|
            \_ ->
                relationOf
                    [ ( "3650b.dat", Mat4.identity )
                    , ( "3647.dat", Mat4.makeTranslate3 40 0 0 )
                    ]
                    |> Expect.equal (Just Meshes)
        , test "a crown and an 8T at right angles on a crossing axle mesh" <|
            \_ ->
                relationOf
                    [ ( "3650b.dat", Mat4.identity )
                    , ( "3647.dat", at 30 0 12.5 axisX )
                    ]
                    |> Expect.equal (Just Meshes)
        , test "two single bevels (6589) on parallel axles need right angles" <|
            \_ ->
                relationOf
                    [ ( "6589.dat", Mat4.identity )
                    , ( "6589.dat", Mat4.makeTranslate3 30 0 0 )
                    ]
                    |> Expect.equal (Just NotPerpendicular)
        , test "double bevels at right angles on skew axles: the axles must cross" <|
            \_ ->
                case
                    relationOf
                        [ ( "32270.dat", at 0 0 20 Mat4.identity )
                        , ( "32270.dat", at 20 5 0 axisX )
                        ]
                of
                    Just (AxlesMustCross { gap }) ->
                        Expect.within (Expect.Absolute 1.0e-3) 5 gap

                    _ ->
                        Expect.fail "Expected AxlesMustCross"
        , test "a single bevel and a 24T spur cannot mesh" <|
            \_ ->
                relationOf
                    [ ( "6589.dat", Mat4.identity )
                    , ( "3648.dat", Mat4.makeTranslate3 45 0 0 )
                    ]
                    |> Expect.equal (Just CannotMesh)
        , test "a crown and a double bevel at right angles cannot mesh" <|
            \_ ->
                relationOf
                    [ ( "3650b.dat", Mat4.identity )
                    , ( "32270.dat", at 30 0 17.5 axisX )
                    ]
                    |> Expect.equal (Just CannotMesh)
        , test "two spurs at right angles are not parallel" <|
            \_ ->
                relationOf
                    [ ( "3648.dat", Mat4.identity )
                    , ( "3648.dat", at 60 0 0 axisX )
                    ]
                    |> Expect.equal (Just NotParallel)
        , test "an 8T behind the crown has its teeth facing away" <|
            \_ ->
                relationOf
                    [ ( "3650b.dat", Mat4.identity )
                    , ( "3647.dat", at 30 0 -12.5 axisX )
                    ]
                    |> Expect.equal (Just TeethFaceAway)
        , test "double bevels 12T/20T swapped report the along-axle offset, not the centre distance" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        relationOf
                            [ ( "32270.dat", at 0 0 20 Mat4.identity )
                            , ( "32269.dat", at 30 0 0 axisX )
                            ]
                            |> (\relation ->
                                    case relation of
                                        Just (AxleOffset o) ->
                                            Expect.equal ( "12T double bevel", 20, 30 ) ( o.gear, round o.actual, round o.required )

                                        _ ->
                                            Expect.fail "Expected AxleOffset"
                               )
                    , \_ ->
                        textOf
                            [ ( "32270.dat", at 0 0 20 Mat4.identity )
                            , ( "32269.dat", at 30 0 0 axisX )
                            ]
                            |> Expect.equal (Just "12T double bevel ↔ 20T double bevel: 12T double bevel is 20 LDU from the other axle — needs 30")
                    ]
                    ()
        , test "a crown and a worm 46 apart are too far (not 'cannot mesh')" <|
            \_ ->
                relationOf
                    [ ( "3650b.dat", Mat4.identity )
                    , ( "4716.dat", at 0 46 0 axisX )
                    ]
                    |> Expect.equal (Just (TooFar { distance = 46, required = 40 }))
        , test "a crown and a worm at the pitch sum mesh" <|
            \_ ->
                relationOf
                    [ ( "3650b.dat", Mat4.identity )
                    , ( "4716.dat", at 0 40 0 axisX )
                    ]
                    |> Expect.equal (Just Meshes)
        , test "a single bevel and a worm 30 apart are too far, not 'cannot mesh'" <|
            \_ ->
                case
                    relationOf
                        [ ( "6589.dat", Mat4.identity )
                        , ( "4716.dat", at 0 30 0 axisX )
                        ]
                of
                    Just (TooFar _) ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected TooFar"
        , test "AxleOffset names the gear outside its window, not the larger raw deviation" <|
            \_ ->
                -- Selected 12T at |s| = 22.4 (+2.4, inside +2.5), the other
                -- at 18.4 (-1.6, outside -1.5).
                case
                    relationOf
                        [ ( "32270.dat", at 0 0 22.4 Mat4.identity )
                        , ( "32270.dat", at 18.4 0 0 axisX )
                        ]
                of
                    Just (AxleOffset o) ->
                        Expect.equal ( 184, 200 ) ( round (o.actual * 10), round (o.required * 10) )

                    _ ->
                        Expect.fail "Expected AxleOffset"
        , test "half-LDU requirements are shown as such (crown needs 12.5)" <|
            \_ ->
                textOf
                    [ ( "3650b.dat", Mat4.identity )
                    , ( "3647.dat", at 30 0 16 axisX )
                    ]
                    |> Expect.equal (Just "24T crown ↔ 8T: 24T crown is 16 LDU from the other axle — needs 12.5")
        , test "parallel spurs too close still report the centre distance" <|
            \_ ->
                textOf
                    [ ( "3648.dat", Mat4.identity )
                    , ( "3648.dat", Mat4.makeTranslate3 40 0 0 )
                    ]
                    |> Expect.equal (Just "24T ↔ 24T: 40 LDU apart — needs 60")
        ]
