module LDraw.CuratedCoverageTest exposing (suite)

{-| Guard: every part Brick Play's search can reach (the visible curated parts in
LDraw.CuratedParts, generated from brick-catalog by `make curated-parts`) has a
hand-made Connectivity.knownCatalogPart entry. Without one a part falls back to
primitive extraction: no connections for parts without connector primitives, the
default 1 x 1 brick box for drags and ground rest, anti-studs at tube positions
and false axle holes from drawing primitives.
-}

import Dict exposing (Dict)
import Expect
import LDraw.Connectivity exposing (BoundingBox3D, knownCatalogPart)
import LDraw.CuratedParts as CuratedParts
import Math.Vector3 as Vec3 exposing (vec3)
import Test exposing (Test, describe, test)


{-| Visible parts that keep the primitive fallback on purpose, with the reason.
-}
allowList : Dict String String
allowList =
    Dict.fromList
        [ ( "125c01.dat", "boat hull: too irregular for a hand-made box; the fallback is acceptable" )
        ]


{-| Parts modelled without connection points and without boxes, so Health skips
them (glass sits inside its window frame's box). Hidden in the catalog; kept for
old models.
-}
decorWithoutBox : List String
decorWithoutBox =
    [ "60601.dat", "60602.dat" ]


{-| The box `inferBounds []` gives a part without connector primitives.
-}
defaultBox : BoundingBox3D
defaultBox =
    { min = vec3 -10 -4 -10, max = vec3 10 24 10 }


sameBox : BoundingBox3D -> BoundingBox3D -> Bool
sameBox a b =
    Vec3.distance a.min b.min < 0.01 && Vec3.distance a.max b.max < 0.01


suite : Test
suite =
    describe "LDraw.CuratedCoverage"
        [ test "every visible curated part has a knownCatalogPart entry or an allow-list reason" <|
            \_ ->
                CuratedParts.visibleFiles
                    |> List.filter (\file -> knownCatalogPart file == Nothing && not (Dict.member file allowList))
                    |> Expect.equal []
        , test "every allow-listed part is still visible and still unmodelled" <|
            \_ ->
                Dict.keys allowList
                    |> List.filter (\file -> not (List.member file CuratedParts.visibleFiles) || knownCatalogPart file /= Nothing)
                    |> Expect.equal []
        , test "the curated list is not empty and has no duplicates" <|
            \_ ->
                let
                    files =
                        CuratedParts.visibleFiles
                in
                Expect.all
                    [ \_ -> Expect.atLeast 200 (List.length files)
                    , \_ -> Expect.equal (List.length files) (Dict.size (Dict.fromList (List.map (\f -> ( f, () )) files)))
                    ]
                    ()
        , test "visible decor entries (no connections) have a measured, non-default box" <|
            \_ ->
                CuratedParts.visibleFiles
                    |> List.filterMap
                        (\file ->
                            case knownCatalogPart file of
                                Just conn ->
                                    if List.isEmpty conn.connections then
                                        case conn.boundingBoxes of
                                            first :: _ ->
                                                if sameBox first defaultBox then
                                                    Just (file ++ ": default box")

                                                else
                                                    Nothing

                                            [] ->
                                                if List.member file decorWithoutBox then
                                                    Nothing

                                                else
                                                    Just (file ++ ": no box")

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing
                        )
                    |> Expect.equal []
        , test "glass decor entries have no connections and no boxes (Health skips them)" <|
            \_ ->
                decorWithoutBox
                    |> List.map (\file -> knownCatalogPart file |> Maybe.map (\c -> ( c.connections, c.boundingBoxes )))
                    |> Expect.equal [ Just ( [], [] ), Just ( [], [] ) ]
        ]
