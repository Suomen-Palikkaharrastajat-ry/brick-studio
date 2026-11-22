module Game.ModelHash exposing
    ( extractBinaryFragment
    , sanitizeModelHash
    )

{-| Utility functions for parsing and manipulating #b/... binary model hash fragments
while cleanly separating game-specific parameters (e.g. tracks, courses, seeds).
-}


{-| Extract the #b/<payload> fragment from any URL hash or string.
-}
extractBinaryFragment : String -> Maybe String
extractBinaryFragment raw =
    let
        trimmed =
            String.trim raw

        withoutPound =
            if String.startsWith "#" trimmed then
                String.dropLeft 1 trimmed

            else
                trimmed

        segments =
            String.split "&" withoutPound
    in
    List.filter (\s -> String.startsWith "b/" s) segments
        |> List.head
        |> Maybe.map (\seg -> "#" ++ seg)


{-| Clean a hash to ensure it has a valid leading '#' and standard form.
-}
sanitizeModelHash : String -> String
sanitizeModelHash raw =
    let
        trimmed =
            String.trim raw
    in
    if String.isEmpty trimmed then
        ""

    else if String.startsWith "#" trimmed then
        trimmed

    else
        "#" ++ trimmed
