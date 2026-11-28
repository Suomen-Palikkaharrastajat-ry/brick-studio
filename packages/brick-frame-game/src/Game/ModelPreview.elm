module Game.ModelPreview exposing (Request, request)

{-| Pictures of whole models (a starter base, a saved build) from the shared
thumbnail pipeline (`renderModelThumbnail` in
`brick-frame-viewport/runtime/thumbnail-pipeline.js`), the same renderer the
part palette uses.

A game declares the two ports the pipeline wires in `initThumbnails`:

    port requestModelThumbnails : List ModelPreview.Request -> Cmd msg

    port modelThumbnailLoaded : ({ key : String, url : String } -> msg) -> Sub msg

and asks for a picture with `request key document`. The pipeline sends the
picture back under the same `key`.

@docs Request, request

-}

import Dict
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document)
import LDraw.Writer as Writer


{-| What the pipeline draws: the host's name for the picture, the model as
flat LDraw text (every part with its world matrix and resolved colour code)
and the RGB of each colour code it uses.
-}
type alias Request =
    { key : String
    , text : String
    , colours : List { code : Int, rgb : List Float }
    }


{-| The picture request of a document, under `key`.
-}
request : String -> Document -> Request
request key doc =
    let
        codes =
            Document.flattenedPlacements doc
                |> List.map (\p -> ( p.color, () ))
                |> Dict.fromList
                |> Dict.keys
    in
    { key = key
    , text = Writer.writeLines (Document.flattenedLines doc)
    , colours =
        List.map
            (\code ->
                let
                    c =
                        Colors.resolveColor 16 code
                in
                { code = code, rgb = [ c.r, c.g, c.b ] }
            )
            codes
    }
