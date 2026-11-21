module Viewport.Snapshot exposing
    ( CropRect
    , encodePreviewRequest
    , encodeRequest
    , encodeRequestWithCrop
    , encodeRequestWithLight
    , encodeRequestWithOptions
    , imageHeight
    , imageWidth
    , previewBudgetMs
    , previewHeight
    , previewWidth
    , sceneTriangles
    , timeBudgetMs
    )

{-| The request a host sends to the offline path tracer (`captureRender`).

The request carries the camera, the output size, a time budget and every
visible triangle of the scene in render space. `sceneTriangles` picks the same
geometry the viewport's `view` draws, so the snapshot matches the screen:

  - a baked scene (`Scene`) is drawn whole;
  - otherwise the instanced scene is drawn placement by placement, leaving out
    the sources in `hiddenSources` and drawing ghost context at ghost alpha;
  - placements are drawn at full detail, because the proxy boxes that stand in
    for far-away placements are a screen-size optimisation, not geometry.

-}

import Dict
import Json.Encode as Encode
import LDraw.ColorData exposing (Finish(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Render.Camera as Camera exposing (Camera)
import Render.Instanced as Instanced exposing (InstancedScene)
import Render.Scene exposing (Scene)
import Render.Shader as Shader
import Render.Snapshot as Snapshot
import Set exposing (Set)


{-| Output width in pixels.
-}
imageWidth : Int
imageWidth =
    1920


{-| Output height in pixels.
-}
imageHeight : Int
imageHeight =
    1080


{-| How long the path tracer may run, in milliseconds.
-}
timeBudgetMs : Int
timeBudgetMs =
    30000


{-| Rough preview output width in pixels.
-}
previewWidth : Int
previewWidth =
    320


{-| Rough preview output height in pixels.
-}
previewHeight : Int
previewHeight =
    180


{-| How long the quick preview path tracer may run, in milliseconds.
-}
previewBudgetMs : Int
previewBudgetMs =
    80


{-| Every visible triangle of the viewport's scene, flattened for the path tracer.

`scene` is the baked scene when the model took the baked path; otherwise
`instanced` is drawn. Placements whose source is in `hidden` are left out, and
those outside `activeSources` (when set) are drawn as ghosts.

-}
sceneTriangles :
    { scene : Maybe Scene
    , instanced : Maybe InstancedScene
    , hidden : Set Int
    , activeSources : Maybe (Set Int)
    }
    -> List Float
sceneTriangles config =
    case ( config.scene, config.instanced ) of
        ( Just scene, _ ) ->
            Snapshot.placedTriangles
                { matrix = Mat4.identity
                , instanceColor = Shader.noInstanceColor
                , finish = Solid
                , ghost = False
                }
                scene.surface

        ( Nothing, Just instanced ) ->
            instanced.placements
                |> List.concatMap (placementTriangles instanced config.hidden config.activeSources)

        ( Nothing, Nothing ) ->
            []


{-| The triangles of one visible placement, or none when it is hidden or has no mesh.
-}
placementTriangles : InstancedScene -> Set Int -> Maybe (Set Int) -> Instanced.Placement -> List Float
placementTriangles instanced hidden activeSources placement =
    if Set.member placement.source hidden then
        []

    else
        case Dict.get placement.part instanced.parts of
            Nothing ->
                []

            Just mesh ->
                Snapshot.placedTriangles
                    { matrix = placement.matrix
                    , instanceColor = placement.color
                    , finish = placement.finish
                    , ghost = isGhost activeSources placement.source
                    }
                    mesh.surface


isGhost : Maybe (Set Int) -> Int -> Bool
isGhost activeSources source =
    case activeSources of
        Just sources ->
            not (Set.member source sources)

        Nothing ->
            False


{-| Normalized crop rectangle `[0, 1]` in viewport coordinates.
-}
type alias CropRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


{-| The JSON object `captureRender` takes: output size, the camera, and the triangles.
-}
encodeRequest : { filename : String, camera : Camera, triangles : List Float } -> Encode.Value
encodeRequest config =
    encodeRequestWithLight
        { filename = config.filename
        , camera = config.camera
        , lightDirection = Nothing
        , triangles = config.triangles
        }


{-| The JSON object `captureRender` takes, with an optional key light direction
for the path tracer.
-}
encodeRequestWithLight :
    { filename : String
    , camera : Camera
    , lightDirection : Maybe Vec3
    , triangles : List Float
    }
    -> Encode.Value
encodeRequestWithLight config =
    encodeRequestWithCrop
        { filename = config.filename
        , camera = config.camera
        , lightDirection = config.lightDirection
        , triangles = config.triangles
        , crop = Nothing
        }


{-| The JSON object `captureRender` takes, with optional key light and crop bounds.
-}
encodeRequestWithCrop :
    { filename : String
    , camera : Camera
    , lightDirection : Maybe Vec3
    , triangles : List Float
    , crop : Maybe CropRect
    }
    -> Encode.Value
encodeRequestWithCrop config =
    encodeRequestWithOptions
        { filename = config.filename
        , camera = config.camera
        , lightDirection = config.lightDirection
        , triangles = config.triangles
        , crop = config.crop
        , outlineEdges = Nothing
        , shader = Nothing
        , width = Nothing
        , height = Nothing
        , timeBudgetMs = Nothing
        , animation = Nothing
        , viewAspect = Nothing
        }


{-| Full configuration for `captureRender`, including optional outline edge rasterization,
shader preset, animation options, and custom width/height/budget.

An animation is encoded with an explicit `"motion"`: `"motor"` when
`framesExtraTriangles` is `Just` (a static camera, with each frame's turning
gears as that frame's extra triangles; a frame may be empty), otherwise
`"turntable"` (the camera makes one full turn around its target).

`frames` is sent as `"frameCount"`: the runtime renders exactly that many
frames (a motor animation renders one per `framesExtraTriangles` entry) and
spreads `durationSeconds` over them with whole-millisecond frame durations.
`"fps"` is also sent, as `max 1 (round (frames / durationSeconds))`, for older
runtimes and logs only. `samplesPerPixel`, when given, overrides the runtime's
per-resolution sample count for every frame.

`viewAspect`, when given, is the aspect of the live view the camera was framed
in: the camera keeps that aspect, `crop` is the frame within the view, and
`width` × `height` is the exact output size.

-}
encodeRequestWithOptions :
    { filename : String
    , camera : Camera
    , lightDirection : Maybe Vec3
    , triangles : List Float
    , crop : Maybe CropRect
    , outlineEdges : Maybe Bool
    , shader : Maybe String
    , width : Maybe Int
    , height : Maybe Int
    , timeBudgetMs : Maybe Int
    , animation : Maybe { frames : Int, durationSeconds : Float, samplesPerPixel : Maybe Int, framesExtraTriangles : Maybe (List (List Float)) }
    , viewAspect : Maybe Float
    }
    -> Encode.Value
encodeRequestWithOptions config =
    let
        targetWidth =
            Maybe.withDefault imageWidth config.width

        targetHeight =
            Maybe.withDefault imageHeight config.height

        targetBudget =
            Maybe.withDefault timeBudgetMs config.timeBudgetMs

        lightFields =
            case config.lightDirection of
                Just dir ->
                    [ ( "lightDirection", encodeVec3 dir ) ]

                Nothing ->
                    []

        viewAspectFields =
            case config.viewAspect of
                Just aspect ->
                    [ ( "viewAspect", Encode.float aspect ) ]

                Nothing ->
                    []

        cropFields =
            case config.crop of
                Just c ->
                    [ ( "crop"
                      , Encode.object
                            [ ( "x", Encode.float c.x )
                            , ( "y", Encode.float c.y )
                            , ( "width", Encode.float c.width )
                            , ( "height", Encode.float c.height )
                            ]
                      )
                    ]

                Nothing ->
                    []

        outlineFields =
            case config.outlineEdges of
                Just outline ->
                    [ ( "outlineEdges", Encode.bool outline ) ]

                Nothing ->
                    []

        shaderFields =
            case config.shader of
                Just s ->
                    [ ( "shader", Encode.string s ) ]

                Nothing ->
                    []

        animFields =
            case config.animation of
                Just anim ->
                    let
                        samplesField =
                            case anim.samplesPerPixel of
                                Just spp ->
                                    [ ( "samplesPerPixel", Encode.int spp ) ]

                                Nothing ->
                                    []

                        framesField =
                            case anim.framesExtraTriangles of
                                Just frames ->
                                    [ ( "motion", Encode.string "motor" )
                                    , ( "framesExtraTriangles"
                                      , Encode.list (Encode.list Encode.float) frames
                                      )
                                    ]

                                Nothing ->
                                    [ ( "motion", Encode.string "turntable" ) ]
                    in
                    [ ( "animation"
                      , Encode.object
                            ([ ( "frameCount", Encode.int anim.frames )
                             , ( "fps", Encode.int (animationFps anim) )
                             , ( "durationSeconds", Encode.float anim.durationSeconds )
                             ]
                                ++ samplesField
                                ++ framesField
                            )
                      )
                    ]

                Nothing ->
                    []
    in
    Encode.object
        ([ ( "width", Encode.int targetWidth )
         , ( "height", Encode.int targetHeight )
         , ( "transparent", Encode.bool True )
         , ( "filename", Encode.string config.filename )
         , ( "timeBudgetMs", Encode.int targetBudget )
         , ( "camera"
           , Encode.object
                [ ( "position", encodeVec3 (Camera.position config.camera) )
                , ( "target", encodeVec3 (Camera.lookTarget config.camera) )

                -- Matches the view matrix in Render.Camera.
                , ( "up", encodeVec3 (Vec3.vec3 0 1 0) )
                , ( "fovYDegrees", Encode.float Camera.fovYDegrees )
                ]
           )
         ]
            ++ lightFields
            ++ viewAspectFields
            ++ cropFields
            ++ outlineFields
            ++ shaderFields
            ++ animFields
            ++ [ ( "triangles", Encode.list Encode.float config.triangles ) ]
        )


{-| Encode a fast WebGL modeler canvas copy request for framing preview.
Omitting "triangles" tells snapshot.js to capture the live 3D canvas instantly (0ms).
-}
encodePreviewRequest :
    { filename : String
    , camera : Camera
    , lightDirection : Maybe Vec3
    , triangles : List Float
    }
    -> Encode.Value
encodePreviewRequest config =
    Encode.object
        [ ( "width", Encode.int imageWidth )
        , ( "height", Encode.int imageHeight )
        , ( "transparent", Encode.bool False )
        , ( "filename", Encode.string config.filename )
        ]


encodeVec3 : Vec3 -> Encode.Value
encodeVec3 v =
    Encode.list Encode.float
        [ Vec3.getX v, Vec3.getY v, Vec3.getZ v ]


{-| The informational frame rate sent next to `frameCount`.
-}
animationFps : { a | frames : Int, durationSeconds : Float } -> Int
animationFps anim =
    if anim.durationSeconds <= 0 then
        1

    else
        max 1 (round (toFloat anim.frames / anim.durationSeconds))
