module Studio.Floor exposing
    ( Vertex
    , baseplateMesh
    , entity
    )

{-| A BrickLink Studio style baseplate floor on the ground plane (render space Y = 0).

Features:

  - Outer baseplate boundary contour with lip and rounded/chamfered corners.
  - Major grid lines every 8 studs and subtle grid lines every 1 stud.
  - Stud rings (circular octagons) at every stud position.
  - "Travels with the camera": the contour center snaps to the 20 LDU lattice near
    the camera focus/target, ensuring grid lines and studs remain pinned in place
    relative to placed parts as the camera pans.
  - Rendered with `DepthTest.lessOrEqual` (depth write disabled) and alpha blending.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Render.Camera as Camera exposing (Camera)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


type alias Vertex =
    { position : Vec3
    , color : Vec4
    }


type alias Uniforms =
    { viewMatrix : Mat4
    , projectionMatrix : Mat4
    , modelMatrix : Mat4
    }


vertexShader : Shader Vertex Uniforms { vColor : Vec4 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec4 color;

        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;
        uniform mat4 modelMatrix;

        varying vec4 vColor;

        void main() {
            vColor = color;
            gl_Position = projectionMatrix * viewMatrix * (modelMatrix * vec4(position, 1.0));
        }
    |]


fragmentShader : Shader {} Uniforms { vColor : Vec4 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec4 vColor;

        void main() {
            gl_FragColor = vColor;
        }
    |]


{-| Color palette for the baseplate floor elements.
-}
borderColor : Vec4
borderColor =
    vec4 0.5 0.58 0.68 0.85


lipColor : Vec4
lipColor =
    vec4 0.48 0.55 0.65 0.5


majorGridColor : Vec4
majorGridColor =
    vec4 0.45 0.52 0.62 0.45


minorGridColor : Vec4
minorGridColor =
    vec4 0.4 0.46 0.54 0.22


studRingColor : Vec4
studRingColor =
    vec4 0.45 0.52 0.62 0.32


groundY : Float
groundY =
    0.02


{-| Precomputed baseplate lines (32x32 studs, 640 LDU across).
-}
baseplateLines : List ( Vertex, Vertex )
baseplateLines =
    let
        halfExtent =
            320.0

        lipOffset =
            316.0

        chamfer =
            4.0

        -- Outer border with chamfered corners
        outerBorder =
            [ -- Top edge
              ( { position = vec3 (-halfExtent + chamfer) groundY halfExtent, color = borderColor }
              , { position = vec3 (halfExtent - chamfer) groundY halfExtent, color = borderColor }
              )
            , -- Top-right chamfer
              ( { position = vec3 (halfExtent - chamfer) groundY halfExtent, color = borderColor }
              , { position = vec3 halfExtent groundY (halfExtent - chamfer), color = borderColor }
              )
            , -- Right edge
              ( { position = vec3 halfExtent groundY (halfExtent - chamfer), color = borderColor }
              , { position = vec3 halfExtent groundY (-halfExtent + chamfer), color = borderColor }
              )
            , -- Bottom-right chamfer
              ( { position = vec3 halfExtent groundY (-halfExtent + chamfer), color = borderColor }
              , { position = vec3 (halfExtent - chamfer) groundY -halfExtent, color = borderColor }
              )
            , -- Bottom edge
              ( { position = vec3 (halfExtent - chamfer) groundY -halfExtent, color = borderColor }
              , { position = vec3 (-halfExtent + chamfer) groundY -halfExtent, color = borderColor }
              )
            , -- Bottom-left chamfer
              ( { position = vec3 (-halfExtent + chamfer) groundY -halfExtent, color = borderColor }
              , { position = vec3 -halfExtent groundY (-halfExtent + chamfer), color = borderColor }
              )
            , -- Left edge
              ( { position = vec3 -halfExtent groundY (-halfExtent + chamfer), color = borderColor }
              , { position = vec3 -halfExtent groundY (halfExtent - chamfer), color = borderColor }
              )
            , -- Top-left chamfer
              ( { position = vec3 -halfExtent groundY (halfExtent - chamfer), color = borderColor }
              , { position = vec3 (-halfExtent + chamfer) groundY halfExtent, color = borderColor }
              )
            ]

        -- Inner lip line
        innerLip =
            [ ( { position = vec3 -lipOffset groundY lipOffset, color = lipColor }
              , { position = vec3 lipOffset groundY lipOffset, color = lipColor }
              )
            , ( { position = vec3 lipOffset groundY lipOffset, color = lipColor }
              , { position = vec3 lipOffset groundY -lipOffset, color = lipColor }
              )
            , ( { position = vec3 lipOffset groundY -lipOffset, color = lipColor }
              , { position = vec3 -lipOffset groundY -lipOffset, color = lipColor }
              )
            , ( { position = vec3 -lipOffset groundY -lipOffset, color = lipColor }
              , { position = vec3 -lipOffset groundY lipOffset, color = lipColor }
              )
            ]

        -- Grid lines along X and Z
        steps =
            List.range -16 16

        gridLineAlongX step =
            let
                x =
                    toFloat step * 20.0

                color =
                    if modBy 8 step == 0 then
                        majorGridColor

                    else
                        minorGridColor
            in
            ( { position = vec3 x groundY -halfExtent, color = color }
            , { position = vec3 x groundY halfExtent, color = color }
            )

        gridLineAlongZ step =
            let
                z =
                    toFloat step * 20.0

                color =
                    if modBy 8 step == 0 then
                        majorGridColor

                    else
                        minorGridColor
            in
            ( { position = vec3 -halfExtent groundY z, color = color }
            , { position = vec3 halfExtent groundY z, color = color }
            )

        gridLines =
            List.map gridLineAlongX steps
                ++ List.map gridLineAlongZ steps

        -- Stud rings: 8-segment circle (octagon) radius 6.0 LDU at each stud center
        studIndices =
            List.range -16 15

        studCenters =
            List.concatMap
                (\ix ->
                    let
                        cx =
                            toFloat ix * 20.0 + 10.0
                    in
                    List.map
                        (\iz ->
                            let
                                cz =
                                    toFloat iz * 20.0 + 10.0
                            in
                            ( cx, cz )
                        )
                        studIndices
                )
                studIndices

        studRadius =
            6.0

        circleSegments ( cx, cz ) =
            let
                angles =
                    [ 0.0, 0.785398, 1.570796, 2.356194, 3.141593, 3.926991, 4.712389, 5.497787 ]

                points =
                    List.map
                        (\a ->
                            vec3 (cx + studRadius * cos a) groundY (cz + studRadius * sin a)
                        )
                        angles

                rotatedPoints =
                    case points of
                        first :: rest ->
                            rest ++ [ first ]

                        [] ->
                            []
            in
            List.map2
                (\p1 p2 ->
                    ( { position = p1, color = studRingColor }
                    , { position = p2, color = studRingColor }
                    )
                )
                points
                rotatedPoints

        studRings =
            List.concatMap circleSegments studCenters
    in
    outerBorder ++ innerLip ++ gridLines ++ studRings


{-| Single compiled mesh for the baseplate floor, created once.
-}
baseplateMesh : Mesh Vertex
baseplateMesh =
    WebGL.lines baseplateLines


{-| WebGL entity rendering the baseplate floor.
The floor tracks the camera focus point on the ground, snapped to the 20 LDU lattice.
-}
entity : Camera -> Float -> Entity
entity camera aspect =
    let
        camX =
            Vec3.getX camera.target

        camZ =
            Vec3.getZ camera.target

        centerX =
            20.0 * toFloat (round (camX / 20.0))

        centerZ =
            20.0 * toFloat (round (camZ / 20.0))

        modelMatrix =
            Mat4.makeTranslate (vec3 centerX 0.0 centerZ)

        uniforms =
            { viewMatrix = Camera.viewMatrix camera
            , projectionMatrix = Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)
            , modelMatrix = modelMatrix
            }
    in
    WebGL.entityWith
        [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
        , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
        ]
        vertexShader
        fragmentShader
        baseplateMesh
        uniforms
