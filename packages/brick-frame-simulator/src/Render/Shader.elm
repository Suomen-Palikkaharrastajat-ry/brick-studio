module Render.Shader exposing (Uniforms, fragmentShader, noInstanceColor, vertexShader)

{-| Lightweight plastic shading GLSL programs for triangle mesh rendering.
-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Render.Mesh exposing (Vertex)
import WebGL exposing (Shader)


{-| Uniforms shared between vertex and fragment shaders.

`instanceColor` is the colour substituted into vertices flagged as inheriting
(see `Render.Mesh.Vertex`). Paths that bake colours into the geometry pass
white here and are unaffected, because no vertex they produce is flagged.

-}
type alias Uniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    , viewPosition : Vec3
    , lightDirection : Vec3
    , instanceColor : Vec4
    , ambientStrength : Float
    , lightStrength : Float
    , specularStrength : Float
    , specularPower : Float
    , rimStrength : Float
    , rimPower : Float
    , vibrance : Float
    , u_ghostMode : Float
    , u_ghostAlpha : Float
    }


{-| The colour to pass as `instanceColor` when geometry already carries its own
resolved colours.
-}
noInstanceColor : Vec4
noInstanceColor =
    Vec4.vec4 1 1 1 1


type alias Varyings =
    { vColor : Vec4
    , vNormalWorld : Vec3
    , vPositionWorld : Vec3
    }


{-| Plastic vertex shader.
Transforms position through MVP matrices and passes normal (in world space)
and colour to the fragment shader.
-}
vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec4 color;
        attribute float inherit;

        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;
        uniform vec4 instanceColor;

        varying vec4 vColor;
        varying vec3 vNormalWorld;
        varying vec3 vPositionWorld;

        void main() {
            vec4 worldPosition = modelMatrix * vec4(position, 1.0);
            gl_Position = projectionMatrix * viewMatrix * worldPosition;
            // inherit is 1.0 only on surfaces that took LDraw colour 16 all the
            // way down, which is what lets one baked mesh serve every colour a
            // part is placed in.
            vColor = mix(color, instanceColor, inherit);
            // Transform normal to world space (assumes uniform scale)
            vNormalWorld = mat3(modelMatrix) * normal;
            vPositionWorld = worldPosition.xyz;
        }
    |]


{-| Plastic fragment shader.
Computes diffuse + soft specular + subtle rim light, then applies a mild
vibrance adjustment.
-}
fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 lightDirection;
        uniform vec3 viewPosition;
        uniform float ambientStrength;
        uniform float lightStrength;
        uniform float specularStrength;
        uniform float specularPower;
        uniform float rimStrength;
        uniform float rimPower;
        uniform float vibrance;
        uniform float u_ghostMode;
        uniform float u_ghostAlpha;

        varying vec4 vColor;
        varying vec3 vNormalWorld;
        varying vec3 vPositionWorld;

        void main() {
            vec3 norm = normalize(vNormalWorld);

            vec3 lightDir = normalize(lightDirection);
            vec3 viewDir = normalize(viewPosition - vPositionWorld);

            // Physical hemisphere lighting: cool sky from above, warm bounce from ground
            float hemi = norm.y * 0.5 + 0.5;
            vec3 skyAmb = vec3(0.95, 0.98, 1.05) * ambientStrength;
            vec3 gndBounce = vec3(1.05, 0.96, 0.88) * (ambientStrength * 0.7);
            vec3 ambColor = mix(gndBounce, skyAmb, hemi);

            // Soft wrap diffuse around rounded plastic contours & cylindrical studs
            float wrap = 0.12;
            float rawDiff = dot(norm, lightDir);
            float diffuse = max(0.0, (rawDiff + wrap) / (1.0 + wrap));
            vec3 sunColor = vec3(1.02, 0.99, 0.95);
            vec3 lightColor = ambColor + sunColor * (lightStrength * diffuse);

            float specular = 0.0;
            if (specularStrength > 0.0001) {
                vec3 halfDir = normalize(lightDir + viewDir);
                float NdotH = max(dot(norm, halfDir), 0.0);
                float VdotH = max(dot(viewDir, halfDir), 0.0);
                float fresnel = 0.04 + 0.96 * pow(1.0 - VdotH, 5.0);
                // Dual-lobe ABS plastic specular: sharp core highlight + soft glossy shoulder
                float sharpSpec = pow(NdotH, specularPower * 1.5) * 0.75;
                float broadSpec = pow(NdotH, max(4.0, specularPower * 0.25)) * 0.25;
                specular = (sharpSpec + broadSpec) * specularStrength * fresnel;
            }

            float rim = 0.0;
            if (rimStrength > 0.0001) {
                float facingLight = max(0.08, dot(norm, lightDir));
                rim = pow(1.0 - max(dot(norm, viewDir), 0.0), rimPower) * rimStrength * facingLight;
            }

            vec3 litColor = vColor.rgb * (lightColor * (1.0 - specular)) + vec3(specular + rim);
            if (abs(vibrance) > 0.0001) {
                float luma = dot(litColor, vec3(0.2126, 0.7152, 0.0722));
                litColor = mix(vec3(luma), litColor, 1.0 + clamp(vibrance, -0.5, 0.5));
            }

            if (u_ghostMode > 0.5) {
                float gray = dot(litColor, vec3(0.299, 0.587, 0.114));
                litColor = mix(litColor, vec3(gray), 0.75);
                gl_FragColor = vec4(clamp(litColor, 0.0, 1.0), u_ghostAlpha);
            } else {
                gl_FragColor = vec4(clamp(litColor, 0.0, 1.0), vColor.a);
            }
        }
    |]
