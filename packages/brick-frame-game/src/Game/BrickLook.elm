module Game.BrickLook exposing
    ( SkyParams
    , SurfaceUniforms
    , SurfaceVaryings
    , colour
    , skyEntity
    , surfaceFragmentShader
    )

{-| The brick look of the surfaces the Brick Frame games do not build from
parts: grass, dirt and gravel, water and the sky.

  - `surfaceFragmentShader` paints a surface as LEGO baseplates: a stud grid
    with the seams of square plates, or (where `v_studs` is 0) smooth tiles in
    running bond, in the surface's own colour. Studs and seams fade out with
    distance before they would shimmer. A game pairs it with its own vertex
    shader (the racer's terrain, the sailing game's swell), which only has to
    fill the varyings below.
  - `skyEntity` draws the sky dome with a sun disc built from yellow 1 x 1
    bricks and clouds of stacked white bricks drifting along the horizon.

Both games use the same shaders, so the grass of a racing track and the sea
of a sailing course have the same studs and seams.

-}

import LDraw.Colors as Colors
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL
import WebGL.Settings.DepthTest as DepthTest


{-| The RGB colour of an LDraw colour code.
-}
colour : Int -> Vec3
colour code =
    let
        c =
            Colors.resolveColor 0 code
    in
    vec3 c.r c.g c.b



-- SURFACES


{-| What a surface's vertex shader passes on: the world position (the stud
grid is laid out in world X and Z), the normal, the surface colour and
`v_studs`: 1 for studded plates, 0 for tiles.
-}
type alias SurfaceVaryings =
    { v_worldPos : Vec3
    , v_normal : Vec3
    , v_color : Vec3
    , v_studs : Float
    }


{-| The uniforms the surface shader reads. `u_studPitch` is the distance
between studs in metres and `u_plateStuds` the size of one square plate in
studs. `u_gloss` (0 to 1) adds the sheen and sky reflection of water.
-}
type alias SurfaceUniforms u =
    { u
        | u_eyePos : Vec3
        , u_sunDir : Vec3
        , u_fogColor : Vec3
        , u_fogNear : Float
        , u_fogFar : Float
        , u_fogMax : Float
        , u_skyAmbient : Vec3
        , u_groundAmbient : Vec3
        , u_studPitch : Float
        , u_plateStuds : Float
        , u_gloss : Float
    }


{-| Brick-look surface shader: see the module documentation.
-}
surfaceFragmentShader : WebGL.Shader {} (SurfaceUniforms u) SurfaceVaryings
surfaceFragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 u_eyePos;
        uniform vec3 u_sunDir;
        uniform vec3 u_fogColor;
        uniform float u_fogNear;
        uniform float u_fogFar;
        uniform float u_fogMax;
        uniform vec3 u_skyAmbient;
        uniform vec3 u_groundAmbient;
        uniform float u_studPitch;
        uniform float u_plateStuds;
        uniform float u_gloss;

        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_color;
        varying float v_studs;

        float hash(vec2 p) {
            return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
        }

        void main() {
            float dist = length(v_worldPos - u_eyePos);
            vec3 n = normalize(v_normal);
            vec3 col = v_color;
            vec3 sun = normalize(u_sunDir);
            vec2 sunXZ = normalize(sun.xz + vec2(0.0001, 0.0));

            // Position in studs.
            vec2 uv = v_worldPos.xz / u_studPitch;
            float studFade = 1.0 - smoothstep(u_studPitch * 70.0, u_studPitch * 220.0, dist);
            float seamFade = 1.0 - smoothstep(u_studPitch * u_plateStuds * 22.0, u_studPitch * u_plateStuds * 80.0, dist);
            // Studs and seams are on the tops only, not on steep sides.
            float upward = smoothstep(0.55, 0.85, n.y);
            studFade *= upward;
            seamFade *= upward;
            float light = 1.0;
            float highlight = 0.0;

            if (v_studs > 0.5) {
                // Square plates of u_plateStuds x u_plateStuds studs, each a
                // touch lighter or darker than its neighbours.
                vec2 plate = floor(uv / u_plateStuds);
                col *= 0.95 + 0.1 * hash(plate);
                vec2 inPlate = uv - plate * u_plateStuds;
                float edge = min(min(inPlate.x, u_plateStuds - inPlate.x), min(inPlate.y, u_plateStuds - inPlate.y));
                light *= mix(1.0, mix(0.7, 1.0, smoothstep(0.03, 0.09, edge)), seamFade);

                // One stud per cell: a lit top with a bright rim towards the sun
                // and a short shadow on the far side.
                vec2 c = fract(uv) - 0.5;
                float d = length(c);
                float top = 1.0 - smoothstep(0.27, 0.31, d);
                float shadow = (1.0 - smoothstep(0.27, 0.33, length(c + sunXZ * 0.14))) * (1.0 - top);
                float rim = smoothstep(0.17, 0.29, d) * top;
                float towards = dot(c / max(d, 0.001), sunXZ);
                light *= 1.0 - 0.28 * shadow * studFade;
                highlight = top * (0.05 + 0.16 * rim * towards) * studFade;
                light *= 1.0 - 0.18 * smoothstep(0.26, 0.3, d) * (1.0 - smoothstep(0.3, 0.34, d)) * studFade;
            } else {
                // 2 x 4 tiles in running bond.
                float row = floor(uv.y / 2.0);
                float x = uv.x + mod(row, 2.0) * 2.0;
                vec2 tile = vec2(floor(x / 4.0), row);
                col *= 0.94 + 0.12 * hash(tile + 7.0);
                vec2 inTile = vec2(x - tile.x * 4.0, uv.y - row * 2.0);
                float edge = min(min(inTile.x, 4.0 - inTile.x), min(inTile.y, 2.0 - inTile.y));
                float tileFade = (1.0 - smoothstep(u_studPitch * 160.0, u_studPitch * 480.0, dist)) * upward;
                light *= mix(1.0, mix(0.68, 1.0, smoothstep(0.03, 0.1, edge)), tileFade);
            }

            vec3 viewDir = normalize(u_eyePos - v_worldPos);
            float hemi = clamp(n.y * 0.5 + 0.5, 0.0, 1.0);
            vec3 amb = mix(u_groundAmbient, u_skyAmbient, hemi) * 0.44;
            float diff = max(dot(n, sun), 0.0) * 0.72;
            vec3 halfDir = normalize(sun + viewDir);
            float spec = pow(max(dot(n, halfDir), 0.0), 24.0 + 176.0 * u_gloss) * (0.08 + 0.5 * u_gloss);
            vec3 lit = col * (amb + vec3(0.32 + diff)) * light + vec3(highlight + spec);

            float fresnel = u_gloss * pow(1.0 - max(dot(n, viewDir), 0.0), 4.0);
            lit = mix(lit, u_skyAmbient, clamp(fresnel, 0.0, 0.6));

            float fog = smoothstep(u_fogNear, u_fogFar, dist) * u_fogMax;
            gl_FragColor = vec4(mix(lit, u_fogColor, fog), 1.0);
        }
    |]



-- SKY


{-| The sky of one frame: the camera, the colours at the zenith and the
horizon, the direction towards the sun and the time in seconds (the clouds
drift).
-}
type alias SkyParams =
    { viewProjection : Mat4
    , eye : Vec3
    , zenith : Vec3
    , horizon : Vec3
    , sunDir : Vec3
    , time : Float
    }


type alias SkyUniforms =
    { u_viewProj : Mat4
    , u_eyePos : Vec3
    , u_zenith : Vec3
    , u_horizon : Vec3
    , u_sunDir : Vec3
    , u_time : Float
    }


{-| Draw the sky dome around the camera. Draw it first: it neither tests nor
writes depth.
-}
skyEntity : SkyParams -> WebGL.Entity
skyEntity p =
    WebGL.entityWith
        [ DepthTest.always { write = False, near = 0.0, far = 1.0 } ]
        skyVertexShader
        skyFragmentShader
        skyDomeMesh
        { u_viewProj = p.viewProjection
        , u_eyePos = p.eye
        , u_zenith = p.zenith
        , u_horizon = p.horizon
        , u_sunDir = p.sunDir
        , u_time = p.time
        }


skyDomeMesh : WebGL.Mesh { position : Vec3 }
skyDomeMesh =
    let
        rings =
            12

        slices =
            24

        pt ring slice =
            let
                phi =
                    pi * (toFloat ring / toFloat rings - 0.5)

                theta =
                    2.0 * pi * toFloat slice / toFloat slices
            in
            { position = vec3 (cos phi * cos theta) (sin phi) (cos phi * sin theta) }

        cell r s =
            [ ( pt r s, pt r (s + 1), pt (r + 1) (s + 1) )
            , ( pt r s, pt (r + 1) (s + 1), pt (r + 1) s )
            ]
    in
    List.range 0 (rings - 1)
        |> List.concatMap (\r -> List.concatMap (cell r) (List.range 0 (slices - 1)))
        |> WebGL.triangles


skyVertexShader : WebGL.Shader { position : Vec3 } SkyUniforms { v_dir : Vec3 }
skyVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        uniform mat4 u_viewProj;
        uniform vec3 u_eyePos;
        varying vec3 v_dir;

        void main() {
            v_dir = position;
            gl_Position = u_viewProj * vec4(position * 40.0 + u_eyePos, 1.0);
        }
    |]


skyFragmentShader : WebGL.Shader {} SkyUniforms { v_dir : Vec3 }
skyFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 u_zenith;
        uniform vec3 u_horizon;
        uniform vec3 u_sunDir;
        uniform float u_time;
        varying vec3 v_dir;

        float hash(vec2 p) {
            return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
        }

        // Is stud column `x` (in studs) inside row `row` of cloud `slot`?
        float cloudRow(float slot, float row, float x) {
            if (row < 0.0 || row > 3.0) { return 0.0; }
            float centre = slot * 12.0 + 6.0 + (hash(vec2(slot, 1.1)) - 0.5) * 3.0
                         + (hash(vec2(slot, row + 2.0)) - 0.5) * 2.0;
            float halfWidth = 3.5 + floor(hash(vec2(slot, 9.1)) * 3.0) - row * 1.2;
            return abs(floor(x - centre) + 0.5) < halfWidth ? 1.0 : 0.0;
        }

        void main() {
            vec3 dir = normalize(v_dir);
            float h = clamp(dir.y, 0.0, 1.0);
            vec3 col = mix(u_horizon, u_zenith, pow(h, 0.6));

            // Sun: a round disc of yellow 1 x 1 bricks, seen from above, with a
            // soft glow around it.
            // The disc hangs lower in the sky than the light comes from, so it
            // shows above the horizon instead of always overhead.
            vec2 sunFlat = normalize(u_sunDir.xz + vec2(0.0001, 0.0));
            vec3 sun = normalize(vec3(sunFlat.x * 0.9, 0.42, sunFlat.y * 0.9));
            vec3 sr = normalize(cross(vec3(0.0, 1.0, 0.0), sun) + vec3(0.0001, 0.0, 0.0));
            vec3 su = cross(sun, sr);
            float facing = dot(dir, sun);
            if (facing > 0.0) {
                vec2 q = vec2(dot(dir, sr), dot(dir, su)) / facing;
                float r = length(q) / 0.09;
                col += vec3(1.0, 0.92, 0.65) * (0.32 * exp(-r * 1.1) + 0.12 * exp(-r * 0.25));
                vec2 g = q / 0.09 * 3.0;
                if (length(floor(g) + 0.5) < 3.05) {
                    vec2 f = fract(g) - 0.5;
                    float d = length(f);
                    float seam = smoothstep(0.42, 0.5, max(abs(f.x), abs(f.y)));
                    float stud = 1.0 - smoothstep(0.27, 0.31, d);
                    float rim = smoothstep(0.15, 0.29, d) * stud * dot(f / max(d, 0.001), vec2(-0.6, 0.8));
                    float ring = smoothstep(0.26, 0.3, d) * (1.0 - smoothstep(0.3, 0.35, d));
                    vec3 yellow = vec3(0.98, 0.84, 0.2);
                    col = yellow * (1.0 - 0.28 * seam - 0.15 * ring) + vec3(0.12 * stud + 0.14 * rim);
                }
            }

            // Clouds: stacks of white bricks along the horizon, four rows at
            // most, each row narrower, drifting slowly.
            float el = asin(clamp(dir.y, -1.0, 1.0));
            if (el > 0.02 && el < 0.55) {
                float x = mod((atan(dir.z, dir.x) / 6.28318 + 0.5) * 168.0 + u_time * 0.12, 168.0);
                float slot = floor(x / 12.0);
                if (hash(vec2(slot, 3.7)) < 0.8) {
                    float y = (el - 0.1) / 0.046 - floor(hash(vec2(slot, 5.3)) * 4.0);
                    float row = floor(y);
                    float fy = y - row;
                    float inside = cloudRow(slot, row, x);
                    float fade = smoothstep(0.02, 0.1, el);
                    if (inside > 0.5) {
                        float bx = (x + mod(row, 2.0)) / 2.0;
                        float fx = fract(bx);
                        float seam = max(1.0 - smoothstep(0.0, 0.04, fx), smoothstep(0.96, 1.0, fx));
                        seam = max(seam, 1.0 - smoothstep(0.0, 0.06, fy));
                        float shade = 0.9 + 0.04 * row + 0.04 * hash(vec2(floor(bx), row + slot * 3.0));
                        vec3 cloud = vec3(shade, shade, min(1.0, shade + 0.03));
                        // Shadowed underside of the bottom row, dark seams.
                        cloud *= row < 0.5 ? mix(0.8, 1.0, smoothstep(0.0, 0.45, fy)) : 1.0;
                        cloud *= 1.0 - 0.3 * seam;
                        cloud += vec3(0.06) * smoothstep(0.75, 1.0, fy);
                        col = mix(col, cloud, fade);
                    } else if (fy < 0.2 && cloudRow(slot, row - 1.0, x) > 0.5) {
                        // Studs on top of the row below.
                        float sx = fract(x);
                        if (sx > 0.2 && sx < 0.8) {
                            col = mix(col, vec3(0.93, 0.95, 0.98) + vec3(0.05) * (1.0 - sx), fade);
                        }
                    }
                }
            }

            gl_FragColor = vec4(col, 1.0);
        }
    |]
