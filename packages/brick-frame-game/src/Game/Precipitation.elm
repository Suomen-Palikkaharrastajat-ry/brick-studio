module Game.Precipitation exposing
    ( Field
    , streakHeight
    , transforms
    )

{-| Falling rain streaks around the camera, as model matrices.

The streaks themselves (a mesh, a shader and a colour) belong to the game,
so this module only works out where each streak goes: two layers scroll
down at different speeds, slanted by the wind, and the second layer appears
only in heavy rain. Brick Sails draws the streak mesh with its ocean shader
and calls `transforms` once per frame; this is the same placement that the
sails game used before it was shared.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


{-| What the streaks follow: the camera position, the scene time in seconds
and the wind, which slants the streaks and speeds them up.
-}
type alias Field =
    { eye : Vec3
    , time : Float
    , windSpeed : Float
    , windDirection : Float
    }


{-| Height in metres of the box the streaks fall through.
-}
streakHeight : Float
streakHeight =
    30


{-| The model matrix of each visible streak for an intensity from 0 to 1.

Nothing falls below an intensity of 0.05. Above 0.5 a second, slower layer
of streaks falls as well.

-}
transforms : Float -> Field -> List Mat4
transforms intensity field =
    if intensity <= 0.05 then
        []

    else
        layer field 22 0
            ++ (if intensity > 0.5 then
                    layer field 17 0.5

                else
                    []
               )


{-| One layer of streaks, falling at `speed` metres a second with a phase
`offset` (0 to 1) so that two layers do not move in step.
-}
layer : Field -> Float -> Float -> List Mat4
layer field speed offset =
    let
        e =
            Vec3.toRecord field.eye

        lean =
            clamp 0 0.5 (field.windSpeed / 40)

        windRad =
            degrees field.windDirection

        fall =
            fract (field.time * speed / streakHeight + offset) * streakHeight
    in
    List.map
        (\k ->
            Mat4.makeTranslate3 e.x (e.y + streakHeight * (0.5 - toFloat k) - fall) e.z
                |> Mat4.rotate windRad (vec3 0 1 0)
                |> (\m -> Mat4.mul m (shear lean))
        )
        [ 0, 1 ]


{-| Slant a streak sideways in proportion to the wind's lean.
-}
shear : Float -> Mat4
shear lean =
    Mat4.fromRecord
        { m11 = 1
        , m21 = 0
        , m31 = 0
        , m41 = 0
        , m12 = 0
        , m22 = 1
        , m32 = -lean
        , m42 = 0
        , m13 = 0
        , m23 = 0
        , m33 = 1
        , m43 = 0
        , m14 = 0
        , m24 = 0
        , m34 = 0
        , m44 = 1
        }


fract : Float -> Float
fract x =
    x - toFloat (floor x)
