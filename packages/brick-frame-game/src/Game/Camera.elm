module Game.Camera exposing
    ( CameraMode(..)
    , OrbitState
    , chasePosition
    , damp
    , dampVec3
    , helmPosition
    , orbitPosition
    )

{-| Universal 3D camera controllers: exponential damping, spherical orbit angles,
and chase-behind / helm viewing positions.
-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)


{-| Standard camera perspective modes.
-}
type CameraMode
    = Chase
    | Orbit
    | Helm


{-| Spherical orbit camera parameters.
-}
type alias OrbitState =
    { yaw : Float
    , pitch : Float
    , distance : Float
    }


{-| Frame-rate independent exponential damping.
`damp lambda current target dt` moves `current` towards `target` with rate `lambda`.
-}
damp : Float -> Float -> Float -> Float -> Float
damp lambda current target dt =
    let
        t =
            clamp 0.0 1.0 (1.0 - (e ^ (0.0 - (lambda * dt))))
    in
    current + (target - current) * t


{-| Vector3 exponential damping.
-}
dampVec3 : Float -> Vec3 -> Vec3 -> Float -> Vec3
dampVec3 lambda current target dt =
    let
        c =
            Vec3.toRecord current

        tg =
            Vec3.toRecord target
    in
    vec3
        (damp lambda c.x tg.x dt)
        (damp lambda c.y tg.y dt)
        (damp lambda c.z tg.z dt)


{-| Compute world-space camera position from spherical orbit coordinates around a center point.
-}
orbitPosition : Vec3 -> OrbitState -> Vec3
orbitPosition center orbit =
    let
        c =
            Vec3.toRecord center

        clampedPitch =
            clamp (-pi / 2.2) (pi / 2.2) orbit.pitch

        dist =
            max 1.0 orbit.distance

        dx =
            dist * cos clampedPitch * sin orbit.yaw

        dy =
            dist * sin clampedPitch

        dz =
            dist * cos clampedPitch * cos orbit.yaw
    in
    vec3 (c.x + dx) (c.y + dy) (c.z + dz)


{-| Compute chase camera position behind a moving object given its target center and forward heading vector.
-}
chasePosition : Vec3 -> Vec3 -> Float -> Float -> Vec3
chasePosition target forward distance height =
    let
        t =
            Vec3.toRecord target

        f =
            Vec3.toRecord forward

        -- normalized horizontal forward
        fLen =
            sqrt (f.x * f.x + f.z * f.z)

        ( nFx, nFz ) =
            if fLen > 0.0001 then
                ( f.x / fLen, f.z / fLen )

            else
                ( 0.0, 1.0 )

        camX =
            t.x - nFx * distance

        camY =
            t.y + height

        camZ =
            t.z - nFz * distance
    in
    vec3 camX camY camZ


{-| Compute first-person / helm camera position looking forward from the bridge/cockpit.
-}
helmPosition : Vec3 -> Vec3 -> Float -> Vec3
helmPosition target forward heightOffset =
    let
        t =
            Vec3.toRecord target

        f =
            Vec3.toRecord forward

        fLen =
            sqrt (f.x * f.x + f.z * f.z)

        ( nFx, nFz ) =
            if fLen > 0.0001 then
                ( f.x / fLen, f.z / fLen )

            else
                ( 0.0, 1.0 )
    in
    vec3 (t.x + nFx * 0.2) (t.y + heightOffset) (t.z + nFz * 0.2)
