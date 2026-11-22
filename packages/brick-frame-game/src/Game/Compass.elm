module Game.Compass exposing
    ( ApparentWind
    , PointOfSail(..)
    , apparentWind
    , cardinalName
    , headingFromVector
    , pointOfSail
    , pointOfSailName
    , relativeAngle
    , vectorFromHeading
    )

{-| Universal 2D/3D navigation math: compass headings, apparent wind vector calculation,
relative bearings, and points of sail.
-}


{-| Result of apparent wind calculation.
`speed` is apparent wind velocity in m/s (or knots).
`relativeAngle` is angle relative to ship bow in degrees (-180 to +180, negative = port, positive = starboard).
-}
type alias ApparentWind =
    { speed : Float
    , relativeAngle : Float
    }


{-| Traditional points of sail relative to true/apparent wind.
-}
type PointOfSail
    = InIrons
    | CloseHauled
    | BeamReach
    | BroadReach
    | Running


{-| Compass heading in degrees [0, 360) from a 2D velocity or forward vector (dx, dz).
Convention: 0° = North (+Z), 90° = East (+X), 180° = South (-Z), 270° = West (-X).
-}
headingFromVector : Float -> Float -> Float
headingFromVector dx dz =
    let
        rad =
            atan2 dx dz

        deg =
            rad * 180.0 / pi
    in
    if deg < 0 then
        deg + 360.0

    else
        deg


{-| 2D unit vector (dx, dz) for a given compass heading in degrees.
-}
vectorFromHeading : Float -> ( Float, Float )
vectorFromHeading deg =
    let
        rad =
            deg * pi / 180.0
    in
    ( sin rad, cos rad )


{-| Relative angle between target and source angle in degrees, normalized to [-180, 180].
-}
relativeAngle : Float -> Float -> Float
relativeAngle target current =
    let
        diff =
            modBy 360 (round (target - current + 540)) - 180
    in
    toFloat diff


{-| Cardinal and intercardinal direction name from compass heading in degrees.
-}
cardinalName : Float -> String
cardinalName deg =
    let
        norm =
            modBy 360 (round deg)
    in
    if norm >= 338 || norm < 23 then
        "N"

    else if norm < 68 then
        "NE"

    else if norm < 113 then
        "E"

    else if norm < 158 then
        "SE"

    else if norm < 203 then
        "S"

    else if norm < 248 then
        "SW"

    else if norm < 293 then
        "W"

    else
        "NW"


{-| Compute apparent wind vector from true wind and ship motion.
- `windSpeed`: true wind velocity (m/s)
- `windHeading`: true wind blowing TOWARDS this compass heading (degrees)
- `shipSpeed`: ship velocity over water (m/s)
- `shipHeading`: ship heading (degrees)
-}
apparentWind : { windSpeed : Float, windHeading : Float } -> { shipSpeed : Float, shipHeading : Float } -> ApparentWind
apparentWind wind ship =
    let
        ( twX, twZ ) =
            vectorFromHeading wind.windHeading

        trueVx =
            twX * wind.windSpeed

        trueVz =
            twZ * wind.windSpeed

        ( shX, shZ ) =
            vectorFromHeading ship.shipHeading

        shipVx =
            shX * ship.shipSpeed

        shipVz =
            shZ * ship.shipSpeed

        -- Apparent wind velocity vector = True wind velocity - Ship velocity
        appVx =
            trueVx - shipVx

        appVz =
            trueVz - shipVz

        appSpeed =
            sqrt (appVx * appVx + appVz * appVz)

        appHeading =
            headingFromVector appVx appVz

        relAngle =
            relativeAngle appHeading ship.shipHeading
    in
    { speed = appSpeed
    , relativeAngle = relAngle
    }


{-| Classify the point of sail based on the absolute relative wind angle (0° = dead into wind, 180° = dead downwind).
-}
pointOfSail : Float -> PointOfSail
pointOfSail absAngleDeg =
    let
        a =
            abs absAngleDeg
    in
    if a < 38.0 then
        InIrons

    else if a < 65.0 then
        CloseHauled

    else if a < 115.0 then
        BeamReach

    else if a < 155.0 then
        BroadReach

    else
        Running


{-| Human-readable name for point of sail.
-}
pointOfSailName : PointOfSail -> String
pointOfSailName pos =
    case pos of
        InIrons ->
            "In Irons"

        CloseHauled ->
            "Close-Hauled"

        BeamReach ->
            "Beam Reach"

        BroadReach ->
            "Broad Reach"

        Running ->
            "Running Downwind"
