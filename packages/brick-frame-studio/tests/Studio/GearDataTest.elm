module Studio.GearDataTest exposing (suite)

{-| Guard against the pitch radii in `Data.gearParts` and the gear boxes in
`LDraw.Connectivity` drifting apart: they are kept in two places because the
simulator package cannot depend on the generated data package.
-}

import Data
import Expect
import Gear.Types exposing (GearKind(..))
import LDraw.Connectivity as Connectivity
import Math.Vector3 as Vec3
import Test exposing (Test, test)


suite : Test
suite =
    test "every spur, bevel, double bevel and crown in Data.gearParts has a Connectivity box of its pitch radius" <|
        \_ ->
            let
                checked =
                    Data.gearParts
                        |> List.filter
                            (\spec ->
                                List.member spec.kind [ SpurGear, BevelGear, DoubleBevelGear, CrownGear ]
                            )

                largestX part =
                    Connectivity.knownCatalogPart part
                        |> Maybe.map
                            (\conn ->
                                List.foldl (\box m -> max m (Vec3.getX box.max)) 0 conn.boundingBoxes
                            )
            in
            Expect.equal
                (List.map (\spec -> ( spec.partFile, Just spec.pitchRadius )) checked)
                (List.map (\spec -> ( spec.partFile, largestX spec.partFile )) checked)
