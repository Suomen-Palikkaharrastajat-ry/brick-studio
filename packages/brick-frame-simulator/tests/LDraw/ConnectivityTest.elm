module LDraw.ConnectivityTest exposing (suite)

{-| Unit tests for LDraw.Connectivity: connection detection, snap transformation math,
and 3D SAT collision detection.
-}

import Expect
import LDraw.Connectivity as Connectivity
    exposing
        ( BoundingBox3D
        , ConnectionPoint
        , ConnectionType(..)
        , OBB
        , SnapCandidate
        , SnapPriority(..)
        , alignedContactCount
        , checkCollisions
        , effectiveEnvelope
        , extractConnections
        , extractPartConnectivity
        , isCompatible
        , isSnapColliding
        , isStudded
        , isUpright
        , knownCatalogPart
        , makeAxle
        , makeGear
        , snapEnterDistance
        , snapReleaseDistance
        , solveSnap
        , solveSnapCandidates
        , solveSnapCandidatesWithPriority
        , solveSnapWithHysteresis
        , solveSnapWithHysteresisAndPredicate
        , solveSnapWithHysteresisAndPredicateWithPriority
        , solveSnapWithPredicate
        , solveSnapWithPredicateWithPriority
        , studOffset
        , testOBB
        , toOBB
        , transformDirection
        , transformPoint
        )
import LDraw.Types exposing (LDrawLine(..), SubFileData)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LDraw.Connectivity"
        [ compatibilityTests
        , extractionTests
        , studLatticeTests
        , snapSolverTests
        , brickStacking2x2Tests
        , collisionSATTests
        , shortestArcSnapTests
        , microscaleCatalogTests
        ]


compatibilityTests : Test
compatibilityTests =
    describe "isCompatible"
        [ test "Stud meshes with AntiStud" <|
            \_ ->
                Expect.equal True (isCompatible Stud AntiStud)
        , test "AntiStud meshes with Stud" <|
            \_ ->
                Expect.equal True (isCompatible AntiStud Stud)
        , test "Stud does not mesh with Stud" <|
            \_ ->
                Expect.equal False (isCompatible Stud Stud)
        , test "Technic pin male meshes with female" <|
            \_ ->
                Expect.equal True
                    (isCompatible
                        (TechnicPinMale { length = 20.0, friction = True })
                        (TechnicPinFemale { length = 20.0 })
                    )
        , test "Technic axle male meshes with female" <|
            \_ ->
                Expect.equal True
                    (isCompatible
                        (TechnicAxleMale { length = 40.0 })
                        (TechnicAxleFemale { length = 20.0 })
                    )
        , test "Clip meshes with Bar" <|
            \_ ->
                Expect.equal True
                    (isCompatible
                        (ClipMale { width = 4.0 })
                        (BarFemale { diameter = 4.0 })
                    )
        , test "Ball joint meshes with socket" <|
            \_ ->
                Expect.equal True (isCompatible BallMale BallSocketFemale)
        , test "Stud does not mesh with axle hole" <|
            \_ ->
                Expect.equal False
                    (isCompatible Stud (TechnicAxleFemale { length = 20.0 }))
        , test "Technic axle male meshes with Technic pin female" <|
            \_ ->
                Expect.equal True
                    (isCompatible
                        (TechnicAxleMale { length = 40.0 })
                        (TechnicPinFemale { length = 20.0 })
                    )
        , test "Technic pin female meshes with Technic axle male" <|
            \_ ->
                Expect.equal True
                    (isCompatible
                        (TechnicPinFemale { length = 20.0 })
                        (TechnicAxleMale { length = 40.0 })
                    )
        ]


extractionTests : Test
extractionTests =
    describe "Connection extraction"
        [ test "extracts stud.dat as Stud with normal (0, -1, 0)" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.makeTranslate3 10 0 20
                            , file = "stud.dat"
                            }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p ] ->
                        Expect.all
                            [ \pt -> Expect.equal Stud pt.connType
                            , \pt -> Expect.equal 10 (Vec3.getX pt.localPosition)
                            , \pt -> Expect.equal 0 (Vec3.getY pt.localPosition)
                            , \pt -> Expect.equal 20 (Vec3.getZ pt.localPosition)
                            , \pt -> Expect.equal -1 (Vec3.getY pt.normal)
                            ]
                            p

                    _ ->
                        Expect.fail ("Expected 1 connection point, got " ++ String.fromInt (List.length points))
        , test "recursively grouped standard bricks keep catalog anti-stud spacing" <|
            \_ ->
                let
                    assemblyLines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.makeTranslate3 0 0 -10
                            , file = "3004.dat"
                            }
                        , SubFileRef
                            { color = 16
                            , transform = Mat4.makeTranslate3 0 0 10
                            , file = "3004.dat"
                            }
                        ]

                    brickLines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.makeTranslate3 -10 0 0
                            , file = "stud.dat"
                            }
                        , SubFileRef
                            { color = 16
                            , transform = Mat4.makeTranslate3 10 0 0
                            , file = "stud.dat"
                            }
                        , SubFileRef
                            { color = 16
                            , transform = Mat4.mul (Mat4.makeTranslate3 0 20 0) (Mat4.makeScale3 1 -1 1)
                            , file = "stud3.dat"
                            }
                        ]

                    resolver file =
                        case file of
                            "assembly.ldr" ->
                                Just assemblyLines

                            "3004.dat" ->
                                Just brickLines

                            _ ->
                                Nothing

                    antiStudPositions =
                        extractPartConnectivity "assembly.ldr" resolver
                            |> .connections
                            |> List.filter (.connType >> (==) AntiStud)
                            |> List.map
                                (\point ->
                                    ( Vec3.getX point.localPosition
                                    , Vec3.getY point.localPosition
                                    , Vec3.getZ point.localPosition
                                    )
                                )
                            |> List.sort
                in
                Expect.equal
                    [ ( -10, 24, -10 )
                    , ( -10, 24, 10 )
                    , ( 10, 24, -10 )
                    , ( 10, 24, 10 )
                    ]
                    antiStudPositions
        , test "recursive catalog lookup keeps the full contact grid for a different standard brick" <|
            \_ ->
                let
                    assemblyLines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.identity
                            , file = "3001.dat"
                            }
                        ]

                    resolver name =
                        if name == "assembly.ldr" then
                            Just assemblyLines

                        else
                            Nothing

                    antiStudPositions =
                        extractPartConnectivity "assembly.ldr" resolver
                            |> .connections
                            |> List.filter (.connType >> (==) AntiStud)
                            |> List.map
                                (\point ->
                                    ( Vec3.getX point.localPosition
                                    , Vec3.getY point.localPosition
                                    , Vec3.getZ point.localPosition
                                    )
                                )
                            |> List.sort
                in
                Expect.equal
                    [ ( -30, 24, -10 )
                    , ( -30, 24, 10 )
                    , ( -10, 24, -10 )
                    , ( -10, 24, 10 )
                    , ( 10, 24, -10 )
                    , ( 10, 24, 10 )
                    , ( 30, 24, -10 )
                    , ( 30, 24, 10 )
                    ]
                    antiStudPositions
        , test "recursive catalog contacts transform with a rotated child" <|
            \_ ->
                let
                    rotation =
                        Mat4.makeRotate (pi / 2) (vec3 1 0 0)

                    resolver name =
                        if name == "rotated-assembly.ldr" then
                            Just
                                [ SubFileRef
                                    { color = 16
                                    , transform = rotation
                                    , file = "3004.dat"
                                    }
                                ]

                        else
                            Nothing

                    catalogAntiStuds =
                        knownCatalogPart "3004.dat"
                            |> Maybe.map (.connections >> List.filter (.connType >> (==) AntiStud))
                            |> Maybe.withDefault []

                    recursiveAntiStuds =
                        extractPartConnectivity "rotated-assembly.ldr" resolver
                            |> .connections
                            |> List.filter (.connType >> (==) AntiStud)

                    positionTuple point =
                        ( Vec3.getX point.localPosition
                        , Vec3.getY point.localPosition
                        , Vec3.getZ point.localPosition
                        )

                    normalTuple point =
                        ( Vec3.getX point.normal
                        , Vec3.getY point.normal
                        , Vec3.getZ point.normal
                        )

                    expectedPositions =
                        catalogAntiStuds
                            |> List.map (.localPosition >> Mat4.transform rotation >> (\v -> ( Vec3.getX v, Vec3.getY v, Vec3.getZ v )))
                            |> List.sort

                    actualPositions =
                        recursiveAntiStuds |> List.map positionTuple |> List.sort

                    expectedNormals =
                        catalogAntiStuds
                            |> List.map (.normal >> transformDirection rotation >> (\v -> ( Vec3.getX v, Vec3.getY v, Vec3.getZ v )))
                            |> List.sort

                    actualNormals =
                        recursiveAntiStuds |> List.map normalTuple |> List.sort
                in
                Expect.all
                    [ \_ -> Expect.equal expectedPositions actualPositions
                    , \_ -> Expect.equal expectedNormals actualNormals
                    ]
                    ()
        , test "extracts stud4.dat as AntiStud at its open end with normal (0, 1, 0)" <|
            \_ ->
                let
                    -- As parts place it: flipped (Y scale -1) with its base at Y = 20,
                    -- so the tube runs down to the underside at Y = 24.
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.mul (Mat4.makeTranslate3 0 20 0) (Mat4.makeScale3 1 -1 1)
                            , file = "stud4.dat"
                            }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p ] ->
                        Expect.all
                            [ \pt -> Expect.equal AntiStud pt.connType
                            , \pt -> Expect.equal 24 (Vec3.getY pt.localPosition)
                            , \pt -> Expect.equal 1 (Vec3.getY pt.normal)
                            ]
                            p

                    _ ->
                        Expect.fail ("Expected 1 connection point, got " ++ String.fromInt (List.length points))
        , test "extracts stud4od.dat as AntiStud at its open end with normal (0, 1, 0)" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.mul (Mat4.makeTranslate3 0 20 0) (Mat4.makeScale3 1 -1 1)
                            , file = "stud4od.dat"
                            }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p ] ->
                        Expect.all
                            [ \pt -> Expect.equal AntiStud pt.connType
                            , \pt -> Expect.equal 24 (Vec3.getY pt.localPosition)
                            , \pt -> Expect.equal 1 (Vec3.getY pt.normal)
                            ]
                            p

                    _ ->
                        Expect.fail ("Expected 1 connection point, got " ++ String.fromInt (List.length points))
        , test "extracts confric subfile as TechnicPinMale with friction = True" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.identity
                            , file = "confric5.dat"
                            }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p ] ->
                        Expect.equal (TechnicPinMale { length = 20.0, friction = True }) p.connType

                    _ ->
                        Expect.fail ("Expected 1 connection point, got " ++ String.fromInt (List.length points))
        , test "extracts connect.dat subfile as TechnicPinMale with friction = False" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.identity
                            , file = "connect.dat"
                            }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p ] ->
                        Expect.equal (TechnicPinMale { length = 20.0, friction = False }) p.connType

                    _ ->
                        Expect.fail ("Expected 1 connection point, got " ++ String.fromInt (List.length points))
        , test "extracts beamhole.dat and connhole.dat as TechnicPinFemale" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef { color = 16, transform = Mat4.identity, file = "beamhole.dat" }
                        , SubFileRef { color = 16, transform = Mat4.identity, file = "connhole.dat" }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p1, p2 ] ->
                        Expect.all
                            [ \_ -> Expect.equal (TechnicPinFemale { length = 20.0 }) p1.connType
                            , \_ -> Expect.equal (TechnicPinFemale { length = 20.0 }) p2.connType
                            ]
                            ()

                    _ ->
                        Expect.fail ("Expected 2 connection points, got " ++ String.fromInt (List.length points))
        , test "knownCatalogPart builds 3673 with frictionless pin connections" <|
            \_ ->
                case knownCatalogPart "3673.dat" of
                    Just conn ->
                        let
                            frictionlessPins =
                                List.filter (\c -> c.connType == TechnicPinMale { length = 20.0, friction = False }) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length conn.connections)
                            , \_ -> Expect.equal 2 (List.length frictionlessPins)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3673.dat was not found"
        , test "knownCatalogPart builds 6558 3L friction pin" <|
            \_ ->
                case knownCatalogPart "6558.dat" of
                    Just conn ->
                        let
                            frictionPins =
                                List.filter (\c -> c.connType == TechnicPinMale { length = 30.0, friction = True }) conn.connections

                            xPositions =
                                List.map (\c -> Vec3.getX c.localPosition) conn.connections
                                    |> List.sort
                        in
                        Expect.all
                            [ \_ -> Expect.equal 3 (List.length conn.connections)
                            , \_ -> Expect.equal 3 (List.length frictionPins)
                            , \_ -> Expect.equal [ -20.0, 0.0, 20.0 ] xPositions
                            , \_ ->
                                case conn.boundingBoxes of
                                    [ box ] ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 -30 -8 -8) box.min
                                            , \_ -> Expect.equal (vec3 30 8 8) box.max
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected 1 bounding box"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 6558.dat was not found"
        , test "knownCatalogPart builds 43093 with axle and pin ends" <|
            \_ ->
                case knownCatalogPart "43093.dat" of
                    Just conn ->
                        let
                            pins =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinMale { friction } ->
                                                friction == True

                                            _ ->
                                                False
                                    )
                                    conn.connections

                            axles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicAxleMale { length } ->
                                                length == 20.0

                                            _ ->
                                                False
                                    )
                                    conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length conn.connections)
                            , \_ -> Expect.equal 1 (List.length pins)
                            , \_ -> Expect.equal 1 (List.length axles)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 43093.dat was not found"
        , test "knownCatalogPart builds 4274 with pin and stud ends" <|
            \_ ->
                case knownCatalogPart "4274.dat" of
                    Just conn ->
                        let
                            pins =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinMale { length, friction } ->
                                                length == 10.0 && friction == True

                                            _ ->
                                                False
                                    )
                                    conn.connections

                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length conn.connections)
                            , \_ -> Expect.equal 1 (List.length pins)
                            , \_ -> Expect.equal 1 (List.length studs)
                            , \_ -> Expect.equal (Just (vec3 -5 0 0)) (List.head pins |> Maybe.map .localPosition)
                            , \_ -> Expect.equal [ { min = vec3 -10 -8 -8, max = vec3 4 8 8 } ] conn.boundingBoxes
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 4274.dat was not found"
        , test "knownCatalogPart builds 89678 with friction pin and stud ends" <|
            \_ ->
                case knownCatalogPart "89678.dat" of
                    Just conn ->
                        let
                            pins =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinMale { length, friction } ->
                                                length == 10.0 && friction == True

                                            _ ->
                                                False
                                    )
                                    conn.connections

                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length conn.connections)
                            , \_ -> Expect.equal 1 (List.length pins)
                            , \_ -> Expect.equal 1 (List.length studs)
                            , \_ -> Expect.equal (Just (vec3 -5 0 0)) (List.head pins |> Maybe.map .localPosition)
                            , \_ -> Expect.equal [ { min = vec3 -10 -8 -8, max = vec3 4 8 8 } ] conn.boundingBoxes
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 89678.dat was not found"
        , test "knownCatalogPart builds 6587 Axle 3 with Stud" <|
            \_ ->
                case knownCatalogPart "6587.dat" of
                    Just conn ->
                        let
                            axles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicAxleMale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    conn.connections

                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            stud =
                                List.head studs
                        in
                        Expect.all
                            [ \_ -> Expect.equal 4 (List.length conn.connections)
                            , \_ -> Expect.equal 3 (List.length axles)
                            , \_ -> Expect.equal 1 (List.length studs)
                            , \_ ->
                                case stud of
                                    Just s ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 30 0 0) s.localPosition
                                            , \_ -> Expect.equal (vec3 1 0 0) s.normal
                                            , \_ -> Expect.equal (vec3 0 1 0) s.tangent
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail "Expected stud on 6587"
                            , \_ ->
                                case conn.boundingBoxes of
                                    [ box ] ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 -30 -4 -4) box.min
                                            , \_ -> Expect.equal (vec3 34 4 4) box.max
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected 1 bounding box for 6587"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 6587.dat was not found"
        , test "knownCatalogPart builds 6541 Technic Brick 1x1 with hole" <|
            \_ ->
                case knownCatalogPart "6541.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections

                            pinHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 5 (List.length conn.connections)
                            , \_ -> Expect.equal 1 (List.length studs)
                            , \_ -> Expect.equal 1 (List.length antiStuds)
                            , \_ -> Expect.equal 3 (List.length pinHoles)
                            , \_ ->
                                case conn.boundingBoxes of
                                    [ box ] ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 -10 -4 -10) box.min
                                            , \_ -> Expect.equal (vec3 10 24 10) box.max
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected 1 bounding box for 6541"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 6541.dat was not found"
        , test "knownCatalogPart builds 3894 Technic Brick 1x6 with 6 studs and 5 holes" <|
            \_ ->
                case knownCatalogPart "3894.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections

                            pinHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 6 (List.length studs)
                            , \_ -> Expect.equal 6 (List.length antiStuds)
                            , \_ -> Expect.equal 15 (List.length pinHoles)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3894.dat was not found"
        , test "knownCatalogPart builds 3709b Technic Plate 2x4 with holes" <|
            \_ ->
                case knownCatalogPart "3709b.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections

                            pinHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 8 (List.length studs)
                            , \_ -> Expect.equal 8 (List.length antiStuds)
                            , \_ -> Expect.equal 9 (List.length pinHoles)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3709b.dat was not found"
        , test "knownCatalogPart builds 2711 Rotor 2 Blade with 2 studs and axle hole" <|
            \_ ->
                case knownCatalogPart "2711.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections

                            axleHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicAxleFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length studs)
                            , \_ -> Expect.equal 2 (List.length antiStuds)
                            , \_ -> Expect.equal 3 (List.length axleHoles)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 2711.dat was not found"
        , test "knownCatalogPart builds 32556 and 32556a 3L frictionless pins" <|
            \_ ->
                case ( knownCatalogPart "32556.dat", knownCatalogPart "32556a.dat" ) of
                    ( Just conn1, Just conn2 ) ->
                        let
                            frictionlessPins c =
                                List.filter (\p -> p.connType == TechnicPinMale { length = 30.0, friction = False }) c.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 3 (List.length conn1.connections)
                            , \_ -> Expect.equal 3 (List.length (frictionlessPins conn1))
                            , \_ -> Expect.equal 3 (List.length conn2.connections)
                            , \_ -> Expect.equal 3 (List.length (frictionlessPins conn2))
                            ]
                            ()

                    _ ->
                        Expect.fail "Catalog parts 32556.dat or 32556a.dat not found"
        , test "knownCatalogPart builds Brick 2x4 with 8 studs and 8 anti-studs" <|
            \_ ->
                case knownCatalogPart "3001.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 8 (List.length studs)
                            , \_ -> Expect.equal 8 (List.length antiStuds)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3001.dat was not found"
        , test "knownCatalogPart builds Brick 1x4 with 4 studs and 4 anti-studs" <|
            \_ ->
                case knownCatalogPart "3010.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 4 (List.length studs)
                            , \_ -> Expect.equal 4 (List.length antiStuds)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3010.dat was not found"
        , test "knownCatalogPart builds Plate 2x8 with 16 studs and 16 anti-studs" <|
            \_ ->
                case knownCatalogPart "3034.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 16 (List.length studs)
                            , \_ -> Expect.equal 16 (List.length antiStuds)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3034.dat was not found"
        , test "knownCatalogPart builds Plate 2x6 (3795) with 12 studs and 12 anti-studs" <|
            \_ ->
                case knownCatalogPart "3795.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 12 (List.length studs)
                            , \_ -> Expect.equal 12 (List.length antiStuds)
                            , \_ ->
                                case conn.boundingBoxes of
                                    [ box ] ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 -60 -4 -20) box.min
                                            , \_ -> Expect.equal (vec3 60 8 20) box.max
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected 1 bounding box for Plate 2x6"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3795.dat was not found"
        , test "knownCatalogPart builds Plate 4x4 (3031) with 16 studs and 16 anti-studs" <|
            \_ ->
                case knownCatalogPart "3031.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 16 (List.length studs)
                            , \_ -> Expect.equal 16 (List.length antiStuds)
                            , \_ ->
                                case conn.boundingBoxes of
                                    [ box ] ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 -40 -4 -40) box.min
                                            , \_ -> Expect.equal (vec3 40 8 40) box.max
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected 1 bounding box for Plate 4x4"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3031.dat was not found"
        , test "knownCatalogPart builds Plate 6x6 (3958) with 36 studs and 36 anti-studs" <|
            \_ ->
                case knownCatalogPart "3958.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 36 (List.length studs)
                            , \_ -> Expect.equal 36 (List.length antiStuds)
                            , \_ ->
                                case conn.boundingBoxes of
                                    [ box ] ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 -60 -4 -60) box.min
                                            , \_ -> Expect.equal (vec3 60 8 60) box.max
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected 1 bounding box for Plate 6x6"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3958.dat was not found"
        , test "knownCatalogPart builds Plate 8x8 (41539) with 64 studs and 64 anti-studs" <|
            \_ ->
                case knownCatalogPart "41539.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 64 (List.length studs)
                            , \_ -> Expect.equal 64 (List.length antiStuds)
                            , \_ ->
                                case conn.boundingBoxes of
                                    [ box ] ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 -80 -4 -80) box.min
                                            , \_ -> Expect.equal (vec3 80 8 80) box.max
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected 1 bounding box for Plate 8x8"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 41539.dat was not found"
        , test "knownCatalogPart builds round plates (4073, 6141, 85861) and round brick (3062b)" <|
            \_ ->
                let
                    verifyRoundPart partName expectedHeight =
                        case knownCatalogPart partName of
                            Just conn ->
                                let
                                    studs =
                                        List.filter (\c -> c.connType == Stud) conn.connections

                                    antiStuds =
                                        List.filter (\c -> c.connType == AntiStud) conn.connections
                                in
                                Expect.all
                                    [ \_ -> Expect.equal 1 (List.length studs)
                                    , \_ -> Expect.equal 1 (List.length antiStuds)
                                    , \_ ->
                                        case conn.boundingBoxes of
                                            [ box ] ->
                                                Expect.all
                                                    [ \_ -> Expect.equal (vec3 -10 -4 -10) box.min
                                                    , \_ -> Expect.equal (vec3 10 expectedHeight 10) box.max
                                                    ]
                                                    ()

                                            _ ->
                                                Expect.fail ("Expected 1 bounding box for " ++ partName)
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail ("Catalog part " ++ partName ++ " was not found")
                in
                Expect.all
                    [ \_ -> verifyRoundPart "4073.dat" 8
                    , \_ -> verifyRoundPart "6141.dat" 8
                    , \_ -> verifyRoundPart "85861.dat" 8
                    , \_ -> verifyRoundPart "3062.dat" 24
                    , \_ -> verifyRoundPart "3062a.dat" 24
                    , \_ -> verifyRoundPart "3062b.dat" 24
                    ]
                    ()
        , test "makeAxle produces connection points at stud intervals and correct bounds" <|
            \_ ->
                let
                    axle4 =
                        makeAxle 4 "3705.dat"

                    xCoords =
                        List.map (\c -> Vec3.getX c.localPosition) axle4.connections
                in
                Expect.all
                    [ \_ -> Expect.equal 4 (List.length axle4.connections)
                    , \_ -> Expect.equal [ -30.0, -10.0, 10.0, 30.0 ] xCoords
                    , \_ ->
                        case axle4.boundingBoxes of
                            [ box ] ->
                                Expect.all
                                    [ \_ -> Expect.equal (vec3 -40 -4 -4) box.min
                                    , \_ -> Expect.equal (vec3 40 4 4) box.max
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected 1 bounding box for axle"
                    ]
                    ()
        , test "makeGear produces central axle hole and bounds" <|
            \_ ->
                let
                    gear8 =
                        makeGear 10.0 20.0 "3647.dat"

                    gear16 =
                        makeGear 20.0 20.0 "4019.dat"
                in
                Expect.all
                    [ \_ -> Expect.equal 3 (List.length gear8.connections)
                    , \_ ->
                        let
                            center =
                                List.filter (\c -> c.id == 1) gear8.connections
                        in
                        case center of
                            [ c ] ->
                                Expect.all
                                    [ \_ -> Expect.equal (TechnicAxleFemale { length = 20.0 }) c.connType
                                    , \_ -> Expect.equal (vec3 0 0 0) c.localPosition
                                    , \_ -> Expect.equal (vec3 0 0 1) c.normal
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected center connection point for gear"
                    , \_ ->
                        Expect.equal
                            [ { min = vec3 -8.0 -8.0 -10.0, max = vec3 8.0 8.0 10.0 }
                            , { min = vec3 -10.0 -10.0 -5.0, max = vec3 10.0 10.0 5.0 }
                            ]
                            gear8.boundingBoxes
                    , \_ ->
                        Expect.equal
                            [ { min = vec3 -8.0 -8.0 -10.0, max = vec3 8.0 8.0 10.0 }
                            , { min = vec3 -20.0 -20.0 -5.0, max = vec3 20.0 20.0 5.0 }
                            ]
                            gear16.boundingBoxes
                    ]
                    ()
        , test "40T gear (3649) includes axle holes and 12 pin holes" <|
            \_ ->
                case knownCatalogPart "3649.dat" of
                    Just gear40 ->
                        let
                            axleHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicAxleFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    gear40.connections

                            pinHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    gear40.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 51 (List.length gear40.connections)
                            , \_ -> Expect.equal 15 (List.length axleHoles)
                            , \_ -> Expect.equal 36 (List.length pinHoles)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3649.dat was not found"
        , test "24T gear (3648b) includes central axle hole and 4 pin holes" <|
            \_ ->
                case knownCatalogPart "3648b.dat" of
                    Just gear24 ->
                        let
                            axleHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicAxleFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    gear24.connections

                            pinHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    gear24.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 15 (List.length gear24.connections)
                            , \_ -> Expect.equal 3 (List.length axleHoles)
                            , \_ -> Expect.equal 12 (List.length pinHoles)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3648b.dat was not found"
        , test "knownCatalogPart supports classic and modern gears, axles, beams, and bushings" <|
            \_ ->
                let
                    partsToCheck =
                        [ "3647"
                        , "10928"
                        , "11955"
                        , "4019"
                        , "3648"
                        , "3648b"
                        , "3649"
                        , "32198"
                        , "32198a"
                        , "32269"
                        , "32270"
                        , "6589"
                        , "4143"
                        , "3650"
                        , "3650b"
                        , "3650c"
                        , "18938"
                        , "4716"
                        , "32905"
                        , "3704"
                        , "4519"
                        , "3705"
                        , "32073"
                        , "3706"
                        , "44294"
                        , "3707"
                        , "60485"
                        , "3737"
                        , "3708"
                        , "32062"
                        , "6587"
                        , "3700"
                        , "3701"
                        , "3702"
                        , "3703"
                        , "32523"
                        , "32316"
                        , "32524"
                        , "32449"
                        , "32525"
                        , "32009"
                        , "3713"
                        , "4265c"
                        , "3894"
                        , "6541"
                        , "3709b"
                        , "2711"
                        , "32125"
                        , "89678"
                        , "3749"
                        , "11214"
                        , "69778"
                        , "94925"
                        , "32498"
                        , "32072"
                        , "99009"
                        , "99010"
                        ]

                    missing =
                        List.filter (\p -> knownCatalogPart p == Nothing) partsToCheck
                in
                Expect.equal [] missing
        , test "thin single bevels (6589, 4143) list the hub box first over their real z span, axle connectors at ±10" <|
            \_ ->
                let
                    summary part =
                        knownCatalogPart part
                            |> Maybe.map
                                (\conn ->
                                    ( List.map (\b -> [ Vec3.getX b.max, Vec3.getZ b.min, Vec3.getZ b.max ]) conn.boundingBoxes
                                    , List.map (.localPosition >> Vec3.getZ) conn.connections
                                    )
                                )
                in
                Expect.equal
                    [ Just ( [ [ 8, -3, 7 ], [ 15, 0, 7 ] ], [ 0, 10, -10 ] )
                    , Just ( [ [ 8, -4, 3 ], [ 17.5, -4, 0 ] ], [ 0, 10, -10 ] )
                    ]
                    [ summary "6589.dat", summary "4143.dat" ]
        , test "turntable pin holes retain opposite-face connectors" <|
            \_ ->
                case ( knownCatalogPart "18938.dat", knownCatalogPart "99009.dat", knownCatalogPart "99010.dat" ) of
                    ( Just turntable60, Just base28, Just top28 ) ->
                        let
                            connectorCounts part =
                                ( List.filter (\c -> c.connType == TechnicPinFemale { length = 20 }) part.connections |> List.length
                                , List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicAxleFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    part.connections
                                    |> List.length
                                )

                            pinNormalZ part =
                                part.connections
                                    |> List.filter (\c -> c.connType == TechnicPinFemale { length = 20 })
                                    |> List.map (\c -> Vec3.getZ c.normal)
                                    |> List.sort

                            pinPositions part =
                                part.connections
                                    |> List.filter (\c -> c.connType == TechnicPinFemale { length = 20 })
                                    |> List.map (\c -> ( Vec3.getX c.localPosition, Vec3.getY c.localPosition, Vec3.getZ c.localPosition ))
                                    |> List.sort
                        in
                        Expect.all
                            [ \_ -> Expect.equal [ ( 6, 0 ), ( 6, 0 ), ( 6, 0 ) ] [ connectorCounts turntable60, connectorCounts base28, connectorCounts top28 ]
                            , \_ -> Expect.equal [ -1, -1, 1, 1, 1, 1 ] (pinNormalZ base28)
                            , \_ -> Expect.equal [ -1, -1, 1, 1, 1, 1 ] (pinNormalZ top28)
                            , \_ -> Expect.equal [ ( -20, -20, -10 ), ( -20, -20, 0 ), ( -20, -20, 10 ), ( 20, -20, -10 ), ( 20, -20, 0 ), ( 20, -20, 10 ) ] (pinPositions top28)
                            ]
                            ()

                    _ ->
                        Expect.fail "A catalog turntable gear part was not found"
        , test "87087 has 1 up stud, 1 side stud facing -Z, and 1 anti-stud; 3070b has 1 anti-stud and no studs" <|
            \_ ->
                case ( knownCatalogPart "87087.dat", knownCatalogPart "3070b.dat" ) of
                    ( Just snot87087, Just tile1x1 ) ->
                        let
                            upStuds =
                                List.filter (\c -> c.connType == Stud && Vec3.getY c.normal < -0.7) snot87087.connections

                            sideStuds =
                                List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7) snot87087.connections

                            antiStuds87087 =
                                List.filter (\c -> c.connType == AntiStud) snot87087.connections

                            tileStuds =
                                List.filter (\c -> c.connType == Stud) tile1x1.connections

                            tileAntiStuds =
                                List.filter (\c -> c.connType == AntiStud) tile1x1.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 1 (List.length upStuds)
                            , \_ -> Expect.equal 1 (List.length sideStuds)
                            , \_ -> Expect.equal 1 (List.length antiStuds87087)
                            , \_ -> Expect.equal 0 (List.length tileStuds)
                            , \_ -> Expect.equal 1 (List.length tileAntiStuds)
                            ]
                            ()

                    _ ->
                        Expect.fail "87087.dat or 3070b.dat not found in catalog"
        , test "4733 (Travis brick) has 1 top stud, 4 side studs facing -Z, +X, +Z, -X, and 1 bottom anti-stud" <|
            \_ ->
                case knownCatalogPart "4733.dat" of
                    Just travis ->
                        let
                            upStuds =
                                List.filter (\c -> c.connType == Stud && Vec3.getY c.normal < -0.7) travis.connections

                            sideStuds =
                                List.filter (\c -> c.connType == Stud && abs (Vec3.getY c.normal) < 0.3) travis.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) travis.connections

                            frontSide =
                                List.filter (\c -> Vec3.getZ c.normal < -0.7) sideStuds

                            rightSide =
                                List.filter (\c -> Vec3.getX c.normal > 0.7) sideStuds

                            backSide =
                                List.filter (\c -> Vec3.getZ c.normal > 0.7) sideStuds

                            leftSide =
                                List.filter (\c -> Vec3.getX c.normal < -0.7) sideStuds
                        in
                        Expect.all
                            [ \_ -> Expect.equal 1 (List.length upStuds)
                            , \_ -> Expect.equal 4 (List.length sideStuds)
                            , \_ -> Expect.equal 1 (List.length antiStuds)
                            , \_ -> Expect.equal 1 (List.length frontSide)
                            , \_ -> Expect.equal 1 (List.length rightSide)
                            , \_ -> Expect.equal 1 (List.length backSide)
                            , \_ -> Expect.equal 1 (List.length leftSide)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "4733.dat not found in catalog"
        , test "4070 (Headlight brick) has 1 top stud, 1 bottom anti-stud, 1 recessed front side stud at Z = -6, and 1 rear anti-stud at Z = 10" <|
            \_ ->
                case knownCatalogPart "4070.dat" of
                    Just headlight ->
                        let
                            upStuds =
                                List.filter (\c -> c.connType == Stud && Vec3.getY c.normal < -0.7) headlight.connections

                            bottomAntiStuds =
                                List.filter (\c -> c.connType == AntiStud && Vec3.getY c.normal > 0.7) headlight.connections

                            frontRecessedStud =
                                List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7) headlight.connections

                            rearAntiStud =
                                List.filter (\c -> c.connType == AntiStud && Vec3.getZ c.normal > 0.7) headlight.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 1 (List.length upStuds)
                            , \_ -> Expect.equal 1 (List.length bottomAntiStuds)
                            , \_ -> Expect.equal 1 (List.length frontRecessedStud)
                            , \_ ->
                                case frontRecessedStud of
                                    [ s ] ->
                                        Expect.equal -6 (round (Vec3.getZ s.localPosition))

                                    _ ->
                                        Expect.fail "Expected exactly 1 front recessed stud"
                            , \_ -> Expect.equal 1 (List.length rearAntiStud)
                            , \_ ->
                                case rearAntiStud of
                                    [ a ] ->
                                        Expect.equal 10 (round (Vec3.getZ a.localPosition))

                                    _ ->
                                        Expect.fail "Expected exactly 1 rear anti-stud"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "4070.dat not found in catalog"
        , test "catalog slope parts have expected stud counts, anti-stud counts, and bounding boxes" <|
            \_ ->
                let
                    checkSlope part expectedStuds expectedAntiStuds expectedMin expectedMax =
                        case knownCatalogPart part of
                            Just conn ->
                                let
                                    studCount =
                                        List.length (List.filter (\c -> c.connType == Stud) conn.connections)

                                    antiCount =
                                        List.length (List.filter (\c -> c.connType == AntiStud) conn.connections)

                                    box =
                                        List.head conn.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 0 0 0, max = vec3 0 0 0 }
                                in
                                Expect.all
                                    [ \_ -> Expect.equal expectedStuds studCount
                                    , \_ -> Expect.equal expectedAntiStuds antiCount
                                    , \_ -> Expect.equal (Vec3.toRecord expectedMin) (Vec3.toRecord box.min)
                                    , \_ -> Expect.equal (Vec3.toRecord expectedMax) (Vec3.toRecord box.max)
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail ("Catalog slope part not found: " ++ part)
                in
                Expect.all
                    [ \_ -> checkSlope "3040b.dat" 1 2 (vec3 -10 -4 -30) (vec3 10 24 10)
                    , \_ -> checkSlope "3039.dat" 2 4 (vec3 -20 -4 -30) (vec3 20 24 10)
                    , \_ -> checkSlope "3038.dat" 3 6 (vec3 -30 -4 -30) (vec3 30 24 10)
                    , \_ -> checkSlope "3037.dat" 4 8 (vec3 -40 -4 -30) (vec3 40 24 10)
                    , \_ -> checkSlope "3298.dat" 2 6 (vec3 -20 -4 -50) (vec3 20 24 10)
                    , \_ -> checkSlope "3665b.dat" 2 1 (vec3 -10 -4 -30) (vec3 10 24 10)
                    , \_ -> checkSlope "3660b.dat" 4 2 (vec3 -20 -4 -30) (vec3 20 24 10)
                    , \_ -> checkSlope "3747b.dat" 6 2 (vec3 -20 -4 -50) (vec3 20 24 10)
                    ]
                    ()
        , test "catalog bracket parts 36840, 36841, and 44728 have expected connection counts and bounding boxes" <|
            \_ ->
                let
                    checkBracket part expectedStuds expectedAntiStuds expectedSideStuds expectedMin expectedMax =
                        case knownCatalogPart part of
                            Just conn ->
                                let
                                    upStuds =
                                        List.filter (\c -> c.connType == Stud && Vec3.getY c.normal < -0.7) conn.connections

                                    sideStuds =
                                        List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7) conn.connections

                                    antiStuds =
                                        List.filter (\c -> c.connType == AntiStud) conn.connections

                                    box =
                                        List.head conn.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 0 0 0, max = vec3 0 0 0 }
                                in
                                Expect.all
                                    [ \_ -> Expect.equal expectedStuds (List.length upStuds)
                                    , \_ -> Expect.equal expectedSideStuds (List.length sideStuds)
                                    , \_ -> Expect.equal expectedAntiStuds (List.length antiStuds)
                                    , \_ -> Expect.equal (Vec3.toRecord expectedMin) (Vec3.toRecord box.min)
                                    , \_ -> Expect.equal (Vec3.toRecord expectedMax) (Vec3.toRecord box.max)
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail ("Catalog bracket part not found: " ++ part)
                in
                Expect.all
                    [ \_ -> checkBracket "36840.dat" 1 1 1 (vec3 -10 -12 -14) (vec3 10 8 10)
                    , \_ -> checkBracket "36841.dat" 1 1 1 (vec3 -10 -4 -14) (vec3 10 20 10)
                    , \_ -> checkBracket "44728.dat" 2 2 4 (vec3 -20 -4 -14) (vec3 20 40 10)
                    ]
                    ()
        ]


studLatticeTests : Test
studLatticeTests =
    let
        noLines _ =
            Nothing

        offsetOf name matrix =
            studOffset (Connectivity.extractPartConnectivity name noLines) matrix

        turnedY =
            Mat4.makeRotate (pi / 2) (vec3 0 1 0)

        rot0 =
            Mat4.identity

        rot90 =
            turnedY

        rot180 =
            Mat4.makeRotate pi (vec3 0 1 0)

        rot270 =
            Mat4.makeRotate (3 * pi / 2) (vec3 0 1 0)
    in
    describe "stud offset and the 60592 window"
        [ test "a 1x1 (odd on both axes) is offset 10 on both" <|
            \_ -> offsetOf "3005.dat" Mat4.identity |> Expect.equal { x = 10, z = 10 }
        , test "a 2x4 (even on both axes) is not offset" <|
            \_ -> offsetOf "3001.dat" Mat4.identity |> Expect.equal { x = 0, z = 0 }
        , test "a 1x3 (odd on both axes) is offset 10 on both" <|
            \_ -> offsetOf "3622.dat" Mat4.identity |> Expect.equal { x = 10, z = 10 }
        , test "a 1x2 has its odd axis along Z, and swaps it when turned 90°" <|
            \_ ->
                Expect.all
                    [ \_ -> offsetOf "3004.dat" Mat4.identity |> Expect.equal { x = 0, z = 10 }
                    , \_ -> offsetOf "3004.dat" turnedY |> Expect.equal { x = 10, z = 0 }
                    ]
                    ()
        , test "only the rotation counts: a translated part keeps its offset" <|
            \_ ->
                offsetOf "3004.dat" (Mat4.mul (Mat4.makeTranslate3 30 -24 7) turnedY)
                    |> Expect.equal { x = 10, z = 0 }
        , test "the window 60592 (2 x 1 studs) is offset 0 on X and 10 on Z" <|
            \_ -> offsetOf "60592.dat" Mat4.identity |> Expect.equal { x = 0, z = 10 }
        , test "the SNOT brick 87087 with side stud counts only its up-facing stud: { x = 10, z = 10 }" <|
            \_ -> offsetOf "87087.dat" Mat4.identity |> Expect.equal { x = 10, z = 10 }
        , test "Technic parts (no stud points) are never offset" <|
            \_ ->
                [ "32524.dat", "3705.dat", "3648.dat", "3647.dat", "6590.dat" ]
                    |> List.map (\name -> offsetOf name turnedY)
                    |> Expect.equal (List.repeat 5 { x = 0, z = 0 })
        , test "isStudded is true for bricks and false for Technic parts" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal True (isStudded (Connectivity.extractPartConnectivity "3001.dat" noLines))
                    , \_ -> Expect.equal False (isStudded (Connectivity.extractPartConnectivity "32524.dat" noLines))
                    , \_ -> Expect.equal False (isStudded (Connectivity.extractPartConnectivity "3648.dat" noLines))
                    ]
                    ()
        , test "60592 has an underside AntiStud at each of its two 2x1 footprint studs" <|
            \_ ->
                case knownCatalogPart "60592.dat" of
                    Just conn ->
                        let
                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length antiStuds)
                            , \_ -> Expect.equal [ 48, 48 ] (List.map (\c -> Vec3.getY c.localPosition) antiStuds)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 60592.dat was not found"
        , test "studOffset across rotations (0°, 90°, 180°, 270°) for 1x1, 2x4, 1x2, 2x3, 2x6, 4x4" <|
            \_ ->
                Expect.all
                    [ -- 1x1 (odd on both axes): { x = 10, z = 10 } at all 4 rotations
                      \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3005.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3005.dat" rot90)
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3005.dat" rot180)
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3005.dat" rot270)
                    , -- 2x4 (even on both axes): { x = 0, z = 0 } at all 4 rotations
                      \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3001.dat" rot0)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3001.dat" rot90)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3001.dat" rot180)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3001.dat" rot270)
                    , -- 1x2 (even X, odd Z): swaps offset when turned 90° or 270°
                      \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3004.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3004.dat" rot90)
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3004.dat" rot180)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3004.dat" rot270)
                    , -- 2x3 (odd X, even Z): swaps offset when turned 90° or 270°
                      \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3002.dat" rot0)
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3002.dat" rot90)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3002.dat" rot180)
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3002.dat" rot270)
                    , -- 2x6 (even on both axes): { x = 0, z = 0 } at all 4 rotations
                      \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3795.dat" rot0)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3795.dat" rot90)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3795.dat" rot180)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3795.dat" rot270)
                    , -- 4x4 (even on both axes): { x = 0, z = 0 } at all 4 rotations
                      \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3031.dat" rot0)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3031.dat" rot90)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3031.dat" rot180)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3031.dat" rot270)
                    ]
                    ()
        , test "studOffset for tiles (3070b, 3069b) and slopes (3040b, 3039) across 0° and 90° rotations" <|
            \_ ->
                Expect.all
                    [ -- 1x1 tile 3070b: odd on both axes, offset { x = 10, z = 10 } at rot0 and rot90
                      \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3070b.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3070b.dat" rot90)

                    -- 2x1 tile 3069b: 2 on X, 1 on Z at rot0, swaps at rot90
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3069b.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3069b.dat" rot90)

                    -- 1x2 slope 3040b: single stud at origin, offset { x = 10, z = 10 }
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3040b.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3040b.dat" rot90)

                    -- 2x2 slope 3039: 2 studs along X, 1 column along Z at rot0, swaps at rot90
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3039.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3039.dat" rot90)
                    ]
                    ()
        , test "studOffset for 1x2 jumper plates (3794b, 15573) and 2x2 jumper plate (87580) across rotations" <|
            \_ ->
                Expect.all
                    [ -- 1x2 jumper plate 3794b: even on X, odd on Z at rot0, swaps at rot90
                      \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3794b.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3794b.dat" rot90)
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3794b.dat" rot180)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3794b.dat" rot270)
                    , -- 1x2 jumper plate 15573
                      \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "15573.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "15573.dat" rot90)
                    , -- 2x2 jumper plate 87580: even on both axes, { x = 0, z = 0 } at all rotations
                      \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "87580.dat" rot0)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "87580.dat" rot90)
                    ]
                    ()
        , test "studOffset for 1x1 tiles (98138, 25269, 24246, 39739) is { x = 10, z = 10 } across rotations" <|
            \_ ->
                let
                    checkPart part =
                        [ \_ -> Expect.equal { x = 10, z = 10 } (offsetOf part rot0)
                        , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf part rot90)
                        , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf part rot180)
                        , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf part rot270)
                        ]
                in
                Expect.all
                    (List.concatMap checkPart [ "98138.dat", "25269.dat", "24246.dat", "39739.dat" ])
                    ()
        , test "studOffset for 1x2 grille tile 2412b is { x = 0, z = 10 } at rot0 and { x = 10, z = 0 } at rot90" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "2412b.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "2412b.dat" rot90)
                    ]
                    ()
        , test "snaps 1x2 grille tile 2412b and 1x1 round quarter tile 25269 onto 6x6 plate 3958" <|
            \_ ->
                let
                    plate6x6 =
                        Connectivity.extractPartConnectivity "3958.dat" noLines

                    targetPoints =
                        List.map
                            (\p ->
                                { placementId = 0
                                , pointId = p.id
                                , connType = p.connType
                                , worldPosition = p.localPosition
                                , worldNormal = p.normal
                                , worldTangent = p.tangent
                                }
                            )
                            plate6x6.connections

                    tile1x2 =
                        Connectivity.extractPartConnectivity "2412b.dat" noLines

                    tile1x1 =
                        Connectivity.extractPartConnectivity "25269.dat" noLines

                    initialMatrix1x2 =
                        Mat4.makeTranslate3 2 -8 12

                    candidates1x2 =
                        solveSnapCandidates 25.0 0 initialMatrix1x2 tile1x2.connections targetPoints

                    initialMatrix1x1 =
                        Mat4.makeTranslate3 12 -8 12

                    candidates1x1 =
                        solveSnapCandidates 25.0 0 initialMatrix1x1 tile1x1.connections targetPoints
                in
                case ( candidates1x2, candidates1x1 ) of
                    ( first1x2 :: _, first1x1 :: _ ) ->
                        let
                            origin1x2 =
                                Mat4.transform first1x2.snapMatrix (vec3 0 0 0)

                            origin1x1 =
                                Mat4.transform first1x1.snapMatrix (vec3 0 0 0)
                        in
                        Expect.all
                            [ \_ -> Expect.equal 0 (round (Vec3.getX origin1x2))
                            , \_ -> Expect.equal -8 (round (Vec3.getY origin1x2))
                            , \_ -> Expect.equal 10 (round (Vec3.getZ origin1x2))
                            , \_ -> Expect.equal 10 (round (Vec3.getX origin1x1))
                            , \_ -> Expect.equal -8 (round (Vec3.getY origin1x1))
                            , \_ -> Expect.equal 10 (round (Vec3.getZ origin1x1))
                            ]
                            ()

                    ( [], _ ) ->
                        Expect.fail "expected snap candidate for 2412b.dat onto 3958.dat"

                    ( _, [] ) ->
                        Expect.fail "expected snap candidate for 25269.dat onto 3958.dat"
        , test "isUpright reports True for yaw rotations and False for tipped/inverted parts" <|
            \_ ->
                let
                    conn2x4 =
                        Connectivity.extractPartConnectivity "3001.dat" noLines

                    conn1x1 =
                        Connectivity.extractPartConnectivity "3024.dat" noLines

                    pitch90 =
                        Mat4.makeRotate (pi / 2) (vec3 1 0 0)

                    roll90 =
                        Mat4.makeRotate (pi / 2) (vec3 0 0 1)

                    upsideDown =
                        Mat4.makeRotate pi (vec3 1 0 0)
                in
                Expect.all
                    [ \_ -> Expect.equal True (isUpright conn2x4 rot0)
                    , \_ -> Expect.equal True (isUpright conn2x4 rot90)
                    , \_ -> Expect.equal True (isUpright conn2x4 rot180)
                    , \_ -> Expect.equal True (isUpright conn2x4 rot270)
                    , \_ -> Expect.equal True (isUpright conn1x1 rot0)
                    , \_ -> Expect.equal True (isUpright conn1x1 rot90)
                    , \_ -> Expect.equal False (isUpright conn2x4 pitch90)
                    , \_ -> Expect.equal False (isUpright conn2x4 roll90)
                    , \_ -> Expect.equal False (isUpright conn2x4 upsideDown)
                    , \_ -> Expect.equal False (isUpright conn1x1 pitch90)
                    , \_ -> Expect.equal False (isUpright conn1x1 upsideDown)
                    ]
                    ()
        ]


snapSolverTests : Test
snapSolverTests =
    describe "solveSnap"
        [ test "snaps dragged anti-stud onto target stud within 12 LDU" <|
            \_ ->
                let
                    -- Bottom brick at (0, 0, 0) with a top stud at (0, 0, 0) pointing (0, -1, 0)
                    targetStud =
                        { placementId = 1
                        , pointId = 1
                        , connType = Stud
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 -1 0
                        , worldTangent = vec3 1 0 0
                        }

                    -- Dragged brick with bottom anti-stud at local position (0, 24, 0), normal (0, 1, 0)
                    draggedAntiStud =
                        { id = 101
                        , connType = AntiStud
                        , localPosition = vec3 0 24 0
                        , normal = vec3 0 1 0
                        , tangent = vec3 1 0 0
                        }

                    -- Currently hovering at (2, -26, 3) (distance ≈ 3.6 LDU from (0, -24, 0))
                    currentHoverMat =
                        Mat4.makeTranslate3 2 -26 3
                in
                case solveSnap 12.0 0 currentHoverMat [ draggedAntiStud ] [ targetStud ] of
                    Just snap ->
                        let
                            -- Verify snapped anti-stud lands exactly on target stud at (0, 0, 0)
                            snappedAntiStudPos =
                                Mat4.transform snap.snapMatrix draggedAntiStud.localPosition
                        in
                        Expect.all
                            [ \_ -> Expect.lessThan 0.001 (Vec3.distance (vec3 0 0 0) snappedAntiStudPos)
                            , \_ -> Expect.lessThan 0.001 (Vec3.distance (vec3 0 -24 0) (Mat4.transform snap.snapMatrix (vec3 0 0 0)))
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Snap candidate was not found"
        , test "soft contact weighting selects edge alignment when hovering directly at edge, but prefers center when equidistant" <|
            \_ ->
                case knownCatalogPart "3003.dat" of
                    Just brick ->
                        let
                            draggedAntiStuds =
                                brick.connections
                                    |> List.filter (.connType >> (==) AntiStud)

                            targetStuds =
                                brick.connections
                                    |> List.filter (.connType >> (==) Stud)
                                    |> List.map (transformPoint (Mat4.makeTranslate3 0 -24 0) 1)

                            -- Hovering directly over the 2-stud edge position (-20, -48, 0)
                            edgeCandidates =
                                solveSnapCandidates 24.0
                                    0
                                    (Mat4.makeTranslate3 -20 -48 0)
                                    draggedAntiStuds
                                    targetStuds

                            -- Hovering at -10 (equidistant 10 LDU from center at 0 and edge at -20)
                            equidistantCandidates =
                                solveSnapCandidates 24.0
                                    0
                                    (Mat4.makeTranslate3 -10 -48 0)
                                    draggedAntiStuds
                                    targetStuds

                            closestEquidistantCandidates =
                                solveSnapCandidatesWithPriority ClosestPoint
                                    24.0
                                    0
                                    (Mat4.makeTranslate3 -11 -48 0)
                                    draggedAntiStuds
                                    targetStuds

                            edgeOrigin =
                                edgeCandidates
                                    |> List.head
                                    |> Maybe.map (\snap -> ( Mat4.transform snap.snapMatrix (vec3 0 0 0), snap.distance ))

                            equidistantOrigin =
                                equidistantCandidates
                                    |> List.head
                                    |> Maybe.map (\snap -> ( Mat4.transform snap.snapMatrix (vec3 0 0 0), snap.distance ))

                            closestOrigin =
                                closestEquidistantCandidates
                                    |> List.head
                                    |> Maybe.map (\snap -> Mat4.transform snap.snapMatrix (vec3 0 0 0))
                        in
                        Expect.all
                            [ \_ ->
                                case edgeOrigin of
                                    Just ( origin, dist ) ->
                                        Expect.all
                                            [ \_ ->
                                                Vec3.distance origin (vec3 -20 -48 0)
                                                    |> Expect.within (Expect.Absolute 0.01) 0
                                            , \_ -> Expect.equal 0 dist
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail "Expected edge snap candidate"
                            , \_ ->
                                case equidistantOrigin of
                                    Just ( origin, dist ) ->
                                        Expect.all
                                            [ \_ ->
                                                Vec3.distance origin (vec3 0 -48 0)
                                                    |> Expect.within (Expect.Absolute 0.01) 0
                                            , \_ -> Expect.equal 10 dist
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail "Expected center snap candidate at equidistant position"
                            , \_ ->
                                Expect.equal (Just (vec3 -20 -48 0)) closestOrigin
                            ]
                            ()

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        , test "hysteresis maintains partial latch until dragged towards center" <|
            \_ ->
                case knownCatalogPart "3003.dat" of
                    Just brick ->
                        let
                            draggedAntiStuds =
                                brick.connections
                                    |> List.filter (.connType >> (==) AntiStud)

                            targetStuds =
                                brick.connections
                                    |> List.filter (.connType >> (==) Stud)
                                    |> List.map (transformPoint (Mat4.makeTranslate3 0 -24 0) 1)

                            hoveringAtPartialAlignment =
                                Mat4.makeTranslate3 -20 -48 0

                            partialLatch =
                                solveSnapCandidates 0.5 0 hoveringAtPartialAlignment draggedAntiStuds targetStuds
                                    |> List.head

                            -- Hovering at -20 stays latched to the 2-stud edge
                            held =
                                solveSnapWithHysteresisAndPredicate
                                    (\_ -> True)
                                    24.0
                                    18.0
                                    partialLatch
                                    0
                                    hoveringAtPartialAlignment
                                    draggedAntiStuds
                                    targetStuds

                            -- Dragged towards center at X = -8 (dist 8 to center, dist 12 to edge)
                            upgraded =
                                solveSnapWithHysteresisAndPredicate
                                    (\_ -> True)
                                    24.0
                                    18.0
                                    partialLatch
                                    0
                                    (Mat4.makeTranslate3 -8 -48 0)
                                    draggedAntiStuds
                                    targetStuds
                        in
                        Expect.all
                            [ \_ ->
                                case held of
                                    Just snap ->
                                        let
                                            snappedOrigin =
                                                Mat4.transform snap.snapMatrix (vec3 0 0 0)
                                        in
                                        Vec3.distance snappedOrigin (vec3 -20 -48 0)
                                            |> Expect.within (Expect.Absolute 0.01) 0

                                    Nothing ->
                                        Expect.fail "Expected active latch to be held at -20"
                            , \_ ->
                                case upgraded of
                                    Just snap ->
                                        let
                                            snappedOrigin =
                                                Mat4.transform snap.snapMatrix (vec3 0 0 0)
                                        in
                                        Expect.all
                                            [ \_ ->
                                                Vec3.distance snappedOrigin (vec3 0 -48 0)
                                                    |> Expect.within (Expect.Absolute 0.01) 0
                                            , \_ -> Expect.equal 8 snap.distance
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail "Expected hysteresis to upgrade to the four-contact snap"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        , test "snaps slope 3039 anti-studs onto target studs of a 2x2 brick" <|
            \_ ->
                let
                    brickConn =
                        Connectivity.extractPartConnectivity "3003.dat" (\_ -> Nothing)

                    targetStuds =
                        brickConn.connections
                            |> List.filter (\c -> c.connType == Stud)
                            |> List.map
                                (\c ->
                                    { placementId = 1
                                    , pointId = c.id
                                    , connType = c.connType
                                    , worldPosition = c.localPosition
                                    , worldNormal = c.normal
                                    , worldTangent = c.tangent
                                    }
                                )

                    slopeConn =
                        Connectivity.extractPartConnectivity "3039.dat" (\_ -> Nothing)

                    hoverMat =
                        Mat4.makeTranslate3 1 -22 11
                in
                case solveSnap 12.0 0 hoverMat slopeConn.connections targetStuds of
                    Just snap ->
                        let
                            snappedPos =
                                Mat4.transform snap.snapMatrix (vec3 0 0 0)
                        in
                        Expect.all
                            [ \_ -> Expect.lessThan 0.001 (abs (Vec3.getX snappedPos - 0))
                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.getY snappedPos - -24))
                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.getZ snappedPos - 10))
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected snap candidate for slope on brick"
        , test "snaps 1x2 jumper plate 3794b anti-studs onto target studs of a 6x6 plate" <|
            \_ ->
                let
                    plate6x6 =
                        Connectivity.extractPartConnectivity "3958.dat" (\_ -> Nothing)

                    targetPoints =
                        List.map
                            (\p ->
                                { placementId = 0
                                , pointId = p.id
                                , connType = p.connType
                                , worldPosition = p.localPosition
                                , worldNormal = p.normal
                                , worldTangent = p.tangent
                                }
                            )
                            plate6x6.connections

                    jumper =
                        Connectivity.extractPartConnectivity "3794b.dat" (\_ -> Nothing)

                    -- Jumper dragged near X = 0, Z = 10 spanning studs at (-10, 0, 10) and (10, 0, 10)
                    initialMatrix =
                        Mat4.makeTranslate3 2 -8 12

                    candidates =
                        solveSnapCandidates 25.0 0 initialMatrix jumper.connections targetPoints
                in
                case candidates of
                    first :: _ ->
                        let
                            snapOrigin =
                                Mat4.transform first.snapMatrix (vec3 0 0 0)
                        in
                        Expect.all
                            [ \_ -> Expect.equal 0 (round (Vec3.getX snapOrigin))
                            , \_ -> Expect.equal -8 (round (Vec3.getY snapOrigin))
                            , \_ -> Expect.equal 10 (round (Vec3.getZ snapOrigin))
                            ]
                            ()

                    [] ->
                        Expect.fail "expected snap candidate for 1x2 jumper plate on 6x6 plate"
        , test "snaps 1x1 brick anti-stud onto central stud of 1x2 jumper plate" <|
            \_ ->
                let
                    jumper =
                        Connectivity.extractPartConnectivity "3794b.dat" (\_ -> Nothing)

                    targetPoints =
                        List.map
                            (\p ->
                                { placementId = 0
                                , pointId = p.id
                                , connType = p.connType
                                , worldPosition = p.localPosition
                                , worldNormal = p.normal
                                , worldTangent = p.tangent
                                }
                            )
                            jumper.connections

                    brick1x1 =
                        Connectivity.extractPartConnectivity "3005.dat" (\_ -> Nothing)

                    initialMatrix =
                        Mat4.makeTranslate3 3 -24 2

                    candidates =
                        solveSnapCandidates 25.0 0 initialMatrix brick1x1.connections targetPoints
                in
                case candidates of
                    first :: _ ->
                        let
                            snapOrigin =
                                Mat4.transform first.snapMatrix (vec3 0 0 0)
                        in
                        Expect.all
                            [ \_ -> Expect.equal 0 (round (Vec3.getX snapOrigin))
                            , \_ -> Expect.equal -24 (round (Vec3.getY snapOrigin))
                            , \_ -> Expect.equal 0 (round (Vec3.getZ snapOrigin))
                            ]
                            ()

                    [] ->
                        Expect.fail "expected snap candidate for 1x1 brick on 1x2 jumper plate"
        , test "rotationIndex cycles orientation around connection normal by 90-degree steps" <|
            \_ ->
                let
                    targetStud =
                        { placementId = 1
                        , pointId = 1
                        , connType = Stud
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 -1 0
                        , worldTangent = vec3 1 0 0
                        }

                    draggedAntiStud =
                        { id = 101
                        , connType = AntiStud
                        , localPosition = vec3 0 24 0
                        , normal = vec3 0 1 0
                        , tangent = vec3 1 0 0
                        }

                    currentHoverMat =
                        Mat4.makeTranslate3 0 -24 0

                    -- Step 1 = 90 degrees turn
                    snap90 =
                        solveSnap 12.0 1 currentHoverMat [ draggedAntiStud ] [ targetStud ]
                in
                case snap90 of
                    Just snap ->
                        let
                            -- In step 1 (90 deg), local X (1, 0, 0) rotates towards local Z (0, 0, 1) or (0, 0, -1)
                            transformedUnitX =
                                Vec3.sub
                                    (Mat4.transform snap.snapMatrix (vec3 1 24 0))
                                    (Mat4.transform snap.snapMatrix (vec3 0 24 0))
                        in
                        Expect.all
                            [ \_ -> Expect.lessThan 0.01 (abs (Vec3.getX transformedUnitX))
                            , \_ -> Expect.greaterThan 0.99 (abs (Vec3.getZ transformedUnitX))
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Snap 90 deg step failed"
        , test "does not snap beyond max distance" <|
            \_ ->
                let
                    targetStud =
                        { placementId = 1
                        , pointId = 1
                        , connType = Stud
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 -1 0
                        , worldTangent = vec3 1 0 0
                        }

                    draggedAntiStud =
                        { id = 101
                        , connType = AntiStud
                        , localPosition = vec3 0 24 0
                        , normal = vec3 0 1 0
                        , tangent = vec3 1 0 0
                        }

                    -- 30 LDU away (> 12 LDU threshold)
                    farHoverMat =
                        Mat4.makeTranslate3 0 -54 0
                in
                Expect.equal Nothing (solveSnap 12.0 0 farHoverMat [ draggedAntiStud ] [ targetStud ])
        , test "snaps Technic axle into gear axle hole" <|
            \_ ->
                let
                    gearAxleHole =
                        { placementId = 2
                        , pointId = 1
                        , connType = TechnicAxleFemale { length = 20.0 }
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 0 1
                        , worldTangent = vec3 1 0 0
                        }

                    axleConn =
                        { id = 1
                        , connType = TechnicAxleMale { length = 80.0 }
                        , localPosition = vec3 0 0 0
                        , normal = vec3 0 0 1
                        , tangent = vec3 1 0 0
                        }

                    hoverMat =
                        Mat4.makeTranslate3 2 1 5
                in
                case solveSnap 12.0 0 hoverMat [ axleConn ] [ gearAxleHole ] of
                    Just snap ->
                        let
                            axlePos =
                                Mat4.transform snap.snapMatrix axleConn.localPosition
                        in
                        Expect.lessThan 0.001 (Vec3.distance (vec3 0 0 0) axlePos)

                    Nothing ->
                        Expect.fail "Axle snap into gear failed"
        , test "snaps Technic pin into 24T gear pin hole" <|
            \_ ->
                case ( knownCatalogPart "2780.dat", knownCatalogPart "3648b.dat" ) of
                    ( Just pinPart, Just gearPart ) ->
                        let
                            gearWorldPoints =
                                List.map
                                    (\c ->
                                        { placementId = 1
                                        , pointId = c.id
                                        , connType = c.connType
                                        , worldPosition = c.localPosition
                                        , worldNormal = c.normal
                                        , worldTangent = c.tangent
                                        }
                                    )
                                    gearPart.connections

                            -- Hover near pin hole at (10, 10, 0)
                            hoverMat =
                                Mat4.makeTranslate3 10 10 -8
                        in
                        case solveSnap 12.0 0 hoverMat pinPart.connections gearWorldPoints of
                            Just snap ->
                                let
                                    snappedPos =
                                        Mat4.transform snap.snapMatrix snap.draggedPoint.localPosition
                                in
                                Expect.lessThan 0.001 (Vec3.distance snap.targetPoint.worldPosition snappedPos)

                            Nothing ->
                                Expect.fail "Pin snap into 24T gear failed"

                    _ ->
                        Expect.fail "Parts 2780.dat or 3648b.dat not found"
        , test "snaps Technic axle into beam pin hole" <|
            \_ ->
                let
                    beamPinHole =
                        { placementId = 2
                        , pointId = 1
                        , connType = TechnicPinFemale { length = 20.0 }
                        , worldPosition = vec3 0 10 0
                        , worldNormal = vec3 0 0 1
                        , worldTangent = vec3 1 0 0
                        }

                    axleConn =
                        { id = 1
                        , connType = TechnicAxleMale { length = 80.0 }
                        , localPosition = vec3 -10 0 0
                        , normal = vec3 1 0 0
                        , tangent = vec3 0 1 0
                        }

                    hoverMat =
                        Mat4.makeTranslate3 2 11 3
                in
                case solveSnap 12.0 0 hoverMat [ axleConn ] [ beamPinHole ] of
                    Just snap ->
                        let
                            axlePos =
                                Mat4.transform snap.snapMatrix axleConn.localPosition
                        in
                        Expect.lessThan 0.001 (Vec3.distance (vec3 0 10 0) axlePos)

                    Nothing ->
                        Expect.fail "Axle snap into beam pin hole failed"
        , test "snaps Technic pin 2780 into Technic brick 3700 hole with orientation rotation" <|
            \_ ->
                case ( knownCatalogPart "2780.dat", knownCatalogPart "3700.dat" ) of
                    ( Just pinPart, Just brickPart ) ->
                        let
                            brickWorldPoints =
                                List.map (Connectivity.transformPoint Mat4.identity 1) brickPart.connections

                            -- Hover near pin hole at (0, 10, 0)
                            hoverMat =
                                Mat4.makeTranslate3 8 10 2
                        in
                        case solveSnap 12.0 0 hoverMat pinPart.connections brickWorldPoints of
                            Just snap ->
                                let
                                    snappedPos =
                                        Mat4.transform snap.snapMatrix snap.draggedPoint.localPosition

                                    snappedNormal =
                                        transformDirection snap.snapMatrix (vec3 1 0 0)
                                in
                                Expect.all
                                    [ \_ -> Expect.lessThan 0.001 (Vec3.distance snap.targetPoint.worldPosition snappedPos)
                                    , \_ -> Expect.greaterThan 0.99 (abs (Vec3.getZ snappedNormal))
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "Pin snap into Technic brick 3700 hole failed"

                    _ ->
                        Expect.fail "Parts 2780.dat or 3700.dat not found"
        , test "snaps 43093 pin end into Technic brick hole and axle end into gear axle hole" <|
            \_ ->
                case ( knownCatalogPart "43093.dat", knownCatalogPart "3700.dat", knownCatalogPart "3648b.dat" ) of
                    ( Just part43093, Just brickPart, Just gearPart ) ->
                        let
                            brickWorldPoints =
                                List.map (Connectivity.transformPoint Mat4.identity 1) brickPart.connections

                            gearWorldPoints =
                                List.map (Connectivity.transformPoint Mat4.identity 2) gearPart.connections

                            -- Hover pin end (localPosition = -10 0 0) near brick hole at (0, 10, 0)
                            hoverPin =
                                Mat4.makeTranslate3 10 10 0

                            -- Hover axle end (localPosition = 10 0 0) near gear center axle hole at (0, 0, 0)
                            hoverAxle =
                                Mat4.makeTranslate3 -10 0 0
                        in
                        case ( solveSnap 12.0 0 hoverPin part43093.connections brickWorldPoints, solveSnap 12.0 0 hoverAxle part43093.connections gearWorldPoints ) of
                            ( Just pinSnap, Just axleSnap ) ->
                                let
                                    snappedPinPos =
                                        Mat4.transform pinSnap.snapMatrix pinSnap.draggedPoint.localPosition

                                    snappedAxlePos =
                                        Mat4.transform axleSnap.snapMatrix axleSnap.draggedPoint.localPosition

                                    snappedPinNormal =
                                        transformDirection pinSnap.snapMatrix (vec3 1 0 0)
                                in
                                Expect.all
                                    [ \_ -> Expect.equal 1 pinSnap.draggedPoint.id
                                    , \_ -> Expect.lessThan 0.001 (Vec3.distance pinSnap.targetPoint.worldPosition snappedPinPos)
                                    , \_ -> Expect.greaterThan 0.99 (abs (Vec3.getZ snappedPinNormal))
                                    , \_ -> Expect.equal 2 axleSnap.draggedPoint.id
                                    , \_ -> Expect.lessThan 0.001 (Vec3.distance axleSnap.targetPoint.worldPosition snappedAxlePos)
                                    , \_ ->
                                        case knownCatalogPart "3749.dat" of
                                            Just part3749 ->
                                                let
                                                    axlePinWorldPoints =
                                                        List.map (Connectivity.transformPoint Mat4.identity 3) part3749.connections

                                                    gearDragHover =
                                                        Mat4.makeTranslate3 10 0 0
                                                in
                                                case solveSnap 12.0 0 gearDragHover gearPart.connections axlePinWorldPoints of
                                                    Just gSnap ->
                                                        Expect.pass

                                                    Nothing ->
                                                        Expect.fail "Dragging gear onto 3749 axle pin failed to snap!"

                                            Nothing ->
                                                Expect.fail "Part 3749.dat not found in knownCatalogPart!"
                                    ]
                                    ()

                            ( Nothing, _ ) ->
                                Expect.fail "43093 pin end snap into brick hole failed"

                            ( _, Nothing ) ->
                                Expect.fail "43093 axle end snap into gear axle hole failed"

                    _ ->
                        Expect.fail "Parts 43093.dat, 3700.dat or 3648b.dat not found"
        , test "1x1 plate dragging near 2x2 plate on ground snaps on top (Y = -16) not below (Y = 0)" <|
            \_ ->
                case ( knownCatalogPart "3024.dat", knownCatalogPart "3022.dat" ) of
                    ( Just plate1x1, Just plate2x2 ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -8 0

                            targetWorldPoints =
                                List.map (Connectivity.transformPoint targetMatrix 1) plate2x2.connections

                            -- Hovering along ground near the (10, -8, 10) stud position
                            hoverMatrix =
                                Mat4.makeTranslate3 10 -8 10
                        in
                        case solveSnap 12.0 0 hoverMatrix plate1x1.connections targetWorldPoints of
                            Just snap ->
                                let
                                    snappedOrigin =
                                        Mat4.transform snap.snapMatrix (vec3 0 0 0)
                                in
                                Expect.all
                                    [ \_ -> Expect.equal -16 (round (Vec3.getY snappedOrigin))
                                    , \_ -> Expect.notEqual 0 (round (Vec3.getY snappedOrigin))
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "Snap candidate was not found"

                    _ ->
                        Expect.fail "Catalog parts 3024.dat or 3022.dat not found"
        , test "1x1 tile dragging near 1x1 brick snaps on top and isSnapColliding is False" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "3005.dat" ) of
                    ( Just tile1x1, Just brick1x1 ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            targetWorldPoints =
                                List.map (Connectivity.transformPoint targetMatrix 1) brick1x1.connections

                            hoverMatrix =
                                Mat4.makeTranslate3 0 -30 0

                            maybeSnap =
                                solveSnap 12.0 0 hoverMatrix tile1x1.connections targetWorldPoints
                        in
                        case maybeSnap of
                            Just snap ->
                                let
                                    snappedOrigin =
                                        Mat4.transform snap.snapMatrix (vec3 0 0 0)

                                    tileBox =
                                        List.head tile1x1.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
                                            |> (\b -> effectiveEnvelope b tile1x1.connections)

                                    brickBox =
                                        List.head brick1x1.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                                            |> (\b -> effectiveEnvelope b brick1x1.connections)

                                    colliding =
                                        isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, tileBox ) ] [ ( 1, targetMatrix, brickBox ) ] snap
                                in
                                Expect.all
                                    [ \_ -> Expect.equal -32 (round (Vec3.getY snappedOrigin))
                                    , \_ -> Expect.equal False colliding
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "Tile snap candidate not found"

                    _ ->
                        Expect.fail "3070b.dat or 3005.dat not found in catalog"
        , test "solveSnap with an upright 3070b near the 87087 side stud returns a matrix with the tile's down axis along +Z (auto-tip)" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "87087.dat" ) of
                    ( Just tile1x1, Just snot87087 ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            sideStud =
                                snot87087.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint targetMatrix 1)

                            -- Hovering upright in front of the side stud at (0, -14, -18)
                            hoverMatrix =
                                Mat4.makeTranslate3 0 -14 -18

                            maybeSnap =
                                solveSnap 12.0 0 hoverMatrix tile1x1.connections sideStud
                        in
                        case maybeSnap of
                            Just snap ->
                                let
                                    -- Down direction of the tile (local (0, 1, 0)) transformed to world
                                    downDir =
                                        Connectivity.transformDirection snap.snapMatrix (vec3 0 1 0)

                                    tileBox =
                                        List.head tile1x1.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
                                            |> (\b -> effectiveEnvelope b tile1x1.connections)

                                    snotBox =
                                        List.head snot87087.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                                            |> (\b -> effectiveEnvelope b snot87087.connections)

                                    colliding =
                                        isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, tileBox ) ] [ ( 1, targetMatrix, snotBox ) ] snap
                                in
                                Expect.all
                                    [ \_ -> Expect.greaterThan 0.7 (Vec3.getZ downDir)
                                    , \_ -> Expect.equal False colliding
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "SNOT side-stud snap candidate not found"

                    _ ->
                        Expect.fail "3070b.dat or 87087.dat not found in catalog"
        , test "solveSnap with a tile snaps cleanly onto each of the 4 side studs of 4733 without collision" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "4733.dat" ) of
                    ( Just tile1x1, Just travis ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            travisBox =
                                List.head travis.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                                    |> (\b -> effectiveEnvelope b travis.connections)

                            tileBox =
                                List.head tile1x1.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
                                    |> (\b -> effectiveEnvelope b tile1x1.connections)

                            sideStuds =
                                List.filter (\c -> c.connType == Stud && abs (Vec3.getY c.normal) < 0.3) travis.connections
                                    |> List.map (Connectivity.transformPoint targetMatrix 1)

                            testSideStud targetStud =
                                let
                                    offsetPos =
                                        Vec3.add targetStud.worldPosition (Vec3.scale 8 targetStud.worldNormal)

                                    hoverMatrix =
                                        Mat4.makeTranslate offsetPos

                                    maybeSnap =
                                        solveSnap 16.0 0 hoverMatrix tile1x1.connections [ targetStud ]
                                in
                                case maybeSnap of
                                    Just snap ->
                                        let
                                            colliding =
                                                isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, tileBox ) ] [ ( 1, targetMatrix, travisBox ) ] snap
                                        in
                                        Expect.all
                                            [ \_ -> Expect.equal AntiStud snap.draggedPoint.connType
                                            , \_ -> Expect.equal False colliding
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail "Tile snap candidate not found on 4733 side stud"
                        in
                        Expect.all (List.map (\stud -> \_ -> testSideStud stud) sideStuds) ()

                    _ ->
                        Expect.fail "3070b.dat or 4733.dat not found in catalog"
        , test "4070 headlight brick rear anti-stud snaps onto a stud and front recessed stud receives a tile" <|
            \_ ->
                case ( knownCatalogPart "4070.dat", knownCatalogPart "87087.dat", knownCatalogPart "3070b.dat" ) of
                    ( Just headlight, Just brick87087, Just tile1x1 ) ->
                        let
                            baseMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            -- Target side stud on 87087 facing -Z at (0, -14, -10)
                            baseStud =
                                brick87087.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint baseMatrix 1)

                            -- Hovering 4070 upright so its rear anti-stud (+Z) faces baseStud (-Z)
                            hoverHeadlight =
                                Mat4.makeTranslate3 0 -24 -24

                            rearSnap =
                                solveSnap 16.0 0 hoverHeadlight headlight.connections baseStud

                            targetHeadlightMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            recessedStud =
                                headlight.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint targetHeadlightMatrix 1)

                            -- Tipped tile so its underside (+Z) faces front recessed stud (-Z)
                            tipTile =
                                Mat4.makeRotate (pi / 2) (vec3 1 0 0)

                            hoverTile =
                                Mat4.mul (Mat4.makeTranslate3 0 -14 -16) tipTile

                            frontSnap =
                                solveSnap 16.0 0 hoverTile tile1x1.connections recessedStud

                            headlightBox =
                                List.head headlight.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                                    |> (\b -> effectiveEnvelope b headlight.connections)

                            tileBox =
                                List.head tile1x1.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
                                    |> (\b -> effectiveEnvelope b tile1x1.connections)
                        in
                        case ( rearSnap, frontSnap ) of
                            ( Just rSnap, Just fSnap ) ->
                                let
                                    colliding =
                                        isSnapColliding 0.5 2 hoverTile [ ( 2, hoverTile, tileBox ) ] [ ( 1, targetHeadlightMatrix, headlightBox ) ] fSnap
                                in
                                Expect.all
                                    [ \_ -> Expect.equal 101 rSnap.draggedPoint.id
                                    , \_ -> Expect.equal AntiStud rSnap.draggedPoint.connType
                                    , \_ -> Expect.equal False colliding
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Failed to snap on headlight rear anti-stud or front recessed stud"

                    _ ->
                        Expect.fail "4070.dat, 87087.dat, or 3070b.dat not found in catalog"
        , test "4-step in-flight rotation on a side stud preserves SO(3) orthonormality and clean alignment without collisions" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "87087.dat" ) of
                    ( Just tile1x1, Just snot87087 ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            sideStud =
                                snot87087.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint targetMatrix 1)

                            hoverMatrix =
                                Mat4.makeTranslate3 0 -14 -18

                            tileBox =
                                List.head tile1x1.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
                                    |> (\b -> effectiveEnvelope b tile1x1.connections)

                            snotBox =
                                List.head snot87087.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                                    |> (\b -> effectiveEnvelope b snot87087.connections)

                            verifyStep idx =
                                case solveSnap 16.0 idx hoverMatrix tile1x1.connections sideStud of
                                    Just snap ->
                                        let
                                            c1 =
                                                transformDirection snap.snapMatrix (vec3 1 0 0)

                                            c2 =
                                                transformDirection snap.snapMatrix (vec3 0 1 0)

                                            c3 =
                                                transformDirection snap.snapMatrix (vec3 0 0 1)

                                            det =
                                                Vec3.dot (Vec3.cross c1 c2) c3

                                            colliding =
                                                isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, tileBox ) ] [ ( 1, targetMatrix, snotBox ) ] snap
                                        in
                                        Expect.all
                                            [ \_ -> Expect.lessThan 0.001 (abs (Vec3.length c1 - 1.0))
                                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.length c2 - 1.0))
                                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.length c3 - 1.0))
                                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.dot c1 c2))
                                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.dot c2 c3))
                                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.dot c3 c1))
                                            , \_ -> Expect.lessThan 0.001 (abs (det - 1.0))
                                            , \_ -> Expect.equal False colliding
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail ("Failed rotation step " ++ String.fromInt idx)
                        in
                        Expect.all (List.map (\idx -> \_ -> verifyStep idx) [ 0, 1, 2, 3 ]) ()

                    _ ->
                        Expect.fail "3070b.dat or 87087.dat not found in catalog"
        , test "solveSnapCandidates allows snapping to horizontal side studs near ground level" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "87087.dat" ) of
                    ( Just tile1x1, Just snot87087 ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            sideStud =
                                snot87087.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint targetMatrix 1)

                            hoverMatrix =
                                Mat4.makeTranslate3 0 -14 -18

                            candidates =
                                solveSnapCandidates 16.0 0 hoverMatrix tile1x1.connections sideStud
                        in
                        Expect.equal False (List.isEmpty candidates)

                    _ ->
                        Expect.fail "3070b.dat or 87087.dat not found in catalog"
        ]


brickStacking2x2Tests : Test
brickStacking2x2Tests =
    let
        setup2x2 =
            case knownCatalogPart "3003.dat" of
                Just brick ->
                    let
                        draggedAntiStuds =
                            brick.connections
                                |> List.filter (.connType >> (==) AntiStud)

                        targetStuds =
                            brick.connections
                                |> List.filter (.connType >> (==) Stud)
                                |> List.map (transformPoint (Mat4.makeTranslate3 0 -24 0) 1)
                    in
                    Just ( draggedAntiStuds, targetStuds )

                Nothing ->
                    Nothing

        expectSnapPosition expectedOrigin expectedContacts dragged target actualSnap =
            case actualSnap of
                Just snap ->
                    let
                        origin =
                            Mat4.transform snap.snapMatrix (vec3 0 0 0)

                        contacts =
                            alignedContactCount snap.snapMatrix snap.targetPlacementId dragged target
                    in
                    Expect.all
                        [ \_ ->
                            Vec3.distance origin expectedOrigin
                                |> Expect.within (Expect.Absolute 0.01) 0
                        , \_ ->
                            Expect.equal expectedContacts contacts
                        ]
                        ()

                Nothing ->
                    Expect.fail "Expected snap candidate but got Nothing"
    in
    describe "2x2 brick stacking configurations (3003.dat)"
        [ test "center position (0, 0) selects 4-stud snap" <|
            \_ ->
                case setup2x2 of
                    Just ( dragged, target ) ->
                        solveSnap 12.0 0 (Mat4.makeTranslate3 0 -48 0) dragged target
                            |> expectSnapPosition (vec3 0 -48 0) 4 dragged target

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        , test "edge positions (±20, 0) and (0, ±20) select 2-stud snap" <|
            \_ ->
                case setup2x2 of
                    Just ( dragged, target ) ->
                        let
                            edgeOffsets =
                                [ ( 20, 0 ), ( -20, 0 ), ( 0, 20 ), ( 0, -20 ) ]

                            checkEdge ( dx, dz ) =
                                solveSnap 12.0 0 (Mat4.makeTranslate3 dx -48 dz) dragged target
                                    |> expectSnapPosition (vec3 dx -48 dz) 2 dragged target
                        in
                        Expect.all (List.map (\offset -> \_ -> checkEdge offset) edgeOffsets) ()

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        , test "corner positions (±20, ±20) select 1-stud snap" <|
            \_ ->
                case setup2x2 of
                    Just ( dragged, target ) ->
                        let
                            cornerOffsets =
                                [ ( 20, 20 ), ( 20, -20 ), ( -20, 20 ), ( -20, -20 ) ]

                            checkCorner ( dx, dz ) =
                                solveSnap 12.0 0 (Mat4.makeTranslate3 dx -48 dz) dragged target
                                    |> expectSnapPosition (vec3 dx -48 dz) 1 dragged target
                        in
                        Expect.all (List.map (\offset -> \_ -> checkCorner offset) cornerOffsets) ()

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        , test "all 9 configurations reachable when cursor is placed near them without Shift" <|
            \_ ->
                case setup2x2 of
                    Just ( dragged, target ) ->
                        let
                            allConfigs =
                                [ ( 0, 0, 4 )
                                , ( 20, 0, 2 )
                                , ( -20, 0, 2 )
                                , ( 0, 20, 2 )
                                , ( 0, -20, 2 )
                                , ( 20, 20, 1 )
                                , ( 20, -20, 1 )
                                , ( -20, 20, 1 )
                                , ( -20, -20, 1 )
                                ]

                            nearOffset ( dx, dz, contacts ) =
                                let
                                    px =
                                        dx
                                            + (if dx >= 0 then
                                                2.0

                                               else
                                                -2.0
                                              )

                                    pz =
                                        dz
                                            + (if dz >= 0 then
                                                2.0

                                               else
                                                -2.0
                                              )

                                    snap =
                                        solveSnapWithPredicateWithPriority
                                            MaximizeContacts
                                            (\_ -> True)
                                            12.0
                                            0
                                            (Mat4.makeTranslate3 px -48 pz)
                                            dragged
                                            target
                                in
                                \_ -> expectSnapPosition (vec3 dx -48 dz) contacts dragged target snap
                        in
                        Expect.all (List.map nearOffset allConfigs) ()

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        , test "all 9 configurations reachable with Shift (ClosestPoint) when cursor is placed near them" <|
            \_ ->
                case setup2x2 of
                    Just ( dragged, target ) ->
                        let
                            allConfigs =
                                [ ( 0, 0, 4 )
                                , ( 20, 0, 2 )
                                , ( -20, 0, 2 )
                                , ( 0, 20, 2 )
                                , ( 0, -20, 2 )
                                , ( 20, 20, 1 )
                                , ( 20, -20, 1 )
                                , ( -20, 20, 1 )
                                , ( -20, -20, 1 )
                                ]

                            nearOffset ( dx, dz, contacts ) =
                                let
                                    px =
                                        dx
                                            + (if dx >= 0 then
                                                2.0

                                               else
                                                -2.0
                                              )

                                    pz =
                                        dz
                                            + (if dz >= 0 then
                                                2.0

                                               else
                                                -2.0
                                              )

                                    snap =
                                        solveSnapWithPredicateWithPriority
                                            ClosestPoint
                                            (\_ -> True)
                                            12.0
                                            0
                                            (Mat4.makeTranslate3 px -48 pz)
                                            dragged
                                            target
                                in
                                \_ -> expectSnapPosition (vec3 dx -48 dz) contacts dragged target snap
                        in
                        Expect.all (List.map nearOffset allConfigs) ()

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        , test "hysteresis switches from 1-stud corner to 4-stud center when dragged towards center" <|
            \_ ->
                case setup2x2 of
                    Just ( dragged, target ) ->
                        let
                            activeCorner =
                                solveSnap 0.5 0 (Mat4.makeTranslate3 20 -48 20) dragged target

                            result =
                                solveSnapWithHysteresis
                                    snapEnterDistance
                                    snapReleaseDistance
                                    activeCorner
                                    0
                                    (Mat4.makeTranslate3 0 -48 0)
                                    dragged
                                    target
                        in
                        expectSnapPosition (vec3 0 -48 0) 4 dragged target result

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        , test "hysteresis switches from 4-stud center to 1-stud corner when dragged towards corner" <|
            \_ ->
                case setup2x2 of
                    Just ( dragged, target ) ->
                        let
                            activeCenter =
                                solveSnap 0.5 0 (Mat4.makeTranslate3 0 -48 0) dragged target

                            result =
                                solveSnapWithHysteresis
                                    snapEnterDistance
                                    snapReleaseDistance
                                    activeCenter
                                    0
                                    (Mat4.makeTranslate3 20 -48 20)
                                    dragged
                                    target
                        in
                        expectSnapPosition (vec3 20 -48 20) 1 dragged target result

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        , test "hysteresis provides bidirectional switching between center (4-stud) and edge (2-stud)" <|
            \_ ->
                case setup2x2 of
                    Just ( dragged, target ) ->
                        let
                            activeCenter =
                                solveSnap 0.5 0 (Mat4.makeTranslate3 0 -48 0) dragged target

                            activeEdge =
                                solveSnap 0.5 0 (Mat4.makeTranslate3 20 -48 0) dragged target

                            -- Dragging from center towards edge:
                            -- At X = 14, active center stays latched
                            centerAt14 =
                                solveSnapWithHysteresis
                                    snapEnterDistance
                                    snapReleaseDistance
                                    activeCenter
                                    0
                                    (Mat4.makeTranslate3 14 -48 0)
                                    dragged
                                    target

                            -- At X = 16, edge snap overcomes the latch barrier and replaces center
                            centerAt16 =
                                solveSnapWithHysteresis
                                    snapEnterDistance
                                    snapReleaseDistance
                                    activeCenter
                                    0
                                    (Mat4.makeTranslate3 16 -48 0)
                                    dragged
                                    target

                            -- Dragging from edge towards center:
                            -- At X = 11, active edge stays latched
                            edgeAt11 =
                                solveSnapWithHysteresis
                                    snapEnterDistance
                                    snapReleaseDistance
                                    activeEdge
                                    0
                                    (Mat4.makeTranslate3 11 -48 0)
                                    dragged
                                    target

                            -- At X = 9, center snap overcomes the latch barrier and replaces edge
                            edgeAt9 =
                                solveSnapWithHysteresis
                                    snapEnterDistance
                                    snapReleaseDistance
                                    activeEdge
                                    0
                                    (Mat4.makeTranslate3 9 -48 0)
                                    dragged
                                    target
                        in
                        Expect.all
                            [ \_ -> expectSnapPosition (vec3 0 -48 0) 4 dragged target centerAt14
                            , \_ -> expectSnapPosition (vec3 20 -48 0) 2 dragged target centerAt16
                            , \_ -> expectSnapPosition (vec3 20 -48 0) 2 dragged target edgeAt11
                            , \_ -> expectSnapPosition (vec3 0 -48 0) 4 dragged target edgeAt9
                            ]
                            ()

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        , test "hysteresis provides bidirectional switching between edge (2-stud) and corner (1-stud)" <|
            \_ ->
                case setup2x2 of
                    Just ( dragged, target ) ->
                        let
                            activeEdge =
                                solveSnap 0.5 0 (Mat4.makeTranslate3 20 -48 0) dragged target

                            activeCorner =
                                solveSnap 0.5 0 (Mat4.makeTranslate3 20 -48 20) dragged target

                            -- Dragging from edge towards corner along Z:
                            -- At Z = 13, active edge stays latched
                            edgeAt13 =
                                solveSnapWithHysteresis
                                    snapEnterDistance
                                    snapReleaseDistance
                                    activeEdge
                                    0
                                    (Mat4.makeTranslate3 20 -48 13)
                                    dragged
                                    target

                            -- At Z = 15, corner snap overcomes the barrier and replaces edge
                            edgeAt15 =
                                solveSnapWithHysteresis
                                    snapEnterDistance
                                    snapReleaseDistance
                                    activeEdge
                                    0
                                    (Mat4.makeTranslate3 20 -48 15)
                                    dragged
                                    target

                            -- Dragging from corner towards edge along Z:
                            -- At Z = 10, active corner stays latched
                            cornerAt10 =
                                solveSnapWithHysteresis
                                    snapEnterDistance
                                    snapReleaseDistance
                                    activeCorner
                                    0
                                    (Mat4.makeTranslate3 20 -48 10)
                                    dragged
                                    target

                            -- At Z = 8, edge snap overcomes the barrier and replaces corner
                            cornerAt8 =
                                solveSnapWithHysteresis
                                    snapEnterDistance
                                    snapReleaseDistance
                                    activeCorner
                                    0
                                    (Mat4.makeTranslate3 20 -48 8)
                                    dragged
                                    target
                        in
                        Expect.all
                            [ \_ -> expectSnapPosition (vec3 20 -48 0) 2 dragged target edgeAt13
                            , \_ -> expectSnapPosition (vec3 20 -48 20) 1 dragged target edgeAt15
                            , \_ -> expectSnapPosition (vec3 20 -48 20) 1 dragged target cornerAt10
                            , \_ -> expectSnapPosition (vec3 20 -48 0) 2 dragged target cornerAt8
                            ]
                            ()

                    Nothing ->
                        Expect.fail "3003.dat is not in the connectivity catalog"
        ]


collisionSATTests : Test
collisionSATTests =
    describe "3D SAT collision detection"
        [ test "detects intersection between two overlapping 2x4 bricks" <|
            \_ ->
                let
                    box =
                        { min = vec3 -40 0 -20, max = vec3 40 24 20 }

                    matA =
                        Mat4.makeTranslate3 0 0 0

                    matB =
                        Mat4.makeTranslate3 10 5 5

                    obbA =
                        toOBB box matA

                    obbB =
                        toOBB box matB
                in
                Expect.equal True (testOBB 0.5 obbA obbB)
        , test "two stacked bricks with effective envelope do not collide (face-touching overlap 0)" <|
            \_ ->
                let
                    boxA =
                        { min = vec3 -40 0 -20, max = vec3 40 24 20 }

                    boxB =
                        { min = vec3 -40 0 -20, max = vec3 40 24 20 }

                    -- Brick A at y = 0..24
                    matA =
                        Mat4.makeTranslate3 0 0 0

                    -- Brick B stacked on top at y = -24..0
                    matB =
                        Mat4.makeTranslate3 0 -24 0

                    obbA =
                        toOBB boxA matA

                    obbB =
                        toOBB boxB matB
                in
                Expect.equal False (testOBB 0.5 obbA obbB)
        , test "vertical stacking plate-on-plate (3024 on 3022) with effectiveEnvelope does not collide" <|
            \_ ->
                case ( knownCatalogPart "3022.dat", knownCatalogPart "3024.dat" ) of
                    ( Just conn3022, Just conn3024 ) ->
                        case ( conn3022.boundingBoxes, conn3024.boundingBoxes ) of
                            ( [ box3022 ], [ box3024 ] ) ->
                                let
                                    env3022 =
                                        effectiveEnvelope box3022 conn3022.connections

                                    env3024 =
                                        effectiveEnvelope box3024 conn3024.connections

                                    -- Plate 3022 placed at y = 0..8
                                    mat3022 =
                                        Mat4.makeTranslate3 0 0 0

                                    -- Plate 3024 stacked on top: translated by y = -8 (occupies y = -8..0 in world)
                                    mat3024 =
                                        Mat4.makeTranslate3 10 -8 10

                                    obb3022 =
                                        toOBB env3022 mat3022

                                    obb3024 =
                                        toOBB env3024 mat3024

                                    rawObb3022 =
                                        toOBB box3022 mat3022

                                    rawObb3024 =
                                        toOBB box3024 mat3024
                                in
                                Expect.all
                                    [ \_ -> Expect.equal True (testOBB 0.5 rawObb3022 rawObb3024)
                                    , \_ -> Expect.equal False (testOBB 0.5 obb3022 obb3024)
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected 1 bounding box each for 3022 and 3024"

                    _ ->
                        Expect.fail "Catalog parts 3022.dat and 3024.dat must exist"
        , test "vertical stacking plate-on-brick (3024 on 3005) with effectiveEnvelope does not collide" <|
            \_ ->
                case ( knownCatalogPart "3005.dat", knownCatalogPart "3024.dat" ) of
                    ( Just conn3005, Just conn3024 ) ->
                        case ( conn3005.boundingBoxes, conn3024.boundingBoxes ) of
                            ( [ box3005 ], [ box3024 ] ) ->
                                let
                                    env3005 =
                                        effectiveEnvelope box3005 conn3005.connections

                                    env3024 =
                                        effectiveEnvelope box3024 conn3024.connections

                                    -- Brick 3005 placed at y = 0..24
                                    mat3005 =
                                        Mat4.makeTranslate3 0 0 0

                                    -- Plate 3024 stacked on top: translated by y = -8 (occupies y = -8..0 in world)
                                    mat3024 =
                                        Mat4.makeTranslate3 0 -8 0

                                    obb3005 =
                                        toOBB env3005 mat3005

                                    obb3024 =
                                        toOBB env3024 mat3024

                                    rawObb3005 =
                                        toOBB box3005 mat3005

                                    rawObb3024 =
                                        toOBB box3024 mat3024
                                in
                                Expect.all
                                    [ \_ -> Expect.equal True (testOBB 0.5 rawObb3005 rawObb3024)
                                    , \_ -> Expect.equal False (testOBB 0.5 obb3005 obb3024)
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected 1 bounding box each for 3005 and 3024"

                    _ ->
                        Expect.fail "Catalog parts 3005.dat and 3024.dat must exist"
        , test "brick intersecting by 10 LDU inside another is flagged by checkCollisions" <|
            \_ ->
                let
                    box =
                        { min = vec3 -10 0 -10, max = vec3 10 24 10 }

                    dragged =
                        [ ( 10, Mat4.makeTranslate3 5 0 0, box ) ]

                    scene =
                        [ ( 1, Mat4.makeTranslate3 0 0 0, box ) ]

                    collisions =
                        checkCollisions 0.5 dragged scene
                in
                Expect.equal [ ( 10, 1 ) ] collisions
        , test "effectiveEnvelope trims top stud protrusion from y = -4 to 0" <|
            \_ ->
                let
                    rawBox =
                        { min = vec3 -40 -4 -20, max = vec3 40 24 20 }

                    studPoints =
                        [ { id = 1
                          , connType = Stud
                          , localPosition = vec3 0 0 0
                          , normal = vec3 0 -1 0
                          , tangent = vec3 1 0 0
                          }
                        ]

                    effective =
                        effectiveEnvelope rawBox studPoints
                in
                Expect.all
                    [ \b -> Expect.equal 0 (Vec3.getY b.min)
                    , \b -> Expect.equal 24 (Vec3.getY b.max)
                    ]
                    effective
        , test "effectiveEnvelope correctly trims multi-level stud bases using min for min faces and max for max faces" <|
            \_ ->
                let
                    box =
                        { min = vec3 -14 -8 -14, max = vec3 14 28 14 }

                    stud1 =
                        { id = 1
                        , connType = Stud
                        , localPosition = vec3 0 -4 0
                        , normal = vec3 0 -1 0
                        , tangent = vec3 1 0 0
                        }

                    stud2 =
                        { id = 2
                        , connType = Stud
                        , localPosition = vec3 0 0 0
                        , normal = vec3 0 -1 0
                        , tangent = vec3 1 0 0
                        }

                    env =
                        effectiveEnvelope box [ stud1, stud2 ]
                in
                Expect.equal -4 (round (Vec3.getY env.min))
        , describe "solveSnapWithHysteresis"
            [ test "enters snap within snapEnterDistance (12 LDU)" <|
                \_ ->
                    let
                        dragged =
                            [ { id = 1, connType = AntiStud, localPosition = vec3 0 24 0, normal = vec3 0 1 0, tangent = vec3 1 0 0 } ]

                        target =
                            [ { pointId = 10, placementId = 1, connType = Stud, worldPosition = vec3 0 24 0, worldNormal = vec3 0 -1 0, worldTangent = vec3 1 0 0 } ]

                        result =
                            solveSnapWithHysteresis snapEnterDistance snapReleaseDistance Nothing 0 (Mat4.makeTranslate3 8 0 0) dragged target
                    in
                    Expect.notEqual Nothing result
            , test "rejects snap at 15 LDU when not already snapped (enter = 12 LDU)" <|
                \_ ->
                    let
                        dragged =
                            [ { id = 1, connType = AntiStud, localPosition = vec3 0 24 0, normal = vec3 0 1 0, tangent = vec3 1 0 0 } ]

                        target =
                            [ { pointId = 10, placementId = 1, connType = Stud, worldPosition = vec3 0 24 0, worldNormal = vec3 0 -1 0, worldTangent = vec3 1 0 0 } ]

                        result =
                            solveSnapWithHysteresis snapEnterDistance snapReleaseDistance Nothing 0 (Mat4.makeTranslate3 15 0 0) dragged target
                    in
                    Expect.equal Nothing result
            , test "maintains latch at 15 LDU when already active (release = 18 LDU)" <|
                \_ ->
                    let
                        draggedPt =
                            { id = 1, connType = AntiStud, localPosition = vec3 0 24 0, normal = vec3 0 1 0, tangent = vec3 1 0 0 }

                        targetPt =
                            { pointId = 10, placementId = 1, connType = Stud, worldPosition = vec3 0 24 0, worldNormal = vec3 0 -1 0, worldTangent = vec3 1 0 0 }

                        activeSnap =
                            solveSnap snapEnterDistance 0 (Mat4.makeTranslate3 5 0 0) [ draggedPt ] [ targetPt ]

                        result =
                            solveSnapWithHysteresis snapEnterDistance snapReleaseDistance activeSnap 0 (Mat4.makeTranslate3 15 0 0) [ draggedPt ] [ targetPt ]
                    in
                    Expect.notEqual Nothing result
            , test "breaks latch when pulled beyond release distance (20 LDU > 18 LDU)" <|
                \_ ->
                    let
                        draggedPt =
                            { id = 1, connType = AntiStud, localPosition = vec3 0 24 0, normal = vec3 0 1 0, tangent = vec3 1 0 0 }

                        targetPt =
                            { pointId = 10, placementId = 1, connType = Stud, worldPosition = vec3 0 24 0, worldNormal = vec3 0 -1 0, worldTangent = vec3 1 0 0 }

                        activeSnap =
                            solveSnap snapEnterDistance 0 (Mat4.makeTranslate3 5 0 0) [ draggedPt ] [ targetPt ]

                        result =
                            solveSnapWithHysteresis snapEnterDistance snapReleaseDistance activeSnap 0 (Mat4.makeTranslate3 20 0 0) [ draggedPt ] [ targetPt ]
                    in
                    Expect.equal Nothing result
            , test "directional connections unlatch if rotated away even within hysteresis distance" <|
                \_ ->
                    let
                        draggedPt =
                            { id = 1, connType = AntiStud, localPosition = vec3 0 0 0, normal = vec3 0 1 0, tangent = vec3 1 0 0 }

                        targetPt =
                            { pointId = 10, placementId = 1, connType = Stud, worldPosition = vec3 0 0 0, worldNormal = vec3 0 -1 0, worldTangent = vec3 1 0 0 }

                        activeSnap =
                            solveSnap snapEnterDistance 0 (Mat4.makeTranslate3 5 0 0) [ draggedPt ] [ targetPt ]

                        -- Rotated 180 degrees around Z axis (normal inverted to 0, -1, 0, facing away from desired normal 0, 1, 0)
                        -- at distance 10 LDU (well within release distance 18 LDU)
                        rotatedMatrix =
                            Mat4.mul (Mat4.makeTranslate3 10 0 0) (Mat4.makeRotate (degrees 180) (vec3 0 0 1))

                        result =
                            solveSnapWithHysteresis snapEnterDistance snapReleaseDistance activeSnap 0 rotatedMatrix [ draggedPt ] [ targetPt ]
                    in
                    Expect.equal Nothing result
            ]
        , describe "collision-aware snapping and isSnapColliding"
            [ test "properly stacked 2x4 brick on another 2x4 brick does not collide (isSnapColliding = False)" <|
                \_ ->
                    let
                        brickA =
                            -- Base 2x4 brick at origin Y=0 (body Y in [0, 24], top studs at Y=0)
                            { id = 1
                            , matrix = Mat4.identity
                            , box = { min = vec3 -40 0 -10, max = vec3 40 24 10 }
                            }

                        draggedBrickB =
                            -- Dragged 2x4 brick
                            { id = 2
                            , matrix = Mat4.makeTranslate3 0 -24 0
                            , box = { min = vec3 -40 0 -10, max = vec3 40 24 10 }
                            }

                        targetStud =
                            { placementId = 1
                            , pointId = 1
                            , connType = Stud
                            , worldPosition = vec3 -30 0 -10
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }

                        draggedAntiStud =
                            { id = 101
                            , connType = AntiStud
                            , localPosition = vec3 -30 24 -10
                            , normal = vec3 0 1 0
                            , tangent = vec3 1 0 0
                            }

                        maybeSnap =
                            solveSnap 12.0 0 (Mat4.makeTranslate3 0 -22 0) [ draggedAntiStud ] [ targetStud ]
                    in
                    case maybeSnap of
                        Just snap ->
                            let
                                colliding =
                                    isSnapColliding 0.5 draggedBrickB.id draggedBrickB.matrix [ ( draggedBrickB.id, draggedBrickB.matrix, draggedBrickB.box ) ] [ ( brickA.id, brickA.matrix, brickA.box ) ] snap
                            in
                            Expect.equal False colliding

                        Nothing ->
                            Expect.fail "Expected snap candidate"
            , test "snapping brick inside another brick returns isSnapColliding = True" <|
                \_ ->
                    let
                        brickA =
                            -- Brick 1 at origin Y=0 (Y in [0, 24])
                            { id = 1
                            , matrix = Mat4.identity
                            , box = { min = vec3 -40 0 -10, max = vec3 40 24 10 }
                            }

                        brickB =
                            -- Brick 2 stacked on Brick 1 (Y in [-24, 0])
                            { id = 2
                            , matrix = Mat4.makeTranslate3 0 -24 0
                            , box = { min = vec3 -40 0 -10, max = vec3 40 24 10 }
                            }

                        draggedBrickC =
                            { id = 3
                            , matrix = Mat4.makeTranslate3 5 -20 5
                            , box = { min = vec3 -40 0 -10, max = vec3 40 24 10 }
                            }

                        -- Covered stud on Brick 1 at Y=0 pointing up
                        coveredStud =
                            { placementId = 1
                            , pointId = 1
                            , connType = Stud
                            , worldPosition = vec3 0 0 0
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }

                        draggedAntiStud =
                            { id = 101
                            , connType = AntiStud
                            , localPosition = vec3 0 24 0
                            , normal = vec3 0 1 0
                            , tangent = vec3 1 0 0
                            }

                        maybeSnap =
                            solveSnap 12.0 0 (Mat4.makeTranslate3 0 -20 0) [ draggedAntiStud ] [ coveredStud ]
                    in
                    case maybeSnap of
                        Just snap ->
                            let
                                -- snapMatrix places Brick C at Y in [-24, 0], which is occupied by Brick B!
                                colliding =
                                    isSnapColliding 0.5 draggedBrickC.id draggedBrickC.matrix [ ( draggedBrickC.id, draggedBrickC.matrix, draggedBrickC.box ) ] [ ( brickA.id, brickA.matrix, brickA.box ), ( brickB.id, brickB.matrix, brickB.box ) ] snap
                            in
                            Expect.equal True colliding

                        Nothing ->
                            Expect.fail "Expected snap candidate"
            , test "solveSnapWithPredicate falls back to second candidate when closest candidate collides" <|
                \_ ->
                    let
                        brickOccupied =
                            -- Occupies X in [0, 40], Y in [-24, 0]
                            ( 1, Mat4.makeTranslate3 20 -24 0, { min = vec3 -20 0 -10, max = vec3 20 24 10 } )

                        dragged =
                            -- 1x2 brick
                            ( 2, Mat4.makeTranslate3 8 -20 0, { min = vec3 -10 0 -10, max = vec3 10 24 10 } )

                        -- Candidate 1: at X = 10 (closer, distance ~ 2 LDU, but collides with brickOccupied at X in [0, 40])
                        targetStud1 =
                            { placementId = 3
                            , pointId = 1
                            , connType = Stud
                            , worldPosition = vec3 10 0 0
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }

                        -- Candidate 2: at X = -10 (farther, distance ~ 18 LDU from X=8, open space, no collision)
                        targetStud2 =
                            { placementId = 3
                            , pointId = 2
                            , connType = Stud
                            , worldPosition = vec3 -10 0 0
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }

                        draggedAntiStud =
                            { id = 101
                            , connType = AntiStud
                            , localPosition = vec3 0 24 0
                            , normal = vec3 0 1 0
                            , tangent = vec3 1 0 0
                            }

                        currentHover =
                            Mat4.makeTranslate3 8 -20 0

                        predicate snap =
                            not (isSnapColliding 0.5 2 currentHover [ dragged ] [ brickOccupied ] snap)

                        result =
                            solveSnapWithPredicate predicate 24.0 0 currentHover [ draggedAntiStud ] [ targetStud1, targetStud2 ]
                    in
                    case result of
                        Just snap ->
                            -- The solver skipped colliding targetStud1 and picked targetStud2!
                            Expect.equal 2 snap.targetPoint.pointId

                        Nothing ->
                            Expect.fail "Expected non-colliding candidate to be chosen"
            , test "solveSnapWithPredicate returns Nothing when all candidates collide" <|
                \_ ->
                    let
                        brickOccupied =
                            ( 1, Mat4.makeTranslate3 0 -24 0, { min = vec3 -20 0 -10, max = vec3 20 24 10 } )

                        dragged =
                            ( 2, Mat4.makeTranslate3 0 -20 0, { min = vec3 -10 0 -10, max = vec3 10 24 10 } )

                        targetStud1 =
                            { placementId = 3
                            , pointId = 1
                            , connType = Stud
                            , worldPosition = vec3 0 0 0
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }

                        draggedAntiStud =
                            { id = 101
                            , connType = AntiStud
                            , localPosition = vec3 0 24 0
                            , normal = vec3 0 1 0
                            , tangent = vec3 1 0 0
                            }

                        currentHover =
                            Mat4.makeTranslate3 0 -20 0

                        predicate snap =
                            not (isSnapColliding 0.5 2 currentHover [ dragged ] [ brickOccupied ] snap)

                        result =
                            solveSnapWithPredicate predicate 24.0 0 currentHover [ draggedAntiStud ] [ targetStud1 ]
                    in
                    Expect.equal Nothing result
            , test "isSnapColliding allows Technic pin lead part to insert into hole of targetPlacementId" <|
                \_ ->
                    let
                        beamTarget =
                            -- Target beam placementId = 10
                            { id = 10
                            , matrix = Mat4.identity
                            , box = { min = vec3 -10 -10 -50, max = vec3 10 10 50 }
                            }

                        draggedPin =
                            -- Dragged pin placementId = 20
                            { id = 20
                            , matrix = Mat4.makeTranslate3 0 0 -8
                            , box = { min = vec3 -5 -5 -10, max = vec3 5 5 10 }
                            }

                        holeTarget =
                            { placementId = 10
                            , pointId = 1
                            , connType = TechnicPinFemale { length = 10.0 }
                            , worldPosition = vec3 0 0 0
                            , worldNormal = vec3 0 0 1
                            , worldTangent = vec3 1 0 0
                            }

                        pinConn =
                            { id = 1
                            , connType = TechnicPinMale { length = 10.0, friction = True }
                            , localPosition = vec3 0 0 10
                            , normal = vec3 0 0 1
                            , tangent = vec3 1 0 0
                            }

                        maybeSnap =
                            solveSnap 12.0 0 (Mat4.makeTranslate3 0 0 -8) [ pinConn ] [ holeTarget ]
                    in
                    case maybeSnap of
                        Just snap ->
                            let
                                colliding =
                                    isSnapColliding 0.5 draggedPin.id draggedPin.matrix [ ( draggedPin.id, draggedPin.matrix, draggedPin.box ) ] [ ( beamTarget.id, beamTarget.matrix, beamTarget.box ) ] snap
                            in
                            Expect.equal False colliding

                        Nothing ->
                            Expect.fail "Expected snap candidate for Technic pin"
            , test "isSnapColliding flags collision if follower part in multi-part drag collides with targetPlacementId" <|
                \_ ->
                    let
                        beamTarget =
                            -- Target beam placementId = 10, extends from Z in [-50, 50]
                            { id = 10
                            , matrix = Mat4.identity
                            , box = { min = vec3 -10 -10 -50, max = vec3 10 10 50 }
                            }

                        leadPin =
                            { id = 20
                            , matrix = Mat4.identity
                            , box = { min = vec3 -5 -5 -10, max = vec3 5 5 10 }
                            }

                        -- Follower part is offset by Z = 10, placing it inside beamTarget's box!
                        followerPart =
                            { id = 21
                            , matrix = Mat4.makeTranslate3 0 0 10
                            , box = { min = vec3 -10 -10 -10, max = vec3 10 10 10 }
                            }

                        holeTarget =
                            { placementId = 10
                            , pointId = 1
                            , connType = TechnicPinFemale { length = 10.0 }
                            , worldPosition = vec3 0 0 0
                            , worldNormal = vec3 0 0 1
                            , worldTangent = vec3 1 0 0
                            }

                        pinConn =
                            { id = 1
                            , connType = TechnicPinMale { length = 10.0, friction = True }
                            , localPosition = vec3 0 0 0
                            , normal = vec3 0 0 1
                            , tangent = vec3 1 0 0
                            }

                        maybeSnap =
                            solveSnap 12.0 0 Mat4.identity [ pinConn ] [ holeTarget ]
                    in
                    case maybeSnap of
                        Just snap ->
                            let
                                colliding =
                                    isSnapColliding 0.5 leadPin.id leadPin.matrix [ ( leadPin.id, leadPin.matrix, leadPin.box ), ( followerPart.id, followerPart.matrix, followerPart.box ) ] [ ( beamTarget.id, beamTarget.matrix, beamTarget.box ) ] snap
                            in
                            Expect.equal True colliding

                        Nothing ->
                            Expect.fail "Expected snap candidate"
            , test "snapping Bracket 1x2 Down (99781) onto Brick 1x2 (3004) returns isSnapColliding = False" <|
                \_ ->
                    case ( knownCatalogPart "99781.dat", knownCatalogPart "3004.dat" ) of
                        ( Just bracket, Just brick ) ->
                            let
                                targetMatrix =
                                    Mat4.makeTranslate3 0 -24 0

                                targetWorldPoints =
                                    List.map (Connectivity.transformPoint targetMatrix 1) brick.connections

                                hoverMatrix =
                                    Mat4.makeTranslate3 0 -30 0

                                maybeSnap =
                                    solveSnap 12.0 0 hoverMatrix bracket.connections targetWorldPoints
                            in
                            case maybeSnap of
                                Just snap ->
                                    let
                                        bracketBox =
                                            List.head bracket.boundingBoxes
                                                |> Maybe.withDefault { min = vec3 -20 -4 -14, max = vec3 20 20 10 }
                                                |> (\b -> effectiveEnvelope b bracket.connections)

                                        brickBox =
                                            List.head brick.boundingBoxes
                                                |> Maybe.withDefault { min = vec3 -20 -4 -10, max = vec3 20 24 10 }
                                                |> (\b -> effectiveEnvelope b brick.connections)

                                        colliding =
                                            isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, bracketBox ) ] [ ( 1, targetMatrix, brickBox ) ] snap
                                    in
                                    Expect.equal False colliding

                                Nothing ->
                                    Expect.fail "Bracket snap candidate not found"

                        _ ->
                            Expect.fail "99781.dat or 3004.dat not found in catalog"
            , test "snapping Plate 1x2 onto side studs of Bracket 99781 returns isSnapColliding = False" <|
                \_ ->
                    case ( knownCatalogPart "3023.dat", knownCatalogPart "99781.dat" ) of
                        ( Just plate, Just bracket ) ->
                            let
                                targetMatrix =
                                    Mat4.makeTranslate3 0 -32 0

                                sideStuds =
                                    bracket.connections
                                        |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                        |> List.map (Connectivity.transformPoint targetMatrix 1)

                                hoverMatrix =
                                    Mat4.makeTranslate3 0 -22 -20

                                maybeSnap =
                                    solveSnap 16.0 0 hoverMatrix plate.connections sideStuds
                            in
                            case maybeSnap of
                                Just snap ->
                                    let
                                        plateBox =
                                            List.head plate.boundingBoxes
                                                |> Maybe.withDefault { min = vec3 -20 -4 -10, max = vec3 20 8 10 }
                                                |> (\b -> effectiveEnvelope b plate.connections)

                                        bracketBox =
                                            List.head bracket.boundingBoxes
                                                |> Maybe.withDefault { min = vec3 -20 -4 -14, max = vec3 20 20 10 }
                                                |> (\b -> effectiveEnvelope b bracket.connections)

                                        colliding =
                                            isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, plateBox ) ] [ ( 1, targetMatrix, bracketBox ) ] snap
                                    in
                                    Expect.equal False colliding

                                Nothing ->
                                    Expect.fail "Plate onto bracket side stud snap candidate not found"

                        _ ->
                            Expect.fail "3023.dat or 99781.dat not found in catalog"
            ]
        ]


shortestArcSnapTests : Test
shortestArcSnapTests =
    describe "yaw preservation and SNOT tilt snap"
        [ test "snapping preserves in-flight yaw rotation" <|
            \_ ->
                case ( knownCatalogPart "3001.dat", knownCatalogPart "3001.dat" ) of
                    ( Just dragged, Just target ) ->
                        let
                            targetMatrix =
                                Mat4.identity

                            targetStuds =
                                target.connections
                                    |> List.filter (\c -> c.connType == Stud)
                                    |> List.map (Connectivity.transformPoint targetMatrix 1)

                            -- Dragged brick rotated 90 degrees around Y (normal stays (0, -1, 0))
                            hoverMatrix =
                                Mat4.mul
                                    (Mat4.makeTranslate3 0 -24 0)
                                    (Mat4.makeRotate (pi / 2) (vec3 0 1 0))

                            maybeSnap =
                                solveSnap 16.0 0 hoverMatrix dragged.connections targetStuds
                        in
                        case maybeSnap of
                            Just snap ->
                                let
                                    hoverDir =
                                        transformDirection hoverMatrix (vec3 1 0 0)

                                    snapDir =
                                        transformDirection snap.snapMatrix (vec3 1 0 0)

                                    diff =
                                        Vec3.distance hoverDir snapDir
                                in
                                Expect.lessThan 0.001 diff

                            Nothing ->
                                Expect.fail "Snap candidate not found"

                    _ ->
                        Expect.fail "3001.dat not found in catalog"
        , test "snapping onto side studs tilts normal via shortest arc without unwanted yaw" <|
            \_ ->
                case ( knownCatalogPart "3023.dat", knownCatalogPart "99781.dat" ) of
                    ( Just plate, Just bracket ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -32 0

                            sideStuds =
                                bracket.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint targetMatrix 1)

                            hoverMatrix =
                                Mat4.makeTranslate3 0 -22 -20

                            maybeSnap =
                                solveSnap 16.0 0 hoverMatrix plate.connections sideStuds
                        in
                        case maybeSnap of
                            Just snap ->
                                let
                                    snapNormal =
                                        transformDirection snap.snapMatrix (vec3 0 1 0)

                                    snapTangent =
                                        transformDirection snap.snapMatrix (vec3 1 0 0)
                                in
                                Expect.all
                                    [ \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getX snapNormal)
                                    , \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getY snapNormal)
                                    , \_ -> Expect.within (Expect.Absolute 0.001) 1 (Vec3.getZ snapNormal)
                                    , \_ -> Expect.within (Expect.Absolute 0.001) 1 (Vec3.getX snapTangent)
                                    , \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getY snapTangent)
                                    , \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getZ snapTangent)
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "Plate onto bracket side stud snap candidate not found"

                    _ ->
                        Expect.fail "3023.dat or 99781.dat not found in catalog"
        , test "wheel rim 4624 snaps onto wheel holder 4600 pin and tire 3641 snaps onto rim 4624" <|
            \_ ->
                case ( knownCatalogPart "4600.dat", knownCatalogPart "4624.dat", knownCatalogPart "3641.dat" ) of
                    ( Just holder, Just rim, Just tire ) ->
                        let
                            holderMatrix =
                                Mat4.identity

                            -- Holder pin 50 is at (28, 5, 0)
                            holderPins =
                                holder.connections
                                    |> List.filter (\c -> Vec3.getX c.localPosition > 20)
                                    |> List.map (Connectivity.transformPoint holderMatrix 1)

                            hoverRim =
                                Mat4.makeTranslate3 35 5 0

                            maybeRimSnap =
                                solveSnap 20.0 0 hoverRim rim.connections holderPins
                        in
                        case maybeRimSnap of
                            Just rimSnap ->
                                let
                                    rimWorldPoints =
                                        rim.connections
                                            |> List.map (Connectivity.transformPoint rimSnap.snapMatrix 2)

                                    hoverTire =
                                        Mat4.makeTranslate3 35 5 5

                                    maybeTireSnap =
                                        solveSnap 20.0 0 hoverTire tire.connections rimWorldPoints
                                in
                                case maybeTireSnap of
                                    Just tireSnap ->
                                        let
                                            rimPos =
                                                Mat4.transform rimSnap.snapMatrix (vec3 0 0 0)

                                            tirePos =
                                                Mat4.transform tireSnap.snapMatrix (vec3 0 0 0)

                                            holderBoxes =
                                                holder.boundingBoxes
                                                    |> List.map (\b -> ( 1, holderMatrix, b ))

                                            rimBoxes =
                                                rim.boundingBoxes
                                                    |> List.map (\b -> ( 2, rimSnap.snapMatrix, b ))

                                            tireBoxes =
                                                tire.boundingBoxes
                                                    |> List.map (\b -> ( 3, tireSnap.snapMatrix, b ))
                                        in
                                        Expect.all
                                            [ \_ -> Expect.within (Expect.Absolute 0.001) 33.0 (Vec3.getX rimPos)
                                            , \_ -> Expect.within (Expect.Absolute 0.001) 5.0 (Vec3.getY rimPos)
                                            , \_ -> Expect.within (Expect.Absolute 0.001) (Vec3.getX rimPos) (Vec3.getX tirePos)
                                            , \_ -> Expect.within (Expect.Absolute 0.001) (Vec3.getY rimPos) (Vec3.getY tirePos)
                                            , \_ -> Expect.within (Expect.Absolute 0.001) (Vec3.getZ rimPos) (Vec3.getZ tirePos)
                                            , \_ -> Expect.equal [] (checkCollisions 0.5 rimBoxes holderBoxes)
                                            , \_ -> Expect.equal [] (checkCollisions 0.5 tireBoxes holderBoxes)
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail "Tire 3641 failed to snap onto rim 4624"

                            Nothing ->
                                Expect.fail "Wheel rim 4624 failed to snap onto holder 4600"

                    _ ->
                        Expect.fail "Vehicle parts not found in catalog"
        , test "Technic wheel 55982 snaps onto axle 3705 and tire 56891 snaps onto 55982 without collisions" <|
            \_ ->
                case ( knownCatalogPart "3705.dat", knownCatalogPart "55982.dat", knownCatalogPart "56891.dat" ) of
                    ( Just axle, Just wheel, Just tire ) ->
                        let
                            axleMatrix =
                                Mat4.identity

                            axlePoints =
                                axle.connections
                                    |> List.map (Connectivity.transformPoint axleMatrix 1)

                            hoverWheel =
                                Mat4.makeTranslate3 0 0 10

                            maybeWheelSnap =
                                solveSnap 20.0 0 hoverWheel wheel.connections axlePoints
                        in
                        case maybeWheelSnap of
                            Just wheelSnap ->
                                let
                                    wheelWorldPoints =
                                        wheel.connections
                                            |> List.map (Connectivity.transformPoint wheelSnap.snapMatrix 2)

                                    wheelPos =
                                        Mat4.transform wheelSnap.snapMatrix (vec3 0 0 0)

                                    hoverTire =
                                        Mat4.makeTranslate3 (Vec3.getX wheelPos + 5) (Vec3.getY wheelPos) (Vec3.getZ wheelPos)

                                    maybeTireSnap =
                                        solveSnap 20.0 0 hoverTire tire.connections wheelWorldPoints
                                in
                                case maybeTireSnap of
                                    Just tireSnap ->
                                        let
                                            tirePos =
                                                Mat4.transform tireSnap.snapMatrix (vec3 0 0 0)

                                            axleBoxes =
                                                axle.boundingBoxes
                                                    |> List.map (\b -> ( 1, axleMatrix, b ))

                                            tireBoxes =
                                                tire.boundingBoxes
                                                    |> List.map (\b -> ( 3, tireSnap.snapMatrix, b ))
                                        in
                                        Expect.all
                                            [ \_ -> Expect.within (Expect.Absolute 0.001) (Vec3.getX wheelPos) (Vec3.getX tirePos)
                                            , \_ -> Expect.within (Expect.Absolute 0.001) (Vec3.getY wheelPos) (Vec3.getY tirePos)
                                            , \_ -> Expect.within (Expect.Absolute 0.001) (Vec3.getZ wheelPos) (Vec3.getZ tirePos)
                                            , \_ -> Expect.equal [] (checkCollisions 0.5 tireBoxes axleBoxes)
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail "Tire 56891 failed to snap onto wheel 55982"

                            Nothing ->
                                Expect.fail "Wheel 55982 failed to snap onto axle 3705"

                    _ ->
                        Expect.fail "Axle, wheel or tire not found in catalog"
        ]



-- ── Microscale, slopes and every other visible curated part ─────────────────


type alias Triple =
    ( Float, Float, Float )


type alias Layout =
    { studs : List Triple
    , antiStuds : List Triple
    , others : List ( String, Triple, Triple )
    , boxes : List ( Triple, Triple )
    }


{-| Rounded to 0.01 LDU so measured fractional boxes compare exactly.
-}
roundTriple : Vec3 -> Triple
roundTriple v =
    let
        r x =
            toFloat (round (x * 100)) / 100
    in
    ( r (Vec3.getX v), r (Vec3.getY v), r (Vec3.getZ v) )


connKind : ConnectionType -> String
connKind connType =
    case connType of
        Stud ->
            "stud"

        AntiStud ->
            "antiStud"

        TechnicPinMale _ ->
            "pin"

        TechnicPinFemale _ ->
            "pinHole"

        TechnicAxleMale _ ->
            "axle"

        TechnicAxleFemale _ ->
            "axleHole"

        ClipMale _ ->
            "clip"

        BarFemale _ ->
            "bar"

        _ ->
            "other"


{-| Sorted rounded positions of a part's studs, anti-studs and other points, and its boxes in order.
-}
layoutOf : Connectivity.PartConnectivity -> Layout
layoutOf conn =
    let
        positionsOf connType =
            conn.connections
                |> List.filter (\c -> c.connType == connType)
                |> List.map (.localPosition >> roundTriple)
                |> List.sort
    in
    { studs = positionsOf Stud
    , antiStuds = positionsOf AntiStud
    , others =
        conn.connections
            |> List.filter (\c -> c.connType /= Stud && c.connType /= AntiStud)
            |> List.map (\c -> ( connKind c.connType, roundTriple c.localPosition, roundTriple c.normal ))
            |> List.sort
    , boxes = List.map (\b -> ( roundTriple b.min, roundTriple b.max )) conn.boundingBoxes
    }


{-| Expected layouts, typed from the measured plan (LDraw kulits tree), not from the helpers.
-}
expectedLayouts : List ( String, Layout )
expectedLayouts =
    [ ( "3941.dat", { studs = [ ( -10, 0, -10 ), ( -10, 0, 10 ), ( 10, 0, -10 ), ( 10, 0, 10 ) ], antiStuds = [ ( -10, 24, -10 ), ( -10, 24, 10 ), ( 10, 24, -10 ), ( 10, 24, 10 ) ], others = [], boxes = [ ( ( -20, -4, -20 ), ( 20, 24, 20 ) ) ] } )
    , ( "4032b.dat", { studs = [ ( -10, 0, -10 ), ( -10, 0, 10 ), ( 10, 0, -10 ), ( 10, 0, 10 ) ], antiStuds = [ ( -10, 8, -10 ), ( -10, 8, 10 ), ( 10, 8, -10 ), ( 10, 8, 10 ) ], others = [], boxes = [ ( ( -20, -4, -20 ), ( 20, 8, 20 ) ) ] } )
    , ( "63864.dat", { studs = [], antiStuds = [ ( -20, 8, 0 ), ( 0, 8, 0 ), ( 20, 8, 0 ) ], others = [], boxes = [ ( ( -30, 0, -10 ), ( 30, 8, 10 ) ) ] } )
    , ( "60481a.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 48, -20 ), ( 0, 48, 0 ) ], others = [], boxes = [ ( ( -10, -4, -30 ), ( 10, 48, 10 ) ) ] } )
    , ( "35464.dat", { studs = [], antiStuds = [ ( 0, 0, 0 ) ], others = [], boxes = [ ( ( -10, -16, -10 ), ( 10, 0, 10 ) ) ] } )
    , ( "85984.dat", { studs = [], antiStuds = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], others = [], boxes = [ ( ( -20, -16, -10 ), ( 20, 0, 10 ) ) ] } )
    , ( "11477.dat", { studs = [], antiStuds = [ ( 0, 0, -10 ) ], others = [], boxes = [ ( ( -10, -16, -20 ), ( 10, 0, 20 ) ) ] } )
    , ( "15068.dat", { studs = [], antiStuds = [ ( -10, 0, -10 ), ( 10, 0, -10 ) ], others = [], boxes = [ ( ( -20, -16, -20 ), ( 20, 0, 20 ) ) ] } )
    , ( "93273.dat", { studs = [], antiStuds = [ ( 0, 0, -30 ), ( 0, 0, 30 ) ], others = [], boxes = [ ( ( -10, -16, -40 ), ( 10, 0, 40 ) ) ] } )
    , ( "50950.dat", { studs = [], antiStuds = [ ( 0, 24, -20 ), ( 0, 24, 0 ) ], others = [], boxes = [ ( ( -10, 0, -30 ), ( 10, 24, 30 ) ) ] } )
    , ( "61678.dat", { studs = [], antiStuds = [ ( 0, 24, -30 ), ( 0, 24, -10 ) ], others = [], boxes = [ ( ( -10, 0, -40 ), ( 10, 24, 40 ) ) ] } )
    , ( "3665b.dat", { studs = [ ( 0, 0, -20 ), ( 0, 0, 0 ) ], antiStuds = [ ( 0, 24, 0 ) ], others = [], boxes = [ ( ( -10, -4, -30 ), ( 10, 24, 10 ) ) ] } )
    , ( "3660b.dat", { studs = [ ( -10, 0, -20 ), ( -10, 0, 0 ), ( 10, 0, -20 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 24, 0 ), ( 10, 24, 0 ) ], others = [], boxes = [ ( ( -20, -4, -30 ), ( 20, 24, 10 ) ) ] } )
    , ( "3747b.dat", { studs = [ ( -10, 0, -40 ), ( -10, 0, -20 ), ( -10, 0, 0 ), ( 10, 0, -40 ), ( 10, 0, -20 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 24, 0 ), ( 10, 24, 0 ) ], others = [], boxes = [ ( ( -20, -4, -50 ), ( 20, 24, 10 ) ) ] } )
    , ( "24201.dat", { studs = [ ( 0, 0, 0 ), ( 0, 8, 20 ) ], antiStuds = [], others = [], boxes = [ ( ( -10, -4, -10 ), ( 10, 16, 30 ) ) ] } )
    , ( "98283.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 24, 0 ), ( 10, 24, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 24, 10 ) ) ] } )
    , ( "2877.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 24, 0 ), ( 10, 24, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 24, 10 ) ) ] } )
    , ( "30136.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 24, 0 ), ( 10, 24, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 24, 10 ) ) ] } )
    , ( "4490.dat", { studs = [ ( -20, 0, 0 ), ( 0, 0, 0 ), ( 20, 0, 0 ) ], antiStuds = [ ( -20, 24, 0 ), ( 20, 24, 0 ) ], others = [], boxes = [ ( ( -30, -4, -10 ), ( 30, 24, 10 ) ) ] } )
    , ( "3659.dat", { studs = [ ( -30, 0, 0 ), ( -10, 0, 0 ), ( 10, 0, 0 ), ( 30, 0, 0 ) ], antiStuds = [ ( -30, 24, 0 ), ( 30, 24, 0 ) ], others = [], boxes = [ ( ( -40, -4, -10 ), ( 40, 24, 10 ) ) ] } )
    , ( "87552.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 48, 0 ), ( 10, 48, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 48, 10 ) ) ] } )
    , ( "4865b.dat", { studs = [], antiStuds = [ ( -10, 24, 0 ), ( 10, 24, 0 ) ], others = [], boxes = [ ( ( -20, 0, -10 ), ( 20, 24, 10 ) ) ] } )
    , ( "3633.dat", { studs = [], antiStuds = [ ( -30, 24, 0 ), ( -10, 24, 0 ), ( 10, 24, 0 ), ( 30, 24, 0 ) ], others = [], boxes = [ ( ( -40, 0, -10 ), ( 40, 24, 10 ) ) ] } )
    , ( "4589.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 24, 0 ) ], others = [], boxes = [ ( ( -10, -4, -10 ), ( 10, 24, 10 ) ) ] } )
    , ( "3942c.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( -10, 48, -10 ), ( -10, 48, 10 ), ( 10, 48, -10 ), ( 10, 48, 10 ) ], others = [], boxes = [ ( ( -20, -4, -20 ), ( 20, 48, 20 ) ) ] } )
    , ( "4740.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 8, 0 ) ], others = [], boxes = [ ( ( -20, -4, -20 ), ( 20, 8, 20 ) ) ] } )
    , ( "43888.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 144, 0 ) ], others = [], boxes = [ ( ( -10, -4, -10 ), ( 10, 144, 10 ) ) ] } )
    , ( "87994.dat", { studs = [], antiStuds = [], others = [ ( "bar", ( 0, 4, 0 ), ( 0, 1, 0 ) ), ( "bar", ( 0, 10, 0 ), ( 0, 1, 0 ) ), ( "bar", ( 0, 30, 0 ), ( 0, 1, 0 ) ), ( "bar", ( 0, 50, 0 ), ( 0, 1, 0 ) ), ( "bar", ( 0, 56, 0 ), ( 0, 1, 0 ) ) ], boxes = [ ( ( -4, 0, -4 ), ( 4, 60, 4 ) ) ] } )
    , ( "30374.dat", { studs = [], antiStuds = [], others = [ ( "bar", ( 0, 4, 0 ), ( 0, 1, 0 ) ), ( "bar", ( 0, 10, 0 ), ( 0, 1, 0 ) ), ( "bar", ( 0, 30, 0 ), ( 0, 1, 0 ) ), ( "bar", ( 0, 50, 0 ), ( 0, 1, 0 ) ), ( "bar", ( 0, 70, 0 ), ( 0, 1, 0 ) ), ( "bar", ( 0, 76, 0 ), ( 0, 1, 0 ) ) ], boxes = [ ( ( -4, 0, -4 ), ( 4, 80, 4 ) ) ] } )
    , ( "3957a.dat", { studs = [], antiStuds = [ ( 0, 8, 0 ) ], others = [], boxes = [ ( ( -8, -88, -8 ), ( 8, 8, 8 ) ) ] } )
    , ( "61252.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 8, 0 ) ], others = [ ( "clip", ( 0, 2, -20 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -10, -4, -10 ), ( 10, 8, 10 ) ) ] } )
    , ( "33291.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 8, 0 ) ], others = [], boxes = [ ( ( -10, -4, -10 ), ( 10, 8, 10 ) ) ] } )
    , ( "24866.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 8, 0 ) ], others = [], boxes = [ ( ( -10, -4, -10 ), ( 10, 8, 10 ) ) ] } )
    , ( "4081b.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 8, 0 ) ], others = [ ( "clip", ( 0, 2, -20 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -10, -4, -10 ), ( 10, 8, 10 ) ) ] } )
    , ( "4085.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 8, 0 ) ], others = [ ( "clip", ( 0, 4, -20 ), ( 0, -1, 0 ) ) ], boxes = [ ( ( -10, -4, -10 ), ( 10, 8, 10 ) ) ] } )
    , ( "4085c.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 8, 0 ) ], others = [ ( "clip", ( 0, 4, -20 ), ( 0, -1, 0 ) ) ], boxes = [ ( ( -10, -4, -10 ), ( 10, 8, 10 ) ) ] } )
    , ( "15070.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 8, 0 ) ], others = [], boxes = [ ( ( -10, -4, -10 ), ( 10, 8, 10 ) ), ( ( -10, 0, -18 ), ( 10, 24, -10 ) ) ] } )
    , ( "32607.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 8, 0 ) ], others = [], boxes = [ ( ( -10, -4, -26 ), ( 28, 8, 10 ) ) ] } )
    , ( "2423.dat", { studs = [ ( -20, 0, -40 ), ( -20, 0, -20 ), ( 0, 0, -60 ), ( 0, 0, 0 ), ( 20, 0, -40 ), ( 20, 0, -20 ) ], antiStuds = [ ( 0, 8, -60 ), ( 0, 8, 0 ) ], others = [], boxes = [ ( ( -26, -4, -68 ), ( 26, 8, 10 ) ) ] } )
    , ( "3032.dat", { studs = [ ( -50, 0, -30 ), ( -50, 0, -10 ), ( -50, 0, 10 ), ( -50, 0, 30 ), ( -30, 0, -30 ), ( -30, 0, -10 ), ( -30, 0, 10 ), ( -30, 0, 30 ), ( -10, 0, -30 ), ( -10, 0, -10 ), ( -10, 0, 10 ), ( -10, 0, 30 ), ( 10, 0, -30 ), ( 10, 0, -10 ), ( 10, 0, 10 ), ( 10, 0, 30 ), ( 30, 0, -30 ), ( 30, 0, -10 ), ( 30, 0, 10 ), ( 30, 0, 30 ), ( 50, 0, -30 ), ( 50, 0, -10 ), ( 50, 0, 10 ), ( 50, 0, 30 ) ], antiStuds = [ ( -50, 8, -30 ), ( -50, 8, -10 ), ( -50, 8, 10 ), ( -50, 8, 30 ), ( -30, 8, -30 ), ( -30, 8, -10 ), ( -30, 8, 10 ), ( -30, 8, 30 ), ( -10, 8, -30 ), ( -10, 8, -10 ), ( -10, 8, 10 ), ( -10, 8, 30 ), ( 10, 8, -30 ), ( 10, 8, -10 ), ( 10, 8, 10 ), ( 10, 8, 30 ), ( 30, 8, -30 ), ( 30, 8, -10 ), ( 30, 8, 10 ), ( 30, 8, 30 ), ( 50, 8, -30 ), ( 50, 8, -10 ), ( 50, 8, 10 ), ( 50, 8, 30 ) ], others = [], boxes = [ ( ( -60, -4, -40 ), ( 60, 8, 40 ) ) ] } )
    , ( "3035.dat", { studs = [ ( -70, 0, -30 ), ( -70, 0, -10 ), ( -70, 0, 10 ), ( -70, 0, 30 ), ( -50, 0, -30 ), ( -50, 0, -10 ), ( -50, 0, 10 ), ( -50, 0, 30 ), ( -30, 0, -30 ), ( -30, 0, -10 ), ( -30, 0, 10 ), ( -30, 0, 30 ), ( -10, 0, -30 ), ( -10, 0, -10 ), ( -10, 0, 10 ), ( -10, 0, 30 ), ( 10, 0, -30 ), ( 10, 0, -10 ), ( 10, 0, 10 ), ( 10, 0, 30 ), ( 30, 0, -30 ), ( 30, 0, -10 ), ( 30, 0, 10 ), ( 30, 0, 30 ), ( 50, 0, -30 ), ( 50, 0, -10 ), ( 50, 0, 10 ), ( 50, 0, 30 ), ( 70, 0, -30 ), ( 70, 0, -10 ), ( 70, 0, 10 ), ( 70, 0, 30 ) ], antiStuds = [ ( -70, 8, -30 ), ( -70, 8, -10 ), ( -70, 8, 10 ), ( -70, 8, 30 ), ( -50, 8, -30 ), ( -50, 8, -10 ), ( -50, 8, 10 ), ( -50, 8, 30 ), ( -30, 8, -30 ), ( -30, 8, -10 ), ( -30, 8, 10 ), ( -30, 8, 30 ), ( -10, 8, -30 ), ( -10, 8, -10 ), ( -10, 8, 10 ), ( -10, 8, 30 ), ( 10, 8, -30 ), ( 10, 8, -10 ), ( 10, 8, 10 ), ( 10, 8, 30 ), ( 30, 8, -30 ), ( 30, 8, -10 ), ( 30, 8, 10 ), ( 30, 8, 30 ), ( 50, 8, -30 ), ( 50, 8, -10 ), ( 50, 8, 10 ), ( 50, 8, 30 ), ( 70, 8, -30 ), ( 70, 8, -10 ), ( 70, 8, 10 ), ( 70, 8, 30 ) ], others = [], boxes = [ ( ( -80, -4, -40 ), ( 80, 8, 40 ) ) ] } )
    , ( "3036.dat", { studs = [ ( -70, 0, -50 ), ( -70, 0, -30 ), ( -70, 0, -10 ), ( -70, 0, 10 ), ( -70, 0, 30 ), ( -70, 0, 50 ), ( -50, 0, -50 ), ( -50, 0, -30 ), ( -50, 0, -10 ), ( -50, 0, 10 ), ( -50, 0, 30 ), ( -50, 0, 50 ), ( -30, 0, -50 ), ( -30, 0, -30 ), ( -30, 0, -10 ), ( -30, 0, 10 ), ( -30, 0, 30 ), ( -30, 0, 50 ), ( -10, 0, -50 ), ( -10, 0, -30 ), ( -10, 0, -10 ), ( -10, 0, 10 ), ( -10, 0, 30 ), ( -10, 0, 50 ), ( 10, 0, -50 ), ( 10, 0, -30 ), ( 10, 0, -10 ), ( 10, 0, 10 ), ( 10, 0, 30 ), ( 10, 0, 50 ), ( 30, 0, -50 ), ( 30, 0, -30 ), ( 30, 0, -10 ), ( 30, 0, 10 ), ( 30, 0, 30 ), ( 30, 0, 50 ), ( 50, 0, -50 ), ( 50, 0, -30 ), ( 50, 0, -10 ), ( 50, 0, 10 ), ( 50, 0, 30 ), ( 50, 0, 50 ), ( 70, 0, -50 ), ( 70, 0, -30 ), ( 70, 0, -10 ), ( 70, 0, 10 ), ( 70, 0, 30 ), ( 70, 0, 50 ) ], antiStuds = [ ( -70, 8, -50 ), ( -70, 8, -30 ), ( -70, 8, -10 ), ( -70, 8, 10 ), ( -70, 8, 30 ), ( -70, 8, 50 ), ( -50, 8, -50 ), ( -50, 8, -30 ), ( -50, 8, -10 ), ( -50, 8, 10 ), ( -50, 8, 30 ), ( -50, 8, 50 ), ( -30, 8, -50 ), ( -30, 8, -30 ), ( -30, 8, -10 ), ( -30, 8, 10 ), ( -30, 8, 30 ), ( -30, 8, 50 ), ( -10, 8, -50 ), ( -10, 8, -30 ), ( -10, 8, -10 ), ( -10, 8, 10 ), ( -10, 8, 30 ), ( -10, 8, 50 ), ( 10, 8, -50 ), ( 10, 8, -30 ), ( 10, 8, -10 ), ( 10, 8, 10 ), ( 10, 8, 30 ), ( 10, 8, 50 ), ( 30, 8, -50 ), ( 30, 8, -30 ), ( 30, 8, -10 ), ( 30, 8, 10 ), ( 30, 8, 30 ), ( 30, 8, 50 ), ( 50, 8, -50 ), ( 50, 8, -30 ), ( 50, 8, -10 ), ( 50, 8, 10 ), ( 50, 8, 30 ), ( 50, 8, 50 ), ( 70, 8, -50 ), ( 70, 8, -30 ), ( 70, 8, -10 ), ( 70, 8, 10 ), ( 70, 8, 30 ), ( 70, 8, 50 ) ], others = [], boxes = [ ( ( -80, -4, -60 ), ( 80, 8, 60 ) ) ] } )
    , ( "3030.dat", { studs = [ ( -90, 0, -30 ), ( -90, 0, -10 ), ( -90, 0, 10 ), ( -90, 0, 30 ), ( -70, 0, -30 ), ( -70, 0, -10 ), ( -70, 0, 10 ), ( -70, 0, 30 ), ( -50, 0, -30 ), ( -50, 0, -10 ), ( -50, 0, 10 ), ( -50, 0, 30 ), ( -30, 0, -30 ), ( -30, 0, -10 ), ( -30, 0, 10 ), ( -30, 0, 30 ), ( -10, 0, -30 ), ( -10, 0, -10 ), ( -10, 0, 10 ), ( -10, 0, 30 ), ( 10, 0, -30 ), ( 10, 0, -10 ), ( 10, 0, 10 ), ( 10, 0, 30 ), ( 30, 0, -30 ), ( 30, 0, -10 ), ( 30, 0, 10 ), ( 30, 0, 30 ), ( 50, 0, -30 ), ( 50, 0, -10 ), ( 50, 0, 10 ), ( 50, 0, 30 ), ( 70, 0, -30 ), ( 70, 0, -10 ), ( 70, 0, 10 ), ( 70, 0, 30 ), ( 90, 0, -30 ), ( 90, 0, -10 ), ( 90, 0, 10 ), ( 90, 0, 30 ) ], antiStuds = [ ( -90, 8, -30 ), ( -90, 8, -10 ), ( -90, 8, 10 ), ( -90, 8, 30 ), ( -70, 8, -30 ), ( -70, 8, -10 ), ( -70, 8, 10 ), ( -70, 8, 30 ), ( -50, 8, -30 ), ( -50, 8, -10 ), ( -50, 8, 10 ), ( -50, 8, 30 ), ( -30, 8, -30 ), ( -30, 8, -10 ), ( -30, 8, 10 ), ( -30, 8, 30 ), ( -10, 8, -30 ), ( -10, 8, -10 ), ( -10, 8, 10 ), ( -10, 8, 30 ), ( 10, 8, -30 ), ( 10, 8, -10 ), ( 10, 8, 10 ), ( 10, 8, 30 ), ( 30, 8, -30 ), ( 30, 8, -10 ), ( 30, 8, 10 ), ( 30, 8, 30 ), ( 50, 8, -30 ), ( 50, 8, -10 ), ( 50, 8, 10 ), ( 50, 8, 30 ), ( 70, 8, -30 ), ( 70, 8, -10 ), ( 70, 8, 10 ), ( 70, 8, 30 ), ( 90, 8, -30 ), ( 90, 8, -10 ), ( 90, 8, 10 ), ( 90, 8, 30 ) ], others = [], boxes = [ ( ( -100, -4, -40 ), ( 100, 8, 40 ) ) ] } )
    , ( "3029.dat", { studs = [ ( -110, 0, -30 ), ( -110, 0, -10 ), ( -110, 0, 10 ), ( -110, 0, 30 ), ( -90, 0, -30 ), ( -90, 0, -10 ), ( -90, 0, 10 ), ( -90, 0, 30 ), ( -70, 0, -30 ), ( -70, 0, -10 ), ( -70, 0, 10 ), ( -70, 0, 30 ), ( -50, 0, -30 ), ( -50, 0, -10 ), ( -50, 0, 10 ), ( -50, 0, 30 ), ( -30, 0, -30 ), ( -30, 0, -10 ), ( -30, 0, 10 ), ( -30, 0, 30 ), ( -10, 0, -30 ), ( -10, 0, -10 ), ( -10, 0, 10 ), ( -10, 0, 30 ), ( 10, 0, -30 ), ( 10, 0, -10 ), ( 10, 0, 10 ), ( 10, 0, 30 ), ( 30, 0, -30 ), ( 30, 0, -10 ), ( 30, 0, 10 ), ( 30, 0, 30 ), ( 50, 0, -30 ), ( 50, 0, -10 ), ( 50, 0, 10 ), ( 50, 0, 30 ), ( 70, 0, -30 ), ( 70, 0, -10 ), ( 70, 0, 10 ), ( 70, 0, 30 ), ( 90, 0, -30 ), ( 90, 0, -10 ), ( 90, 0, 10 ), ( 90, 0, 30 ), ( 110, 0, -30 ), ( 110, 0, -10 ), ( 110, 0, 10 ), ( 110, 0, 30 ) ], antiStuds = [ ( -110, 8, -30 ), ( -110, 8, -10 ), ( -110, 8, 10 ), ( -110, 8, 30 ), ( -90, 8, -30 ), ( -90, 8, -10 ), ( -90, 8, 10 ), ( -90, 8, 30 ), ( -70, 8, -30 ), ( -70, 8, -10 ), ( -70, 8, 10 ), ( -70, 8, 30 ), ( -50, 8, -30 ), ( -50, 8, -10 ), ( -50, 8, 10 ), ( -50, 8, 30 ), ( -30, 8, -30 ), ( -30, 8, -10 ), ( -30, 8, 10 ), ( -30, 8, 30 ), ( -10, 8, -30 ), ( -10, 8, -10 ), ( -10, 8, 10 ), ( -10, 8, 30 ), ( 10, 8, -30 ), ( 10, 8, -10 ), ( 10, 8, 10 ), ( 10, 8, 30 ), ( 30, 8, -30 ), ( 30, 8, -10 ), ( 30, 8, 10 ), ( 30, 8, 30 ), ( 50, 8, -30 ), ( 50, 8, -10 ), ( 50, 8, 10 ), ( 50, 8, 30 ), ( 70, 8, -30 ), ( 70, 8, -10 ), ( 70, 8, 10 ), ( 70, 8, 30 ), ( 90, 8, -30 ), ( 90, 8, -10 ), ( 90, 8, 10 ), ( 90, 8, 30 ), ( 110, 8, -30 ), ( 110, 8, -10 ), ( 110, 8, 10 ), ( 110, 8, 30 ) ], others = [], boxes = [ ( ( -120, -4, -40 ), ( 120, 8, 40 ) ) ] } )
    , ( "3028.dat", { studs = [ ( -110, 0, -50 ), ( -110, 0, -30 ), ( -110, 0, -10 ), ( -110, 0, 10 ), ( -110, 0, 30 ), ( -110, 0, 50 ), ( -90, 0, -50 ), ( -90, 0, -30 ), ( -90, 0, -10 ), ( -90, 0, 10 ), ( -90, 0, 30 ), ( -90, 0, 50 ), ( -70, 0, -50 ), ( -70, 0, -30 ), ( -70, 0, -10 ), ( -70, 0, 10 ), ( -70, 0, 30 ), ( -70, 0, 50 ), ( -50, 0, -50 ), ( -50, 0, -30 ), ( -50, 0, -10 ), ( -50, 0, 10 ), ( -50, 0, 30 ), ( -50, 0, 50 ), ( -30, 0, -50 ), ( -30, 0, -30 ), ( -30, 0, -10 ), ( -30, 0, 10 ), ( -30, 0, 30 ), ( -30, 0, 50 ), ( -10, 0, -50 ), ( -10, 0, -30 ), ( -10, 0, -10 ), ( -10, 0, 10 ), ( -10, 0, 30 ), ( -10, 0, 50 ), ( 10, 0, -50 ), ( 10, 0, -30 ), ( 10, 0, -10 ), ( 10, 0, 10 ), ( 10, 0, 30 ), ( 10, 0, 50 ), ( 30, 0, -50 ), ( 30, 0, -30 ), ( 30, 0, -10 ), ( 30, 0, 10 ), ( 30, 0, 30 ), ( 30, 0, 50 ), ( 50, 0, -50 ), ( 50, 0, -30 ), ( 50, 0, -10 ), ( 50, 0, 10 ), ( 50, 0, 30 ), ( 50, 0, 50 ), ( 70, 0, -50 ), ( 70, 0, -30 ), ( 70, 0, -10 ), ( 70, 0, 10 ), ( 70, 0, 30 ), ( 70, 0, 50 ), ( 90, 0, -50 ), ( 90, 0, -30 ), ( 90, 0, -10 ), ( 90, 0, 10 ), ( 90, 0, 30 ), ( 90, 0, 50 ), ( 110, 0, -50 ), ( 110, 0, -30 ), ( 110, 0, -10 ), ( 110, 0, 10 ), ( 110, 0, 30 ), ( 110, 0, 50 ) ], antiStuds = [ ( -110, 8, -50 ), ( -110, 8, -30 ), ( -110, 8, -10 ), ( -110, 8, 10 ), ( -110, 8, 30 ), ( -110, 8, 50 ), ( -90, 8, -50 ), ( -90, 8, -30 ), ( -90, 8, -10 ), ( -90, 8, 10 ), ( -90, 8, 30 ), ( -90, 8, 50 ), ( -70, 8, -50 ), ( -70, 8, -30 ), ( -70, 8, -10 ), ( -70, 8, 10 ), ( -70, 8, 30 ), ( -70, 8, 50 ), ( -50, 8, -50 ), ( -50, 8, -30 ), ( -50, 8, -10 ), ( -50, 8, 10 ), ( -50, 8, 30 ), ( -50, 8, 50 ), ( -30, 8, -50 ), ( -30, 8, -30 ), ( -30, 8, -10 ), ( -30, 8, 10 ), ( -30, 8, 30 ), ( -30, 8, 50 ), ( -10, 8, -50 ), ( -10, 8, -30 ), ( -10, 8, -10 ), ( -10, 8, 10 ), ( -10, 8, 30 ), ( -10, 8, 50 ), ( 10, 8, -50 ), ( 10, 8, -30 ), ( 10, 8, -10 ), ( 10, 8, 10 ), ( 10, 8, 30 ), ( 10, 8, 50 ), ( 30, 8, -50 ), ( 30, 8, -30 ), ( 30, 8, -10 ), ( 30, 8, 10 ), ( 30, 8, 30 ), ( 30, 8, 50 ), ( 50, 8, -50 ), ( 50, 8, -30 ), ( 50, 8, -10 ), ( 50, 8, 10 ), ( 50, 8, 30 ), ( 50, 8, 50 ), ( 70, 8, -50 ), ( 70, 8, -30 ), ( 70, 8, -10 ), ( 70, 8, 10 ), ( 70, 8, 30 ), ( 70, 8, 50 ), ( 90, 8, -50 ), ( 90, 8, -30 ), ( 90, 8, -10 ), ( 90, 8, 10 ), ( 90, 8, 30 ), ( 90, 8, 50 ), ( 110, 8, -50 ), ( 110, 8, -30 ), ( 110, 8, -10 ), ( 110, 8, 10 ), ( 110, 8, 30 ), ( 110, 8, 50 ) ], others = [], boxes = [ ( ( -120, -4, -60 ), ( 120, 8, 60 ) ) ] } )
    , ( "3027.dat", { studs = [ ( -150, 0, -50 ), ( -150, 0, -30 ), ( -150, 0, -10 ), ( -150, 0, 10 ), ( -150, 0, 30 ), ( -150, 0, 50 ), ( -130, 0, -50 ), ( -130, 0, -30 ), ( -130, 0, -10 ), ( -130, 0, 10 ), ( -130, 0, 30 ), ( -130, 0, 50 ), ( -110, 0, -50 ), ( -110, 0, -30 ), ( -110, 0, -10 ), ( -110, 0, 10 ), ( -110, 0, 30 ), ( -110, 0, 50 ), ( -90, 0, -50 ), ( -90, 0, -30 ), ( -90, 0, -10 ), ( -90, 0, 10 ), ( -90, 0, 30 ), ( -90, 0, 50 ), ( -70, 0, -50 ), ( -70, 0, -30 ), ( -70, 0, -10 ), ( -70, 0, 10 ), ( -70, 0, 30 ), ( -70, 0, 50 ), ( -50, 0, -50 ), ( -50, 0, -30 ), ( -50, 0, -10 ), ( -50, 0, 10 ), ( -50, 0, 30 ), ( -50, 0, 50 ), ( -30, 0, -50 ), ( -30, 0, -30 ), ( -30, 0, -10 ), ( -30, 0, 10 ), ( -30, 0, 30 ), ( -30, 0, 50 ), ( -10, 0, -50 ), ( -10, 0, -30 ), ( -10, 0, -10 ), ( -10, 0, 10 ), ( -10, 0, 30 ), ( -10, 0, 50 ), ( 10, 0, -50 ), ( 10, 0, -30 ), ( 10, 0, -10 ), ( 10, 0, 10 ), ( 10, 0, 30 ), ( 10, 0, 50 ), ( 30, 0, -50 ), ( 30, 0, -30 ), ( 30, 0, -10 ), ( 30, 0, 10 ), ( 30, 0, 30 ), ( 30, 0, 50 ), ( 50, 0, -50 ), ( 50, 0, -30 ), ( 50, 0, -10 ), ( 50, 0, 10 ), ( 50, 0, 30 ), ( 50, 0, 50 ), ( 70, 0, -50 ), ( 70, 0, -30 ), ( 70, 0, -10 ), ( 70, 0, 10 ), ( 70, 0, 30 ), ( 70, 0, 50 ), ( 90, 0, -50 ), ( 90, 0, -30 ), ( 90, 0, -10 ), ( 90, 0, 10 ), ( 90, 0, 30 ), ( 90, 0, 50 ), ( 110, 0, -50 ), ( 110, 0, -30 ), ( 110, 0, -10 ), ( 110, 0, 10 ), ( 110, 0, 30 ), ( 110, 0, 50 ), ( 130, 0, -50 ), ( 130, 0, -30 ), ( 130, 0, -10 ), ( 130, 0, 10 ), ( 130, 0, 30 ), ( 130, 0, 50 ), ( 150, 0, -50 ), ( 150, 0, -30 ), ( 150, 0, -10 ), ( 150, 0, 10 ), ( 150, 0, 30 ), ( 150, 0, 50 ) ], antiStuds = [ ( -150, 8, -50 ), ( -150, 8, -30 ), ( -150, 8, -10 ), ( -150, 8, 10 ), ( -150, 8, 30 ), ( -150, 8, 50 ), ( -130, 8, -50 ), ( -130, 8, -30 ), ( -130, 8, -10 ), ( -130, 8, 10 ), ( -130, 8, 30 ), ( -130, 8, 50 ), ( -110, 8, -50 ), ( -110, 8, -30 ), ( -110, 8, -10 ), ( -110, 8, 10 ), ( -110, 8, 30 ), ( -110, 8, 50 ), ( -90, 8, -50 ), ( -90, 8, -30 ), ( -90, 8, -10 ), ( -90, 8, 10 ), ( -90, 8, 30 ), ( -90, 8, 50 ), ( -70, 8, -50 ), ( -70, 8, -30 ), ( -70, 8, -10 ), ( -70, 8, 10 ), ( -70, 8, 30 ), ( -70, 8, 50 ), ( -50, 8, -50 ), ( -50, 8, -30 ), ( -50, 8, -10 ), ( -50, 8, 10 ), ( -50, 8, 30 ), ( -50, 8, 50 ), ( -30, 8, -50 ), ( -30, 8, -30 ), ( -30, 8, -10 ), ( -30, 8, 10 ), ( -30, 8, 30 ), ( -30, 8, 50 ), ( -10, 8, -50 ), ( -10, 8, -30 ), ( -10, 8, -10 ), ( -10, 8, 10 ), ( -10, 8, 30 ), ( -10, 8, 50 ), ( 10, 8, -50 ), ( 10, 8, -30 ), ( 10, 8, -10 ), ( 10, 8, 10 ), ( 10, 8, 30 ), ( 10, 8, 50 ), ( 30, 8, -50 ), ( 30, 8, -30 ), ( 30, 8, -10 ), ( 30, 8, 10 ), ( 30, 8, 30 ), ( 30, 8, 50 ), ( 50, 8, -50 ), ( 50, 8, -30 ), ( 50, 8, -10 ), ( 50, 8, 10 ), ( 50, 8, 30 ), ( 50, 8, 50 ), ( 70, 8, -50 ), ( 70, 8, -30 ), ( 70, 8, -10 ), ( 70, 8, 10 ), ( 70, 8, 30 ), ( 70, 8, 50 ), ( 90, 8, -50 ), ( 90, 8, -30 ), ( 90, 8, -10 ), ( 90, 8, 10 ), ( 90, 8, 30 ), ( 90, 8, 50 ), ( 110, 8, -50 ), ( 110, 8, -30 ), ( 110, 8, -10 ), ( 110, 8, 10 ), ( 110, 8, 30 ), ( 110, 8, 50 ), ( 130, 8, -50 ), ( 130, 8, -30 ), ( 130, 8, -10 ), ( 130, 8, 10 ), ( 130, 8, 30 ), ( 130, 8, 50 ), ( 150, 8, -50 ), ( 150, 8, -30 ), ( 150, 8, -10 ), ( 150, 8, 10 ), ( 150, 8, 30 ), ( 150, 8, 50 ) ], others = [], boxes = [ ( ( -160, -4, -60 ), ( 160, 8, 60 ) ) ] } )
    , ( "4477.dat", { studs = [ ( -90, 0, 0 ), ( -70, 0, 0 ), ( -50, 0, 0 ), ( -30, 0, 0 ), ( -10, 0, 0 ), ( 10, 0, 0 ), ( 30, 0, 0 ), ( 50, 0, 0 ), ( 70, 0, 0 ), ( 90, 0, 0 ) ], antiStuds = [ ( -90, 8, 0 ), ( -70, 8, 0 ), ( -50, 8, 0 ), ( -30, 8, 0 ), ( -10, 8, 0 ), ( 10, 8, 0 ), ( 30, 8, 0 ), ( 50, 8, 0 ), ( 70, 8, 0 ), ( 90, 8, 0 ) ], others = [], boxes = [ ( ( -100, -4, -10 ), ( 100, 8, 10 ) ) ] } )
    , ( "3023bp00.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 8, 0 ), ( 10, 8, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 8, 10 ) ) ] } )
    , ( "63868.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 8, 0 ), ( 10, 8, 0 ) ], others = [ ( "clip", ( 30, 2, 0 ), ( 0, 0, 1 ) ) ], boxes = [ ( ( -20, -4, -10 ), ( 20, 8, 10 ) ) ] } )
    , ( "4162.dat", { studs = [], antiStuds = [ ( -70, 8, 0 ), ( -50, 8, 0 ), ( -30, 8, 0 ), ( -10, 8, 0 ), ( 10, 8, 0 ), ( 30, 8, 0 ), ( 50, 8, 0 ), ( 70, 8, 0 ) ], others = [], boxes = [ ( ( -80, 0, -10 ), ( 80, 8, 10 ) ) ] } )
    , ( "26603.dat", { studs = [], antiStuds = [ ( -20, 8, -10 ), ( -20, 8, 10 ), ( 0, 8, -10 ), ( 0, 8, 10 ), ( 20, 8, -10 ), ( 20, 8, 10 ) ], others = [], boxes = [ ( ( -30, 0, -20 ), ( 30, 8, 20 ) ) ] } )
    , ( "14769.dat", { studs = [], antiStuds = [ ( -10, 8, -10 ), ( -10, 8, 10 ), ( 10, 8, -10 ), ( 10, 8, 10 ) ], others = [], boxes = [ ( ( -20, 0, -20 ), ( 20, 8, 20 ) ) ] } )
    , ( "3069bp64.dat", { studs = [], antiStuds = [ ( -10, 8, 0 ), ( 10, 8, 0 ) ], others = [], boxes = [ ( ( -20, 0, -10 ), ( 20, 8, 10 ) ) ] } )
    , ( "2555.dat", { studs = [], antiStuds = [ ( 0, 8, 0 ) ], others = [ ( "clip", ( 0, -6, 0 ), ( 0, 0, 1 ) ) ], boxes = [ ( ( -10, 0, -10 ), ( 10, 8, 10 ) ) ] } )
    , ( "15712.dat", { studs = [], antiStuds = [ ( 0, 8, 0 ) ], others = [ ( "clip", ( 0, -6, 0 ), ( 0, 0, 1 ) ) ], boxes = [ ( ( -10, 0, -10 ), ( 10, 8, 10 ) ) ] } )
    , ( "14719.dat", { studs = [], antiStuds = [ ( 0, 8, 0 ), ( 0, 8, 20 ), ( 20, 8, 0 ) ], others = [], boxes = [ ( ( -10, 0, -10 ), ( 30, 8, 10 ) ), ( ( -10, 0, 10 ), ( 10, 8, 30 ) ) ] } )
    , ( "18674.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( -10, 8, -10 ), ( -10, 8, 10 ), ( 10, 8, -10 ), ( 10, 8, 10 ) ], others = [], boxes = [ ( ( -20, -4, -20 ), ( 20, 8, 20 ) ) ] } )
    , ( "3001b.dat", { studs = [ ( -30, 0, -10 ), ( -30, 0, 10 ), ( -10, 0, -10 ), ( -10, 0, 10 ), ( 10, 0, -10 ), ( 10, 0, 10 ), ( 30, 0, -10 ), ( 30, 0, 10 ) ], antiStuds = [ ( -30, 24, -10 ), ( -30, 24, 10 ), ( -10, 24, -10 ), ( -10, 24, 10 ), ( 10, 24, -10 ), ( 10, 24, 10 ), ( 30, 24, -10 ), ( 30, 24, 10 ) ], others = [], boxes = [ ( ( -40, -4, -20 ), ( 40, 24, 20 ) ) ] } )
    , ( "3245c.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 48, 0 ), ( 10, 48, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 48, 10 ) ) ] } )
    , ( "60593.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 72, 0 ), ( 10, 72, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 72, 10 ) ) ] } )
    , ( "59900.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 24, 0 ) ], others = [], boxes = [ ( ( -10, -4, -10 ), ( 10, 24, 10 ) ) ] } )
    , ( "4286.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 24, -40 ), ( 0, 24, -20 ), ( 0, 24, 0 ) ], others = [], boxes = [ ( ( -10, -4, -50 ), ( 10, 24, 10 ) ) ] } )
    , ( "4460b.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 72, -20 ), ( 0, 72, 0 ) ], others = [], boxes = [ ( ( -10, -4, -30 ), ( 10, 72, 10 ) ) ] } )
    , ( "3678b.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 48, -20 ), ( -10, 48, 0 ), ( 10, 48, -20 ), ( 10, 48, 0 ) ], others = [], boxes = [ ( ( -20, -4, -30 ), ( 20, 48, 10 ) ) ] } )
    , ( "3044b.dat", { studs = [], antiStuds = [ ( 0, 24, -10 ), ( 0, 24, 10 ) ], others = [], boxes = [ ( ( -10, 0, -20 ), ( 10, 24, 20 ) ) ] } )
    , ( "3048b.dat", { studs = [], antiStuds = [ ( -10, 24, 0 ), ( 10, 24, 0 ) ], others = [], boxes = [ ( ( -20, 0, -10 ), ( 20, 24, 10 ) ) ] } )
    , ( "3045.dat", { studs = [ ( 0, 0, 0 ) ], antiStuds = [ ( 0, 24, -20 ), ( 0, 24, 0 ), ( 20, 24, -20 ), ( 20, 24, 0 ) ], others = [], boxes = [ ( ( -10, -4, -30 ), ( 30, 24, 10 ) ) ] } )
    , ( "3046.dat", { studs = [ ( 0, 0, -20 ), ( 0, 0, 0 ), ( 20, 0, 0 ) ], antiStuds = [ ( 0, 24, -20 ), ( 0, 24, 0 ), ( 20, 24, -20 ), ( 20, 24, 0 ) ], others = [], boxes = [ ( ( -10, -4, -30 ), ( 30, 24, 10 ) ) ] } )
    , ( "30413.dat", { studs = [], antiStuds = [ ( -30, 24, 0 ), ( -10, 24, 0 ), ( 10, 24, 0 ), ( 30, 24, 0 ) ], others = [], boxes = [ ( ( -40, 0, -10 ), ( 40, 24, 10 ) ) ] } )
    , ( "3455.dat", { studs = [ ( -50, 0, 0 ), ( -30, 0, 0 ), ( -10, 0, 0 ), ( 10, 0, 0 ), ( 30, 0, 0 ), ( 50, 0, 0 ) ], antiStuds = [ ( -50, 24, 0 ), ( 50, 24, 0 ) ], others = [], boxes = [ ( ( -60, -4, -10 ), ( 60, 24, 10 ) ) ] } )
    , ( "6182.dat", { studs = [ ( -30, 0, 0 ), ( -10, 0, 0 ), ( 10, 0, 0 ), ( 30, 0, 0 ) ], antiStuds = [ ( -30, 48, 0 ), ( 30, 48, 0 ) ], others = [], boxes = [ ( ( -40, -4, -10 ), ( 40, 48, 10 ) ) ] } )
    , ( "2489.dat", { studs = [], antiStuds = [ ( -10, 40, -10 ), ( -10, 40, 10 ), ( 10, 40, -10 ), ( 10, 40, 10 ) ], others = [], boxes = [ ( ( -20, -4, -20 ), ( 20, 40, 20 ) ) ] } )
    , ( "35442.dat", { studs = [ ( -10, 0, -10 ), ( -10, 0, 10 ), ( 10, 0, -10 ), ( 10, 0, 10 ) ], antiStuds = [ ( -10, 8, -10 ), ( -10, 8, 10 ), ( 10, 8, -10 ), ( 10, 8, 10 ) ], others = [], boxes = [ ( ( -33.4, -4, -37.5 ), ( 33.4, 8, 37.5 ) ) ] } )
    , ( "51739.dat", { studs = [ ( -10, 0, -10 ), ( -10, 0, 10 ), ( 10, 0, -10 ), ( 10, 0, 10 ) ], antiStuds = [ ( -10, 8, -10 ), ( -10, 8, 10 ), ( 10, 8, -10 ), ( 10, 8, 10 ) ], others = [], boxes = [ ( ( -38.5, -4, -20 ), ( 38.5, 8, 20 ) ) ] } )
    , ( "61485.dat", { studs = [], antiStuds = [ ( -30, 8, -30 ), ( -30, 8, -10 ), ( -30, 8, 10 ), ( -30, 8, 30 ), ( -10, 8, -30 ), ( -10, 8, -10 ), ( -10, 8, 10 ), ( -10, 8, 30 ), ( 10, 8, -30 ), ( 10, 8, -10 ), ( 10, 8, 10 ), ( 10, 8, 30 ), ( 30, 8, -30 ), ( 30, 8, -10 ), ( 30, 8, 10 ), ( 30, 8, 30 ) ], others = [], boxes = [ ( ( -40, -4, -40 ), ( 40, 8, 40 ) ) ] } )
    , ( "3680.dat", { studs = [], antiStuds = [ ( -10, 8, -10 ), ( -10, 8, 10 ), ( 10, 8, -10 ), ( 10, 8, 10 ) ], others = [], boxes = [ ( ( -20, 0, -20 ), ( 20, 8, 20 ) ) ] } )
    , ( "2335.dat", { studs = [], antiStuds = [], others = [ ( "clip", ( 0, 4, 0 ), ( 0, 1, 0 ) ), ( "clip", ( 0, 36, 0 ), ( 0, 1, 0 ) ) ], boxes = [ ( ( -2, 0, 10 ), ( 2, 40, 50 ) ) ] } )
    , ( "30258.dat", { studs = [], antiStuds = [], others = [ ( "clip", ( 0, 0, 0 ), ( 0, 1, 0 ) ) ], boxes = [ ( ( -20, -20, -9 ), ( 20, 20, -6 ) ) ] } )
    , ( "6005.dat", { studs = [ ( 0, 8, 0 ) ], antiStuds = [ ( 0, 16, 0 ), ( 0, 48, -40 ) ], others = [], boxes = [ ( ( -10, 0, -50 ), ( 10, 48, -10 ) ), ( ( -10, 4, -10 ), ( 10, 16, 10 ) ) ] } )
    , ( "2654a.dat", { studs = [ ( -10, 0, -10 ), ( -10, 0, 10 ), ( 10, 0, -10 ), ( 10, 0, 10 ) ], antiStuds = [ ( 0, 8, 0 ) ], others = [], boxes = [ ( ( -20, -4, -20 ), ( 20, 8, 20 ) ) ] } )
    , ( "32028.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 8, 0 ), ( 10, 8, 0 ) ], others = [], boxes = [ ( ( -20, -4, -19 ), ( 20, 8, 10 ) ) ] } )
    , ( "92013.dat", { studs = [ ( -10, 0, -10 ), ( -10, 0, 10 ), ( 10, 0, -10 ), ( 10, 0, 10 ) ], antiStuds = [ ( -10, 24, -10 ), ( -10, 24, 10 ), ( 10, 24, -10 ), ( 10, 24, 10 ) ], others = [], boxes = [ ( ( -20, -4, -20 ), ( 20, 24, 20 ) ) ] } )
    , ( "4022.dat", { studs = [], antiStuds = [ ( -10, 8, -10 ), ( -10, 8, 10 ), ( 10, 8, -10 ), ( 10, 8, 10 ) ], others = [], boxes = [ ( ( -61, 0, -53 ), ( 61, 8, 20 ) ), ( ( -65, 8, -54 ), ( 65, 40, -20 ) ) ] } )
    , ( "4315.dat", { studs = [ ( -30, 0, 0 ), ( -10, 0, 0 ), ( 10, 0, 0 ), ( 30, 0, 0 ) ], antiStuds = [ ( -30, 8, 0 ), ( -10, 8, 0 ), ( 10, 8, 0 ), ( 30, 8, 0 ) ], others = [], boxes = [ ( ( -40, -4, -10 ), ( 40, 8, 10 ) ) ] } )
    , ( "44570.dat", { studs = [ ( -30, 0, -30 ), ( -30, 0, -10 ), ( -30, 0, 10 ), ( -10, 0, -30 ), ( -10, 0, -10 ), ( -10, 0, 10 ), ( 10, 0, -30 ), ( 10, 0, -10 ), ( 10, 0, 10 ), ( 30, 0, -30 ), ( 30, 0, -10 ), ( 30, 0, 10 ) ], antiStuds = [ ( -30, 8, -30 ), ( -30, 8, -10 ), ( -30, 8, 10 ), ( -10, 8, -30 ), ( -10, 8, -10 ), ( -10, 8, 10 ), ( 10, 8, -30 ), ( 10, 8, -10 ), ( 10, 8, 10 ), ( 30, 8, -30 ), ( 30, 8, -10 ), ( 30, 8, 10 ) ], others = [], boxes = [ ( ( -40, -4, -40 ), ( 40, 8, 20 ) ) ] } )
    , ( "2429.dat", { studs = [ ( -30, 0, 10 ), ( -10, 0, 10 ) ], antiStuds = [ ( -30, 8, 10 ), ( -10, 8, 10 ) ], others = [], boxes = [ ( ( -40, -4, 0 ), ( 0, 8, 20 ) ) ] } )
    , ( "2430.dat", { studs = [ ( 10, 0, 10 ), ( 30, 0, 10 ) ], antiStuds = [ ( 10, 8, 10 ), ( 30, 8, 10 ) ], others = [], boxes = [ ( ( 0, -4, 0 ), ( 40, 8, 20 ) ) ] } )
    , ( "4275b.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 8, 0 ), ( 10, 8, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 8, 10 ) ) ] } )
    , ( "4276b.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 8, 0 ), ( 10, 8, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 8, 10 ) ) ] } )
    , ( "44301a.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 8, 0 ), ( 10, 8, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 8, 10 ) ) ] } )
    , ( "44302a.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [ ( -10, 8, 0 ), ( 10, 8, 0 ) ], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 8, 10 ) ) ] } )
    , ( "44568.dat", { studs = [ ( -30, 0, 0 ), ( -10, 0, 0 ), ( 10, 0, 0 ), ( 30, 0, 0 ) ], antiStuds = [ ( -30, 8, 0 ), ( -10, 8, 0 ), ( 10, 8, 0 ), ( 30, 8, 0 ) ], others = [], boxes = [ ( ( -40, -4, -10 ), ( 40, 8, 10 ) ) ] } )
    , ( "3937.dat", { studs = [], antiStuds = [ ( -10, 24, 0 ), ( 10, 24, 0 ) ], others = [], boxes = [ ( ( -20, 2, -10 ), ( 20, 24, 10 ) ) ] } )
    , ( "3938.dat", { studs = [ ( -10, 0, 0 ), ( 10, 0, 0 ) ], antiStuds = [], others = [], boxes = [ ( ( -20, -4, -10 ), ( 20, 16, 10 ) ) ] } )
    , ( "3679.dat", { studs = [ ( -10, 0, -10 ), ( -10, 0, 10 ), ( 10, 0, -10 ), ( 10, 0, 10 ) ], antiStuds = [], others = [], boxes = [ ( ( -17.5, -4, -17.5 ), ( 17.5, 8, 17.5 ) ) ] } )
    , ( "3742.dat", { studs = [], antiStuds = [], others = [], boxes = [ ( ( -11, -4, -11.5 ), ( 11, 2, 11.9 ) ) ] } )
    , ( "3711.dat", { studs = [], antiStuds = [], others = [], boxes = [ ( ( -9.7, -5, -5 ), ( 9.8, 5, 21 ) ) ] } )
    , ( "4000.dat", { studs = [], antiStuds = [], others = [], boxes = [ ( ( -266, -21, -35 ), ( 46, 34, 35 ) ) ] } )
    , ( "4593.dat", { studs = [], antiStuds = [], others = [], boxes = [ ( ( -4, -38, -4 ), ( 4, 4, 4 ) ) ] } )
    , ( "60601.dat", { studs = [], antiStuds = [], others = [], boxes = [] } )
    , ( "60602.dat", { studs = [], antiStuds = [], others = [], boxes = [] } )
    , ( "32209.dat", { studs = [], antiStuds = [], others = [ ( "axle", ( -45, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -25, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -5, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 15, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 35, 0, 0 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -55, -8, -8 ), ( 52.5, 8, 8 ) ) ] } )
    , ( "32235.dat", { studs = [], antiStuds = [], others = [ ( "axle", ( -180, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -160, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -140, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -120, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -100, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -80, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -60, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -40, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -20, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 0, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 20, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 40, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 60, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 80, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 100, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 120, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 140, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 160, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 180, 0, 0 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -190, -4, -4 ), ( 190, 4, 4 ) ) ] } )
    , ( "87083.dat", { studs = [], antiStuds = [], others = [ ( "axle", ( -30, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -10, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 10, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 30, 0, 0 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -40, -4, -4 ), ( 40, 4, 4 ) ) ] } )
    , ( "55013.dat", { studs = [], antiStuds = [], others = [ ( "axle", ( -70, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -50, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -30, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( -10, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 10, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 30, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 50, 0, 0 ), ( 1, 0, 0 ) ), ( "axle", ( 70, 0, 0 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -80, -4, -4 ), ( 80, 4, 4 ) ) ] } )
    , ( "4459.dat", { studs = [], antiStuds = [], others = [ ( "pin", ( -10, 0, 0 ), ( -1, 0, 0 ) ), ( "pin", ( 10, 0, 0 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -20, -8, -8 ), ( 20, 8, 8 ) ) ] } )
    , ( "32054.dat", { studs = [], antiStuds = [], others = [ ( "pin", ( -20, 0, 0 ), ( -1, 0, 0 ) ), ( "pin", ( 0, 0, 0 ), ( 1, 0, 0 ) ), ( "pin", ( 20, 0, 0 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -30, -8, -8 ), ( 30, 8, 8 ) ) ] } )
    , ( "15100.dat", { studs = [], antiStuds = [], others = [ ( "pin", ( 20, 0, 0 ), ( 1, 0, 0 ) ), ( "pinHole", ( 0, 0, 0 ), ( 0, 1, 0 ) ) ], boxes = [ ( ( -9, -10, -9 ), ( 30, 10, 9 ) ) ] } )
    , ( "18654.dat", { studs = [], antiStuds = [], others = [ ( "pinHole", ( 0, 0, 0 ), ( 0, 1, 0 ) ) ], boxes = [ ( ( -9, -10, -9 ), ( 9, 10, 9 ) ) ] } )
    , ( "15459.dat", { studs = [], antiStuds = [], others = [ ( "pinHole", ( 0, 0, -60 ), ( 1, 0, 0 ) ), ( "pinHole", ( 0, 0, -40 ), ( 1, 0, 0 ) ), ( "pinHole", ( 0, 0, -20 ), ( 1, 0, 0 ) ), ( "pinHole", ( 0, 0, 0 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -10, -9, -90 ), ( 10, 9, 9 ) ) ] } )
    , ( "32526.dat", { studs = [], antiStuds = [], others = [ ( "pinHole", ( 0, 0, 0 ), ( 0, 1, 0 ) ), ( "pinHole", ( 0, 0, 20 ), ( 0, 1, 0 ) ), ( "pinHole", ( 0, 0, 40 ), ( 0, 1, 0 ) ), ( "pinHole", ( 0, 0, 60 ), ( 0, 1, 0 ) ), ( "pinHole", ( 0, 0, 80 ), ( 0, 1, 0 ) ), ( "pinHole", ( 20, 0, 80 ), ( 0, 1, 0 ) ), ( "pinHole", ( 40, 0, 80 ), ( 0, 1, 0 ) ) ], boxes = [ ( ( -9, -10, -9 ), ( 9, 10, 89 ) ), ( ( -9, -10, 71 ), ( 49, 10, 89 ) ) ] } )
    , ( "32527.dat", { studs = [], antiStuds = [], others = [ ( "pinHole", ( 0, -50, -20 ), ( 0, 0, 1 ) ), ( "pinHole", ( 0, -30, -20 ), ( 0, 0, 1 ) ) ], boxes = [ ( ( -36.5, -59, -20 ), ( 9, -4, 79.7 ) ) ] } )
    , ( "32528.dat", { studs = [], antiStuds = [], others = [ ( "pinHole", ( 0, -50, -20 ), ( 0, 0, 1 ) ), ( "pinHole", ( 0, -30, -20 ), ( 0, 0, 1 ) ) ], boxes = [ ( ( -9, -59, -20 ), ( 36.5, -4, 79.7 ) ) ] } )
    , ( "4185a.dat", { studs = [], antiStuds = [], others = [ ( "axleHole", ( 0, 0, -5 ), ( 0, 0, -1 ) ), ( "axleHole", ( 0, 0, 0 ), ( 0, 0, 1 ) ), ( "axleHole", ( 0, 0, 5 ), ( 0, 0, 1 ) ) ], boxes = [ ( ( -30, -30, -5 ), ( 30, 30, 5 ) ) ] } )
    , ( "6628.dat", { studs = [], antiStuds = [], others = [ ( "pin", ( 10, 0, 0 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -18, -8, -8 ), ( 20, 8, 8 ) ) ] } )
    , ( "2736.dat", { studs = [], antiStuds = [], others = [ ( "axle", ( 8.75, 0, 0 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -20, -8, -8 ), ( 17.5, 8, 8 ) ) ] } )
    , ( "53585.dat", { studs = [], antiStuds = [], others = [ ( "axleHole", ( 0, -7.5, 0 ), ( 0, -1, 0 ) ), ( "axleHole", ( 0, 0, 0 ), ( 0, 1, 0 ) ), ( "axleHole", ( 0, 7.5, 0 ), ( 0, 1, 0 ) ) ], boxes = [ ( ( -12.8, -9.6, -12.8 ), ( 12.8, 9.6, 12.8 ) ) ] } )
    , ( "60176.dat", { studs = [], antiStuds = [], others = [ ( "axleHole", ( 0, 0, 0 ), ( 1, 0, 0 ) ) ], boxes = [ ( ( -20, -10, -29.2 ), ( 20, 10, 30 ) ) ] } )
    ]


boxContainsXZ : Vec3 -> BoundingBox3D -> Bool
boxContainsXZ p box =
    Vec3.getX p
        >= Vec3.getX box.min
        - 0.01
        && Vec3.getX p
        <= Vec3.getX box.max
        + 0.01
        && Vec3.getZ p
        >= Vec3.getZ box.min
        - 0.01
        && Vec3.getZ p
        <= Vec3.getZ box.max
        + 0.01


nearly : Float -> Float -> Bool
nearly a b =
    abs (a - b) < 0.01


{-| Structural invariants over every new entry (and the empty box list of the glass entries).
-}
layoutInvariants : String -> Connectivity.PartConnectivity -> List String
layoutInvariants file conn =
    let
        boxes =
            conn.boundingBoxes

        studs =
            List.filter (\c -> c.connType == Stud) conn.connections

        antiStuds =
            List.filter (\c -> c.connType == AntiStud) conn.connections

        badStudNormals =
            List.filter (\c -> Vec3.distance c.normal (vec3 0 -1 0) > 0.01) studs

        badAntiNormals =
            List.filter (\c -> Vec3.distance c.normal (vec3 0 1 0) > 0.01) antiStuds

        antiOffBox =
            List.filter
                (\a ->
                    not
                        (List.any
                            (\b -> boxContainsXZ a.localPosition b && nearly (Vec3.getY a.localPosition) (Vec3.getY b.max))
                            boxes
                        )
                )
                antiStuds

        studOffBox =
            List.filter
                (\s ->
                    not
                        (List.any
                            (\b -> boxContainsXZ s.localPosition b && nearly (Vec3.getY s.localPosition - 4) (Vec3.getY b.min))
                            boxes
                        )
                )
                studs

        firstBoxIsLowest =
            case boxes of
                first :: _ ->
                    List.all (\b -> Vec3.getY b.max <= Vec3.getY first.max + 0.01) boxes

                [] ->
                    True

        check ok message =
            if ok then
                []

            else
                [ file ++ ": " ++ message ]
    in
    List.concat
        [ check (List.isEmpty badStudNormals) "stud normal is not (0, -1, 0)"
        , check (List.isEmpty badAntiNormals) "anti-stud normal is not (0, 1, 0)"
        , check (List.isEmpty antiOffBox) "anti-stud not at the max Y of a box containing it"
        , check (List.isEmpty studOffBox || file == "24201.dat") "stud not on top (min Y + 4) of a box containing it"
        , check (firstBoxIsLowest || List.member file [ "15070.dat", "4022.dat" ]) "box 1 does not reach the part's bottom"
        ]


{-| A world point of `conn` placed at `matrix`.
-}
worldPointsOf : Mat4 -> Int -> Connectivity.PartConnectivity -> List Connectivity.WorldConnectionPoint
worldPointsOf matrix placementId conn =
    List.map (Connectivity.transformPoint matrix placementId) conn.connections


firstBox : Connectivity.PartConnectivity -> BoundingBox3D
firstBox conn =
    List.head conn.boundingBoxes |> Maybe.withDefault { min = vec3 0 0 0, max = vec3 0 0 0 }


{-| Drag `barFile` near the clip of `clipFile` (placed at `clipMatrix`) and check
that the bar's axis runs along the clip normal, a bar point sits on the clip point,
and the bar's box clears the clip part's box.
-}
checkBarInClip : String -> String -> Mat4 -> Vec3 -> Vec3 -> Expect.Expectation
checkBarInClip barFile clipFile clipMatrix clipPosition clipAxis =
    case ( knownCatalogPart barFile, knownCatalogPart clipFile ) of
        ( Just bar, Just clipPart ) ->
            let
                targets =
                    worldPointsOf clipMatrix 1 clipPart
                        |> List.filter (\t -> t.connType == ClipMale { width = 4.0 })

                nearMatrix =
                    Mat4.makeTranslate (Vec3.add clipPosition (vec3 3 -25 2))
            in
            case solveSnap 30 0 nearMatrix bar.connections targets of
                Just snap ->
                    let
                        axis =
                            Connectivity.transformDirection snap.snapMatrix (vec3 0 1 0)

                        barPoints =
                            List.map (\c -> Mat4.transform snap.snapMatrix c.localPosition) bar.connections
                    in
                    Expect.all
                        [ \_ -> Expect.atLeast 0.999 (abs (Vec3.dot axis clipAxis))
                        , \_ -> Expect.equal True (List.any (\p -> Vec3.distance p clipPosition < 0.01) barPoints)
                        , \_ ->
                            checkCollisions 0.5
                                [ ( 2, snap.snapMatrix, firstBox bar ) ]
                                (List.map (\b -> ( 1, clipMatrix, b )) clipPart.boundingBoxes)
                                |> Expect.equal []
                        ]
                        ()

                Nothing ->
                    Expect.fail (barFile ++ " did not snap into the clip of " ++ clipFile)

        _ ->
            Expect.fail (barFile ++ " or " ++ clipFile ++ " is not a catalog part")


{-| A bar dragged into a 4085c that rests on the ground seats flush on the ground:
no bar point and no box corner below Y 0 (LDraw Y down).
-}
checkBarOnGround : String -> Float -> Expect.Expectation
checkBarOnGround barFile barLength =
    case ( knownCatalogPart barFile, knownCatalogPart "4085c.dat" ) of
        ( Just bar, Just clipPlate ) ->
            let
                -- plate bottom (local Y 8) on the ground: clip at world (0, −4, −20)
                plateMatrix =
                    Mat4.makeTranslate3 0 -8 0

                targets =
                    worldPointsOf plateMatrix 1 clipPlate
                        |> List.filter (\t -> t.connType == ClipMale { width = 4.0 })

                -- hover with the bar's point 10 LDU from its lower end at the clip
                nearMatrix =
                    Mat4.makeTranslate3 2 (-4 - (barLength - 10)) -18
            in
            case solveSnap 30 0 nearMatrix bar.connections targets of
                Just snap ->
                    let
                        lowestPoint =
                            bar.connections
                                |> List.map (\c -> Vec3.getY (Mat4.transform snap.snapMatrix c.localPosition))
                                |> List.maximum
                                |> Maybe.withDefault 0

                        box =
                            firstBox bar

                        boxYs =
                            [ Vec3.getY (Mat4.transform snap.snapMatrix box.min)
                            , Vec3.getY (Mat4.transform snap.snapMatrix box.max)
                            ]
                    in
                    Expect.all
                        [ \_ -> Expect.atMost 0.5 lowestPoint
                        , \_ -> Expect.atMost 0.5 (List.maximum boxYs |> Maybe.withDefault 0)
                        , \_ -> Expect.within (Expect.Absolute 0.01) 0 (List.maximum boxYs |> Maybe.withDefault 99)
                        ]
                        ()

                Nothing ->
                    Expect.fail (barFile ++ " found no seat in a ground-level 4085c")

        _ ->
            Expect.fail (barFile ++ " or 4085c.dat is not a catalog part")


microscaleCatalogTests : Test
microscaleCatalogTests =
    describe "microscale, slope and curated part layouts"
        [ describe "layouts match the measured table"
            (List.map
                (\( file, expected ) ->
                    test file <|
                        \_ ->
                            case knownCatalogPart file of
                                Just conn ->
                                    Expect.equal expected (layoutOf conn)

                                Nothing ->
                                    Expect.fail (file ++ " is not a catalog part")
                )
                expectedLayouts
            )
        , test "the layout invariants hold for every new entry" <|
            \_ ->
                expectedLayouts
                    |> List.concatMap
                        (\( file, _ ) ->
                            knownCatalogPart file
                                |> Maybe.map (layoutInvariants file)
                                |> Maybe.withDefault [ file ++ ": missing" ]
                        )
                    |> Expect.equal []
        , test "3811 baseplate 32 x 32 has 1024 studs and no anti-studs" <|
            \_ ->
                case knownCatalogPart "3811.dat" of
                    Just conn ->
                        let
                            layout =
                                layoutOf conn
                        in
                        Expect.all
                            [ \_ -> Expect.equal 1024 (List.length layout.studs)
                            , \_ -> Expect.equal [] layout.antiStuds
                            , \_ -> Expect.equal [] layout.others
                            , \_ ->
                                List.filter (\s -> List.member s [ ( -310, 0, -310 ), ( -310, 0, 310 ), ( 310, 0, -310 ), ( 310, 0, 310 ) ]) layout.studs
                                    |> List.length
                                    |> Expect.equal 4
                            , \_ -> Expect.equal [ ( ( -320, -4, -320 ), ( 320, 4, 320 ) ) ] layout.boxes
                            , \_ -> Expect.equal [] (layoutInvariants "3811.dat" conn)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "3811.dat is not a catalog part"
        , test "3867 baseplate 16 x 16 has 256 studs and no anti-studs" <|
            \_ ->
                case knownCatalogPart "3867.dat" of
                    Just conn ->
                        let
                            layout =
                                layoutOf conn
                        in
                        Expect.all
                            [ \_ -> Expect.equal 256 (List.length layout.studs)
                            , \_ -> Expect.equal [] layout.antiStuds
                            , \_ -> Expect.equal (Just ( -150, 0, -150 )) (List.head layout.studs)
                            , \_ -> Expect.equal (Just ( 150, 0, 150 )) (List.head (List.reverse layout.studs))
                            , \_ -> Expect.equal [ ( ( -160, -4, -160 ), ( 160, 4, 160 ) ) ] layout.boxes
                            ]
                            ()

                    Nothing ->
                        Expect.fail "3867.dat is not a catalog part"
        , describe "bars seat in clips at the held bar's centre"
            [ test "87994 in 61252: bar along X through (0, 2, -20), clear of the plate" <|
                \_ -> checkBarInClip "87994.dat" "61252.dat" Mat4.identity (vec3 0 2 -20) (vec3 1 0 0)
            , test "30374 in 4085c: bar along Y through (0, 4, -20), clear of the plate" <|
                \_ -> checkBarInClip "30374.dat" "4085c.dat" Mat4.identity (vec3 0 4 -20) (vec3 0 1 0)
            , test "87994 in 4081b: bar along X through (0, 2, -20)" <|
                \_ -> checkBarInClip "87994.dat" "4081b.dat" Mat4.identity (vec3 0 2 -20) (vec3 1 0 0)
            , test "87994 in 63868: bar along Z through (30, 2, 0)" <|
                \_ -> checkBarInClip "87994.dat" "63868.dat" Mat4.identity (vec3 30 2 0) (vec3 0 0 1)
            , test "87994 in 15712: bar along Z through (0, -6, 0)" <|
                \_ -> checkBarInClip "87994.dat" "15712.dat" Mat4.identity (vec3 0 -6 0) (vec3 0 0 1)
            ]
        , describe "a bar in a ground-level 4085c does not go below the ground"
            [ test "30374 (4L) seats flush on the ground" <|
                \_ -> checkBarOnGround "30374.dat" 80
            , test "87994 (3L) seats flush on the ground" <|
                \_ -> checkBarOnGround "87994.dat" 60
            ]
        , describe "side-target normals (SideSnap takes |normal.y| < 0.3)"
            [ test "4085c's clip is vertical, so it is no longer a side target" <|
                \_ ->
                    knownCatalogPart "4085c.dat"
                        |> Maybe.map (.connections >> List.filter (\c -> c.connType == ClipMale { width = 4.0 }) >> List.map (\c -> abs (Vec3.getY c.normal) < 0.3))
                        |> Expect.equal (Just [ False ])
            , test "61252's clip is horizontal, so it is a side target" <|
                \_ ->
                    knownCatalogPart "61252.dat"
                        |> Maybe.map (.connections >> List.filter (\c -> c.connType == ClipMale { width = 4.0 }) >> List.map (\c -> abs (Vec3.getY c.normal) < 0.3))
                        |> Expect.equal (Just [ True ])
            ]
        , describe "microscale bar and clip connectors"
            [ test "32828 (plate 1x1 round with bar handle) has BarFemale connection on handle" <|
                \_ ->
                    knownCatalogPart "32828.dat"
                        |> Maybe.map (.connections >> List.filter (\c -> c.connType == BarFemale { diameter = 4.0 }))
                        |> Maybe.map List.length
                        |> Expect.equal (Just 1)
            , test "15712 (tile 1x1 with clip) has ClipMale connection" <|
                \_ ->
                    knownCatalogPart "15712.dat"
                        |> Maybe.map (.connections >> List.filter (\c -> c.connType == ClipMale { width = 4.0 }))
                        |> Maybe.map List.length
                        |> Expect.equal (Just 1)
            , test "15712 clip can connect to 32828 bar handle" <|
                \_ ->
                    let
                        clip =
                            knownCatalogPart "15712.dat"
                                |> Maybe.andThen (.connections >> List.filter (\c -> c.connType == ClipMale { width = 4.0 }) >> List.head)

                        bar =
                            knownCatalogPart "32828.dat"
                                |> Maybe.andThen (.connections >> List.filter (\c -> c.connType == BarFemale { diameter = 4.0 }) >> List.head)
                    in
                    case ( clip, bar ) of
                        ( Just c, Just b ) ->
                            Connectivity.isCompatible c.connType b.connType
                                |> Expect.equal True

                        _ ->
                            Expect.fail "Missing clip or bar connection point"
            ]
        ]
