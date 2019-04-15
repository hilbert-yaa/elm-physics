module NarrowPhaseTest exposing (addSphereConvexContacts)

import Expect exposing (Expectation)
import Fixtures.ConvexPolyhedron as HullFixtures
import Fixtures.NarrowPhase exposing (body1, body2)
import Internal.Body as Body exposing (Body)
import Internal.Const as Const
import Internal.NarrowPhase as NarrowPhase
import Internal.Quaternion as Quaternion
import Internal.SolverEquation exposing (ContactEquation)
import Internal.Vector3 as Vec3 exposing (Vec3, vec3)
import Test exposing (..)


addSphereConvexContacts : Test
addSphereConvexContacts =
    let
        center =
            vec3 0 0 7

        radius =
            5

        boxHalfExtent =
            3

        boxHull =
            HullFixtures.boxHull boxHalfExtent

        ( boxPositions, boxExpectedResults ) =
            Fixtures.NarrowPhase.sphereContactBoxPositions
                center
                radius
                boxHalfExtent
                |> listOfPairsToPairOfLists

        boxFarPositions =
            Fixtures.NarrowPhase.sphereContactBoxPositions
                center
                (radius * 2)
                boxHalfExtent
                |> List.map Tuple.first

        octoHalfExtent =
            1

        octoHull =
            HullFixtures.octoHull octoHalfExtent

        ( octoPositions, octoExpectedResults ) =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions
                center
                radius
                octoHalfExtent
                |> listOfPairsToPairOfLists

        octoFarPositions =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions
                center
                (radius * 2)
                octoHalfExtent
                |> List.map Tuple.first
    in
    describe "NarrowPhase.addSphereConvexContacts"
        [ test "for a box" <|
            \_ ->
                boxPositions
                    |> List.map
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , orientation = Quaternion.identity
                                }
                                boxHull
                                body2
                                []
                        )
                    |> expectNormalizedEqual
                        (normalizeListTowards <|
                            normalizeListTowards <|
                                normalizeContactTowards
                        )
                        boxExpectedResults
        , test "fail for a far box" <|
            \_ ->
                boxFarPositions
                    |> List.concatMap
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , orientation = Quaternion.identity
                                }
                                boxHull
                                body2
                                []
                        )
                    |> Expect.equal []
        , test "for an octohedron" <|
            \_ ->
                octoPositions
                    |> List.map
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , orientation = Quaternion.identity
                                }
                                octoHull
                                body2
                                []
                        )
                    |> expectNormalizedEqual
                        (normalizeListTowards <|
                            normalizeListTowards <|
                                normalizeContactTowards
                        )
                        octoExpectedResults
        , test "fail for a far octohedron" <|
            \_ ->
                octoFarPositions
                    |> List.concatMap
                        (\position ->
                            NarrowPhase.addSphereConvexContacts
                                { position = center
                                , orientation = Quaternion.identity
                                }
                                radius
                                body1
                                { position = position
                                , orientation = Quaternion.identity
                                }
                                octoHull
                                body2
                                []
                        )
                    |> Expect.equal []
        ]



-- Test helpers


listOfPairsToPairOfLists : List ( a, b ) -> ( List a, List b )
listOfPairsToPairOfLists list =
    ( List.map Tuple.first list
    , List.map Tuple.second list
    )


expectNormalizedEqual : (a -> a -> a) -> a -> a -> Expectation
expectNormalizedEqual normalizeTowards expected actual =
    actual
        |> normalizeTowards expected
        |> Expect.equal expected


normalizeListTowards : (a -> a -> a) -> List a -> List a -> List a
normalizeListTowards normalizeElementTowards expected actual =
    List.map2
        normalizeElementTowards
        expected
        actual


normalizeContactTowards : ContactEquation () -> ContactEquation () -> ContactEquation ()
normalizeContactTowards expected actual =
    -- optimize common case
    if actual == expected then
        actual

    else
        { actual
            | ni = normalizeVec3Towards expected.ni actual.ni
            , ri = normalizeVec3Towards expected.ri actual.ri
            , rj = normalizeVec3Towards expected.rj actual.rj
        }


normalizeVec3Towards : Vec3 -> Vec3 -> Vec3
normalizeVec3Towards expected actual =
    if Vec3.distanceSquared expected actual < Const.precision then
        -- ignore any negligible difference.
        expected

    else
        actual
