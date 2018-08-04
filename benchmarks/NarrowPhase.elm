module NarrowPhase exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Fixtures.ConvexPolyhedron as HullFixtures
import Fixtures.NarrowPhase
import Physics.NarrowPhase as NarrowPhase
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform


{- For a useful benchmark,
   copy and rename an older baseline version of Physics/NarrowPhase.elm
   to Physics/OriginalNarrowPhase.elm and toggle the import below
   from:

      import Physics.NarrowPhase as OriginalNarrowPhase

    to:

      import Physics.OriginalNarrowPhase as OriginalNarrowPhase

    Switching it back to use the (current) NarrowPhase.elm through the
    OriginalNarrowPhase alias keeps obsolete or redundant code out of
    the repo while the comparison benchmarks continue to be maintained and
    built and run essentially as absolute non-comparison benchmarks until
    they are needed again in another round of performance work.
-}

import Physics.NarrowPhase as OriginalNarrowPhase


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        radius =
            5

        boxHalfExtent =
            1

        boxHull =
            HullFixtures.boxHull boxHalfExtent

        originalBoxHull =
            HullFixtures.originalBoxHull boxHalfExtent

        boxPositions =
            Fixtures.NarrowPhase.sphereContactBoxPositions radius boxHalfExtent
                |> List.map Tuple.first

        octoHalfExtent =
            3

        octoHull =
            HullFixtures.octoHull octoHalfExtent

        originalOctoHull =
            HullFixtures.originalOctoHull octoHalfExtent

        octoPositions =
            Fixtures.NarrowPhase.sphereContactOctohedronPositions radius octoHalfExtent
                |> List.map Tuple.first
    in
        describe "NarrowPhase"
            [ Benchmark.compare "addSphereConvexContacts"
                "baseline"
                (\_ ->
                    boxPositions
                        |> List.map
                            (\position ->
                                OriginalNarrowPhase.addSphereConvexContacts
                                    Transform.identity
                                    radius
                                    0
                                    { position = position
                                    , quaternion = Quaternion.identity
                                    }
                                    originalBoxHull
                                    1
                                    []
                            )
                )
                "latest code"
                (\_ ->
                    boxPositions
                        |> List.map
                            (\position ->
                                NarrowPhase.addSphereConvexContacts
                                    Transform.identity
                                    radius
                                    0
                                    { position = position
                                    , quaternion = Quaternion.identity
                                    }
                                    boxHull
                                    1
                                    []
                            )
                )
            , Benchmark.compare "addSphereConvexContacts octohedron"
                "baseline"
                (\_ ->
                    octoPositions
                        |> List.map
                            (\position ->
                                OriginalNarrowPhase.addSphereConvexContacts
                                    Transform.identity
                                    radius
                                    0
                                    { position = position
                                    , quaternion = Quaternion.identity
                                    }
                                    originalOctoHull
                                    1
                                    []
                            )
                )
                "latest code"
                (\_ ->
                    octoPositions
                        |> List.map
                            (\position ->
                                NarrowPhase.addSphereConvexContacts
                                    Transform.identity
                                    radius
                                    0
                                    { position = position
                                    , quaternion = Quaternion.identity
                                    }
                                    octoHull
                                    1
                                    []
                            )
                )
            ]
