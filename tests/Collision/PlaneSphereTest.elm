module Collision.PlaneSphereTest exposing (addContacts)

import Collision.PlaneSphere
import Extra.Expect as Expect
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Shapes.Sphere as Sphere
import Test exposing (Test, describe, test)


addContacts : Test
addContacts =
    let
        radius =
            1

        plane =
            { position = Vec3.zero, normal = Vec3.zAxis }

        sphere =
            Sphere.atOrigin radius
                |> Sphere.placeIn (Transform3d.atPoint { x = 0, y = 0, z = radius })

        delta =
            0.3

        overlappingSphere =
            Sphere.atOrigin radius
                |> Sphere.placeIn (Transform3d.atPoint { x = 0, y = 0, z = radius - delta })

        nonCollidingSphere =
            Sphere.atOrigin radius
                |> Sphere.placeIn (Transform3d.atPoint { x = 0, y = 0, z = radius + delta })
    in
    describe "Collision.PlaneSphere.addContacts"
        [ test "exact collision" <|
            \_ ->
                Collision.PlaneSphere.addContacts identity plane sphere []
                    |> Expect.contacts
                        [ { ni = { x = 0, y = 0, z = 1 }
                          , pi = { x = 0, y = 0, z = 0 }
                          , pj = { x = 0, y = 0, z = 0 }
                          }
                        ]
        , test "overlapping collision" <|
            \_ ->
                Collision.PlaneSphere.addContacts identity plane overlappingSphere []
                    |> Expect.contacts
                        [ { ni = { x = 0, y = 0, z = 1 }
                          , pi = { x = 0, y = 0, z = 0 }
                          , pj = { x = 0, y = 0, z = -delta }
                          }
                        ]
        , test "no collision" <|
            \_ ->
                Collision.PlaneSphere.addContacts identity plane nonCollidingSphere []
                    |> Expect.contacts []
        ]
