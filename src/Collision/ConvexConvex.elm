module Collision.ConvexConvex exposing
    ( addContacts
    , findSeparatingAxis
    , project
    , testSeparatingAxis
    )

import Internal.Const as Const
import Internal.Contact exposing (Contact)
import Internal.Convex as Convex exposing (Convex, Face)
import Internal.Vector3 as Vec3 exposing (Vec3)


minDist : Float
minDist =
    -100


maxDist : Float
maxDist =
    100


addContacts : Convex -> Convex -> List Contact -> List Contact
addContacts convex1 convex2 contacts =
    case findSeparatingAxis convex1 convex2 of
        Just separatingAxis ->
            let
                reversedSeparatingAxis =
                    Vec3.negate separatingAxis
            in
            case bestFace convex1.faces separatingAxis of
                Just face1 ->
                    case bestFace convex2.faces reversedSeparatingAxis of
                        Just face2 ->
                            clipTwoFaces face1 face2 reversedSeparatingAxis contacts

                        Nothing ->
                            contacts

                Nothing ->
                    contacts

        Nothing ->
            contacts


clipTwoFaces : Face -> Face -> Vec3 -> List Contact -> List Contact
clipTwoFaces face { vertices } separatingAxis contacts =
    let
        point =
            case face.vertices of
                first :: _ ->
                    first

                [] ->
                    Vec3.zero

        worldPlaneConstant =
            -(Vec3.dot face.normal point)
    in
    List.foldl
        (\vertex result ->
            let
                depth =
                    max minDist (Vec3.dot face.normal vertex + worldPlaneConstant)
            in
            if depth <= maxDist && depth <= 0 then
                { ni = separatingAxis
                , pi = Vec3.sub vertex (Vec3.scale depth face.normal)
                , pj = vertex
                }
                    :: result

            else
                result
        )
        contacts
        (clipAgainstAdjacentFaces face vertices)


bestFace : List Face -> Vec3 -> Maybe Face
bestFace faces separatingAxis =
    case faces of
        face :: restFaces ->
            Just
                (bestFaceHelp
                    separatingAxis
                    restFaces
                    face
                    (Vec3.dot face.normal separatingAxis)
                )

        [] ->
            Nothing


bestFaceHelp : Vec3 -> List Face -> Face -> Float -> Face
bestFaceHelp separatingAxis faces currentBestFace currentBestDistance =
    case faces of
        face :: remainingFaces ->
            let
                faceDistance =
                    Vec3.dot face.normal separatingAxis
            in
            if currentBestDistance - faceDistance > 0 then
                bestFaceHelp
                    separatingAxis
                    remainingFaces
                    face
                    faceDistance

            else
                bestFaceHelp
                    separatingAxis
                    remainingFaces
                    currentBestFace
                    currentBestDistance

        [] ->
            currentBestFace


clipAgainstAdjacentFaces : Face -> List Vec3 -> List Vec3
clipAgainstAdjacentFaces { vertices, normal } worldVertices =
    Convex.foldFaceEdges
        (\v1 v2 ->
            let
                edge =
                    Vec3.normalize (Vec3.sub v1 v2)

                planeNormal =
                    Vec3.cross normal edge

                planeConstant =
                    -(Vec3.dot v1 planeNormal)
            in
            Convex.foldFaceEdges
                (clipFaceAgainstPlaneAdd planeNormal planeConstant)
                []
        )
        worldVertices
        vertices


clipFaceAgainstPlaneAdd : Vec3 -> Float -> Vec3 -> Vec3 -> List Vec3 -> List Vec3
clipFaceAgainstPlaneAdd planeNormal planeConstant prev next result =
    let
        nDotPrev =
            Vec3.dot planeNormal prev + planeConstant

        nDotNext =
            Vec3.dot planeNormal next + planeConstant
    in
    if nDotPrev < 0 then
        if nDotNext < 0 then
            next :: result

        else
            Vec3.lerp (nDotPrev / (nDotPrev - nDotNext)) prev next
                :: result

    else if nDotNext < 0 then
        next
            :: Vec3.lerp (nDotPrev / (nDotPrev - nDotNext)) prev next
            :: result

    else
        result


findSeparatingAxis : Convex -> Convex -> Maybe Vec3
findSeparatingAxis convex1 convex2 =
    let
        -- normals from both convexes converted in the world coordinates
        worldNormals =
            convex1.uniqueNormals ++ convex2.uniqueNormals
    in
    case testUniqueNormals convex1 convex2 worldNormals Vec3.zero Const.maxNumber of
        Just { target, dmin } ->
            testUniqueEdges convex1
                convex2
                convex2.uniqueEdges
                convex1.uniqueEdges
                convex2.uniqueEdges
                target
                dmin

        _ ->
            Nothing


testUniqueNormals : Convex -> Convex -> List Vec3 -> Vec3 -> Float -> Maybe { target : Vec3, dmin : Float }
testUniqueNormals convex1 convex2 normals target dmin =
    case normals of
        [] ->
            Just { target = target, dmin = dmin }

        normal :: restNormals ->
            case testSeparatingAxis convex1 convex2 normal of
                Nothing ->
                    Nothing

                Just d ->
                    if d - dmin < 0 then
                        testUniqueNormals convex1 convex2 restNormals normal d

                    else
                        testUniqueNormals convex1 convex2 restNormals target dmin


testUniqueEdges : Convex -> Convex -> List Vec3 -> List Vec3 -> List Vec3 -> Vec3 -> Float -> Maybe Vec3
testUniqueEdges convex1 convex2 initEdges2 edges1 edges2 target dmin =
    case edges1 of
        [] ->
            if Vec3.dot (Vec3.sub convex2.position convex1.position) target > 0 then
                Just (Vec3.negate target)

            else
                Just target

        worldEdge1 :: remainingEdges1 ->
            case edges2 of
                [] ->
                    -- requeue edges2
                    testUniqueEdges convex1 convex2 initEdges2 remainingEdges1 initEdges2 target dmin

                worldEdge2 :: remainingEdges2 ->
                    let
                        cross =
                            Vec3.cross worldEdge1 worldEdge2
                    in
                    if Vec3.almostZero cross then
                        -- continue because edges are parallel
                        testUniqueEdges convex1 convex2 initEdges2 edges1 remainingEdges2 target dmin

                    else
                        let
                            normalizedCross =
                                Vec3.normalize cross
                        in
                        case testSeparatingAxis convex1 convex2 normalizedCross of
                            Nothing ->
                                -- exit because hulls don't collide
                                Nothing

                            Just dist ->
                                if dist - dmin < 0 then
                                    -- update target and dmin
                                    testUniqueEdges convex1 convex2 initEdges2 edges1 remainingEdges2 normalizedCross dist

                                else
                                    -- continue
                                    testUniqueEdges convex1 convex2 initEdges2 edges1 remainingEdges2 target dmin


{-| If projections of two convexes don’t overlap, then they don’t collide.
-}
testSeparatingAxis : Convex -> Convex -> Vec3 -> Maybe Float
testSeparatingAxis convex1 convex2 separatingAxis =
    let
        p1 =
            project separatingAxis Const.maxNumber -Const.maxNumber convex1.vertices

        p2 =
            project separatingAxis Const.maxNumber -Const.maxNumber convex2.vertices

        d1 =
            p1.max - p2.min

        d2 =
            p2.max - p1.min
    in
    if d1 < 0 || d2 < 0 then
        Nothing

    else if d1 - d2 > 0 then
        Just d2

    else
        Just d1


{-| Get max and min dot product of a convex hull at ShapeWorldTransform3d projected onto an axis.
-}
project : Vec3 -> Float -> Float -> List Vec3 -> { min : Float, max : Float }
project localAxis minVal maxVal currentVertices =
    case currentVertices of
        [] ->
            { min = minVal, max = maxVal }

        vec :: remainingVertices ->
            let
                val =
                    vec.x * localAxis.x + vec.y * localAxis.y + vec.z * localAxis.z
            in
            project
                localAxis
                (if minVal - val > 0 then
                    val

                 else
                    minVal
                )
                (if maxVal - val > 0 then
                    maxVal

                 else
                    val
                )
                remainingVertices
