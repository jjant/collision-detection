module Epa exposing
    ( Polytope(..)
    , epa
    , epaBestNormal
    , updatePolytope
    )

{-| WIP. This should probably return the collision normal, penetration depth, and maybe also the closest points between the two bodies.

I _think_ these points are the ones that effectively generated the normal, but not sure yet.

-}

import Array exposing (Array)
import Array.Extra
import CSOPoint exposing (CSOPoint)
import Gjk exposing (LocalSupportMap)
import Isometry exposing (Isometry)
import List.Extra
import Util
import Vec2 exposing (Vec2, vec2)


{-| NOTE: This function assumes that the bodies are NOT colliding. Infinite loops (or worse) may happen if they weren't.
Takes the simplex with which GJK ended and returns the polytope that contains the closest point to the origin.
-}
epa : Polytope CSOPoint -> Isometry -> LocalSupportMap -> LocalSupportMap -> Polytope CSOPoint
epa polytope pos12 g1 g2 =
    let
        minFace =
            epaBestNormal polytope

        { done, newPolytope } =
            updatePolytope minFace pos12 g1 g2 polytope
    in
    -- TODO: Can I assume polytope is in counter-clockwise order??
    --
    -- I don't think so, I think the triangle from GJK may be arbitrarily winded.
    -- I'm not sure, so might as well cover both cases in doFace.
    if done then
        newPolytope

    else
        epa newPolytope pos12 g1 g2


epaBestNormal : Polytope CSOPoint -> { index : Int, origin : Vec2, normal : Vec2, distance : Float }
epaBestNormal polytope =
    let
        len =
            length polytope

        faceNormals =
            List.range 0 (len - 1)
                |> List.map
                    (\i ->
                        ( i
                        , getUnsafe "i" i polytope
                        , getUnsafe "i + 1" (modBy len (i + 1)) polytope
                        )
                    )
                |> List.map
                    (\( index, vertexI, vertexJ ) ->
                        let
                            { normal, distance } =
                                doFace vertexI.point vertexJ.point
                        in
                        { index = index
                        , origin = Vec2.midpoint vertexI.point vertexJ.point
                        , normal = normal
                        , distance = distance
                        }
                    )

        minFace : { index : Int, origin : Vec2, normal : Vec2, distance : Float }
        minFace =
            faceNormals
                |> List.Extra.minimumBy (\{ distance } -> distance)
                |> Util.unwrap "Min face not found"
    in
    minFace


length : Polytope a -> Int
length (Polytope _ _ _ rest) =
    3 + Array.length rest


{-| -}
updatePolytope : { r | normal : Vec2, distance : Float, index : Int } -> Isometry -> (Vec2 -> Vec2) -> (Vec2 -> Vec2) -> Polytope CSOPoint -> { done : Bool, newPolytope : Polytope CSOPoint }
updatePolytope face pos12 g1 g2 polytope =
    let
        supportPoint_ =
            Gjk.support pos12 g1 g2 face.normal

        minDistance =
            Vec2.dot face.normal supportPoint_.point
    in
    if abs (minDistance - face.distance) > 0.001 then
        { done = False, newPolytope = insertAfter face.index supportPoint_ polytope }

    else
        { done = True, newPolytope = polytope }


{-| Inserts AFTER index

insertAfter 3 point polytope =

                    |-- Array indices --|
                    0   1   2   3
        a   b   c   d | e   f   g
        0   1   2   3 ^ 4   5   6
                      |
                      | insert here

-}
insertAfter : Int -> a -> Polytope a -> Polytope a
insertAfter index point (Polytope first second third rest) =
    case index of
        0 ->
            Polytope first point second (Array.Extra.insertAt 0 third rest)

        1 ->
            Polytope first second point (Array.Extra.insertAt 0 third rest)

        2 ->
            Polytope first second third (Array.Extra.insertAt 0 point rest)

        _ ->
            Polytope first second third (Array.Extra.insertAt (index - 2) point rest)


type Polytope a
    = Polytope a a a (Array a)


get : Int -> Polytope a -> Maybe a
get index (Polytope a0 a1 a2 rest) =
    case index of
        0 ->
            Just a0

        1 ->
            Just a1

        2 ->
            Just a2

        _ ->
            Array.get (index - 3) rest


getUnsafe : String -> Int -> Polytope a -> a
getUnsafe msg index p =
    Util.unwrap msg (get index p)


{-| Takes an edge (two contiguous vertices) from the polytope (inside the Minkowski difference),
and returns the out-facing normal.
-}
doFace : Vec2 -> Vec2 -> { normal : Vec2, distance : Float }
doFace vertexI vertexJ =
    let
        iToJ =
            Vec2.sub vertexJ vertexI

        tentativeNormal =
            vec2 iToJ.y -iToJ.x
                |> Vec2.normalize

        tentativeDistance =
            Vec2.dot tentativeNormal vertexI
    in
    if tentativeDistance < 0 then
        { normal = Vec2.scale -1 tentativeNormal
        , distance = -tentativeDistance
        }

    else
        { normal = tentativeNormal
        , distance = tentativeDistance
        }
