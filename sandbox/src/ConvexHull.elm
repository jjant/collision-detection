module ConvexHull exposing (convexHull)

import List.Extra
import Unwrap
import Vec2 exposing (Vec2)



---- Convex hull ----


convexHull : List Vec2 -> List Vec2
convexHull points =
    -- gift wrapping algorithm
    let
        sortedPoints =
            List.sortBy .x points

        pointOnHull =
            sortedPoints
                |> List.head
                |> Unwrap.maybe
    in
    doHull pointOnHull pointOnHull sortedPoints []


doHull : Vec2 -> Vec2 -> List Vec2 -> List Vec2 -> List Vec2
doHull endpoint pointOnHull points hull =
    let
        newHull =
            hull ++ [ pointOnHull ]

        newEndpoint =
            innerLoop 0 points (List.length points) endpoint pointOnHull
    in
    if Just newEndpoint == List.head newHull then
        newHull

    else
        doHull endpoint newEndpoint points newHull


innerLoop j points numPoints endpoint pointOnHull =
    if j >= numPoints then
        endpoint

    else
        let
            sj =
                List.Extra.getAt j points
                    |> Unwrap.maybe
        in
        if (endpoint == pointOnHull) || isOnTheLeft pointOnHull endpoint sj then
            innerLoop (j + 1) points numPoints sj pointOnHull

        else
            innerLoop (j + 1) points numPoints endpoint pointOnHull


isOnTheLeftEpsilon =
    -- 0.0001
    0


isOnTheLeft : Vec2 -> Vec2 -> Vec2 -> Bool
isOnTheLeft pointOnHull endpoint point =
    Vec2.cross (Vec2.sub endpoint pointOnHull) (Vec2.sub point pointOnHull) < -isOnTheLeftEpsilon
