module Contact exposing (Contact, contactCircleCircle)

import Circle exposing (Circle)
import Isometry exposing (Isometry)
import Vec2 exposing (Vec2, distanceSquared)


type alias Contact =
    { point1 : Vec2
    , point2 : Vec2
    , normal1 : Vec2
    , normal2 : Vec2
    , dist : Float
    }


{-| If the circles are exactly in the same position, it treats the normal as being the x axis.
-}
contactCircleCircle : Isometry -> Circle -> Circle -> Float -> Maybe Contact
contactCircleCircle pos12 circle1 circle2 prediction =
    let
        r1 =
            circle1.radius

        r2 =
            circle2.radius

        center2_1 =
            pos12.translation

        distanceSquared =
            Vec2.lengthSquared center2_1

        sumRadius =
            r1 + r2

        sumRadiusWithError =
            sumRadius + prediction
    in
    if distanceSquared < sumRadiusWithError ^ 2 then
        let
            normal1 =
                if distanceSquared /= 0 then
                    Vec2.normalize center2_1

                else
                    Vec2.right

            normal2 =
                Vec2.negate (Isometry.vectorApplyInverse pos12 normal1)
        in
        Just
            { point1 = Vec2.scale r1 normal1
            , point2 = Vec2.scale r2 normal2
            , normal1 = normal1
            , normal2 = normal2
            , dist = sqrt distanceSquared - sumRadius
            }

    else
        Nothing
