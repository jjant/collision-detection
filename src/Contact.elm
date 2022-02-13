module Contact exposing (Contact, contactCircleCircle)

import Circle exposing (Circle)
import Vec2 exposing (Vec2, distanceSquared)


type alias Contact =
    { world1 : Vec2
    , world2 : Vec2
    , normal : Vec2
    , depth : Float
    }


{-| If the circles are exactly in the same position, it treats the normal as being the x axis.
-}
contactCircleCircle : Vec2 -> Circle -> Vec2 -> Circle -> Maybe Contact
contactCircleCircle center1 circle1 center2 circle2 =
    let
        r1 =
            circle1.radius

        r2 =
            circle2.radius

        deltaPos =
            Vec2.sub center2 center1

        distanceSquared =
            Vec2.lengthSquared deltaPos

        sumRadius =
            r1 + r2
    in
    if distanceSquared < sumRadius ^ 2 then
        let
            normal =
                if deltaPos /= Vec2.zero then
                    Vec2.normalize deltaPos

                else
                    Vec2.right
        in
        Just
            { world1 = Vec2.add center1 (Vec2.scale r1 normal)
            , world2 = Vec2.add center2 (Vec2.scale -r2 normal)
            , normal = normal
            , depth = sumRadius - sqrt distanceSquared
            }

    else
        Nothing
