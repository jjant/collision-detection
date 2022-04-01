module CSOPoint exposing (CSOPoint, new, origin)

import Vec2 exposing (Vec2)


{-| "Configuration space obstacle" point.
In other words, a point in the Minkowski difference,
together with the points from the bodies that generated them:

    point =
        orig1 - orig2

(I think)

-}
type alias CSOPoint =
    { point : Vec2
    , orig1 : Vec2

    -- For the moment, both points (orig1, orig2) are in body1's local space.
    , orig2 : Vec2
    }


new : Vec2 -> Vec2 -> CSOPoint
new orig1 orig2 =
    let
        point =
            Vec2.sub orig1 orig2
    in
    { point = point, orig1 = orig1, orig2 = orig2 }


origin : CSOPoint
origin =
    new Vec2.zero Vec2.zero
