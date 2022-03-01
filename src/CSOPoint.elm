module CSOPoint exposing (CSOPoint, new, origin)

import Vec2 exposing (Vec2)


type alias CSOPoint =
    { point : Vec2
    , orig1 : Vec2
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
