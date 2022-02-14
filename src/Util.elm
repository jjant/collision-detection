module Util exposing (componentWiseMax)

import Vec2 exposing (Vec2, vec2)


componentWiseMax : Vec2 -> Vec2 -> Vec2
componentWiseMax v1 v2 =
    vec2 (max v1.x v2.x) (max v1.y v2.y)
