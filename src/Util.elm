module Util exposing
    ( componentWiseMax
    , copySign
    , copySign2
    )

import Vec2 exposing (Vec2, vec2)


componentWiseMax : Vec2 -> Vec2 -> Vec2
componentWiseMax v1 v2 =
    vec2 (max v1.x v2.x) (max v1.y v2.y)


copySign : { from : number, to : number } -> number
copySign { from, to } =
    if from >= 0 && to >= 0 then
        to

    else if from < 0 && to < 0 then
        to

    else
        -to


copySign2 : { from : Vec2, to : Vec2 } -> Vec2
copySign2 { from, to } =
    vec2
        (copySign { from = from.x, to = to.x })
        (copySign { from = from.y, to = to.y })
