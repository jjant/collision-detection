module Isometry exposing (Isometry, apply, applyInverse)

import Vec2 exposing (Vec2)


type alias Isometry =
    { translation : Vec2
    , rotation : Float
    }


apply : Isometry -> Vec2 -> Vec2
apply { translation, rotation } p =
    p
        |> Vec2.rotate rotation
        |> Vec2.add translation


applyInverse : Isometry -> Vec2 -> Vec2
applyInverse { translation, rotation } p =
    Vec2.sub p translation
        |> Vec2.rotate -rotation
