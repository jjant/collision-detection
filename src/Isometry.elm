module Isometry exposing
    ( Isometry
    , apply
    , applyInverse
    , compose
    , identity
    , invert
    , vectorApply
    , vectorApplyInverse
    )

import Vec2 exposing (Vec2)


type alias Isometry =
    { translation : Vec2
    , rotation : Float
    }


identity : Isometry
identity =
    { translation = Vec2.zero
    , rotation = 0
    }


compose : Isometry -> Isometry -> Isometry
compose second first =
    first
        |> appendRotation second.rotation
        |> appendTranslation second.translation


appendRotation : Float -> Isometry -> Isometry
appendRotation angle { rotation, translation } =
    { translation = Vec2.rotate angle translation
    , rotation = rotation + angle
    }


appendTranslation : Vec2 -> Isometry -> Isometry
appendTranslation t { translation, rotation } =
    { translation = Vec2.add t translation
    , rotation = rotation
    }


invert : Isometry -> Isometry
invert { translation, rotation } =
    { rotation = -rotation
    , translation =
        Vec2.rotate -rotation translation
            |> Vec2.negate
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


vectorApply : Isometry -> Vec2 -> Vec2
vectorApply { rotation } v =
    Vec2.rotate rotation v


vectorApplyInverse : Isometry -> Vec2 -> Vec2
vectorApplyInverse { rotation } v =
    Vec2.rotate -rotation v
