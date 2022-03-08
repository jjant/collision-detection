module ConfigForm.Custom exposing (Vec2, Vec2Field, encodeVec2)

import Json.Encode as Encode exposing (Value)
import Vec2


type alias Vec2 =
    Vec2.Vec2


type alias Vec2Field =
    {}


encodeVec2 : Vec2Field -> Maybe Value
encodeVec2 _ =
    Encode.object []
        |> Just
