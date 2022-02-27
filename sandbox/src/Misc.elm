module Misc exposing (listIf, mouseDecoder)

import Json.Decode as Decode exposing (Decoder)
import Vec2 exposing (Vec2, vec2)


listIf : Bool -> List a -> List a
listIf b l =
    if b then
        l

    else
        []


mouseDecoder : Decoder Vec2
mouseDecoder =
    Decode.map2 vec2
        (Decode.at [ "offsetX" ] Decode.float)
        (Decode.at [ "offsetY" ] Decode.float)
