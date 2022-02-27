module Misc exposing (listIf, mouseDecoder, setTranslation, updateTranslation)

import Body exposing (Body)
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


updateTranslation : (Vec2 -> Vec2) -> Body -> Body
updateTranslation fn body =
    let
        transform =
            body.transform
    in
    { body | transform = { transform | translation = fn transform.translation } }


setTranslation : Vec2 -> Body -> Body
setTranslation translation =
    updateTranslation (\_ -> translation)
