module Misc exposing
    ( attrIf
    , cursor
    , listIf
    , mouseDecoder
    , setTranslation
    , showIf
    , toElementColor
    , updateTransform
    , updateTranslation
    , userSelect
    )

import Body exposing (Body)
import Color
import Element exposing (Element)
import Html.Attributes
import Isometry exposing (Isometry)
import Json.Decode as Decode exposing (Decoder)
import Vec2 exposing (Vec2, vec2)


showIf : Bool -> Element msg -> Element msg
showIf b element =
    if b then
        element

    else
        Element.none


attrIf : Bool -> Element.Attribute msg -> Element.Attribute msg
attrIf b attr =
    if b then
        attr

    else
        Element.htmlAttribute (Html.Attributes.class "")


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


updateTransform : (Isometry -> Isometry) -> Body -> Body
updateTransform fn body =
    { body | transform = fn body.transform }


updateTranslation : (Vec2 -> Vec2) -> Body -> Body
updateTranslation fn body =
    updateTransform (\transform -> { transform | translation = fn transform.translation }) body


setTranslation : Vec2 -> Body -> Body
setTranslation translation =
    updateTranslation (\_ -> translation)


toElementColor : Color.Color -> Element.Color
toElementColor c =
    let
        { red, green, blue, alpha } =
            Color.toRgba c
    in
    Element.rgba red green blue alpha


userSelect : String -> Element.Attribute msg
userSelect value =
    Element.htmlAttribute <| Html.Attributes.style "user-select" value


cursor : String -> Element.Attribute msg
cursor value =
    Element.htmlAttribute <| Html.Attributes.style "cursor" value
