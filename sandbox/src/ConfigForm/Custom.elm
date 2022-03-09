module ConfigForm.Custom exposing (Vec2, Vec2Field, decodeVec2Field, emptyVec2, encodeVec2)

import Json.Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Vec2


type alias Vec2 =
    Vec2.Vec2


type alias Vec2Field =
    { val : Vec2 }


encodeVec2 : Vec2Field -> Maybe Value
encodeVec2 _ =
    Encode.object []
        |> Just


emptyVec2 : { fieldName : String, label : String, getter : config -> Vec2 } -> config -> Vec2Field
emptyVec2 { getter } emptyConfig =
    { val = getter emptyConfig }


decodeVec2Field : Decoder a
decodeVec2Field =
    Debug.todo "TODO"
