module ConfigForm.Custom exposing
    ( Vec2
    , Vec2Field
    , decodeVec2Field
    , emptyVec2
    , encodeVec2
    , viewVec2Field
    )

import ConfigForm.Options exposing (ViewOptions)
import ConfigForm.View exposing (viewFloatField)
import Element exposing (Element, column, el, fill, paddingEach, text, width)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Vec2 exposing (vec2)


type alias Vec2 =
    Vec2.Vec2


type alias Vec2Field =
    { val : Vec2
    , power : Int
    }


encodeVec2 : Vec2Field -> Maybe Value
encodeVec2 { val, power } =
    Encode.object
        [ ( "val"
          , Encode.object
                [ ( "x", Encode.float val.x )
                , ( "y", Encode.float val.y )
                ]
          )
        , ( "power", Encode.int power )
        ]
        |> Just


decodeVec2Field : Decoder Vec2Field
decodeVec2Field =
    Decode.map2 (\val power -> { val = val, power = power })
        (Decode.field "val" <|
            Decode.map2 vec2
                (Decode.field "x" Decode.float)
                (Decode.field "y" Decode.float)
        )
        (Decode.field "power" Decode.int)


emptyVec2 : { fieldName : String, label : String, getter : config -> Vec2 } -> config -> Vec2Field
emptyVec2 { getter } emptyConfig =
    { val = getter emptyConfig
    , power = 1
    }


viewVec2Field :
    { hoveredLabel : Bool -> msg
    , changedConfigForm : Vec2Field -> msg
    , label : String
    , fieldName : String
    , options : ViewOptions
    , field : Vec2Field
    , index : Int
    , isActive : Bool
    }
    -> Element msg
viewVec2Field { hoveredLabel, changedConfigForm, options, label, field, isActive } =
    let
        xField =
            { power = field.power, val = field.val.x }

        yField =
            { power = field.power, val = field.val.y }
    in
    column
        [ width fill ]
        [ el [] (text label)
        , column [ paddingEach { top = 0, left = 100, right = 0, bottom = 0 }, width fill ]
            [ viewFloatField
                { hoveredLabel = hoveredLabel
                , changedConfigForm = \newX -> changedConfigForm { field | power = newX.power, val = vec2 newX.val field.val.y }
                , options = options
                , label = "X"
                , floatField = xField
                , isActive = isActive
                }
            , viewFloatField
                { hoveredLabel = hoveredLabel
                , changedConfigForm = \newY -> changedConfigForm { field | power = newY.power, val = vec2 field.val.x newY.val }
                , options = options
                , label = "Y"
                , floatField = yField
                , isActive = isActive
                }
            ]
        ]
