module ConfigForm.Custom exposing
    ( Vec2
    , Vec2Field
    , decodeVec2Field
    , emptyVec2
    , encodeVec2
    , viewVec2Field
    )

import ConfigForm.UI exposing (ViewOptions, makePowerEl, moveFloat, poweredFloat, resizeAttrs, textInputHelper)
import Element exposing (Element, fill, height, paddingXY, rgb255, row, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Misc
import UI exposing (slider)
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
viewVec2Field { hoveredLabel, changedConfigForm, options, fieldName, label, field, isActive } =
    row
        (width fill :: resizeAttrs hoveredLabel)
        [ Element.row
            [ width fill
            , height fill
            , paddingXY 5 0
            , Font.color (rgb255 33 33 33)
                |> Misc.attrIf isActive
            , Background.color (Misc.toElementColor options.labelHighlightBgColor)
                |> Misc.attrIf isActive
            ]
            [ Element.html <|
                slider (\i -> changedConfigForm { field | val = vec2 (moveFloat i { val = field.val.x, power = field.power }) field.val.y })
                    [ Html.text label ]
            , Element.html <|
                (makePowerEl
                    changedConfigForm
                    options
                    field.power
                    { field
                        | power = field.power - 1
                        , val = vec2 (poweredFloat (field.power - 1) field.val.x) field.val.y
                    }
                    { field
                        | power = field.power + 1
                        , val = vec2 (poweredFloat (field.power + 1) field.val.x) field.val.y
                    }
                    False
                    |> Misc.showHtmlIf isActive
                )
            ]
        , Element.el
            [ width
                (fill
                    |> Element.maximum 100
                )
            ]
            (textInputHelper
                [ Font.center ]
                -- (Font.center :: incrementalAttrs changedConfigForm String.fromFloat FloatField fieldName floatField)
                { label = Input.labelHidden fieldName
                , text = "(" ++ String.fromFloat field.val.x ++ ", " ++ String.fromFloat field.val.y ++ ")"
                , onChange =
                    \newStr ->
                        changedConfigForm <|
                            { field
                                | val =
                                    case String.toFloat newStr of
                                        Just newX ->
                                            Vec2.setX newX field.val

                                        Nothing ->
                                            field.val
                            }
                }
            )
        ]
