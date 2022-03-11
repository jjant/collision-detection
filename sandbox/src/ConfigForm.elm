module ConfigForm exposing
    ( viewOptions, withFontSize, withRowSpacing, withInputWidth, withInputSpacing, withLabelHighlightBgColor, withSectionSpacing
    , int, float, string, bool, color, section
    , ViewOptions, colorValDecoder, encodeColor, formatPoweredFloat, formatPoweredInt, tuple2Encoder, viewField
    )

{-| Note: The `config` in the following type signatures is a record of all your config values, like...

    type alias Config =
        { headerFontSize : Int
        , bodyFontSize : Int
        , backgroundColor : Color
        }

Also, `Value` is shorthand for `Json.Encode.Value`.

@docs ConfigForm, init, InitOptions


# Msg

@docs Msg


# Update

@docs update


# Encoding

@docs encode


# View

@docs view


# View options

@docs viewOptions, withFontSize, withRowSpacing, withInputWidth, withInputSpacing, withLabelHighlightBgColor, withSectionSpacing


# Used only by generated Config code

@docs int, float, string, bool, color, section

-}

import Color exposing (Color)
import ColorPicker
import ConfigTypes exposing (ColorFieldData, ColorFieldMeta(..), Field(..), Logic, LogicKind(..))
import Element exposing (Element, centerX, centerY, el, fill, height, paddingEach, paddingXY, rgb255, rgba255, row, spaceEvenly, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes exposing (style)
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as JE
import Misc
import Round
import UI exposing (slider)


{-| Creates the logic for Int values
-}
int : String -> String -> (config -> Int) -> (Int -> config -> config) -> Logic config
int fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = IntLogic { getter = getter, setter = setter }
    }


{-| Creates the logic for Float values
-}
float : String -> String -> (config -> Float) -> (Float -> config -> config) -> Logic config
float fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = FloatLogic { getter = getter, setter = setter }
    }


{-| Creates the logic for String values
-}
string : String -> String -> (config -> String) -> (String -> config -> config) -> Logic config
string fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = StringLogic { getter = getter, setter = setter }
    }


{-| Creates the logic for Bool values
-}
bool : String -> String -> (config -> Bool) -> (Bool -> config -> config) -> Logic config
bool fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = BoolLogic { getter = getter, setter = setter }
    }


{-| Creates the logic for Color values
-}
color : String -> String -> (config -> Color) -> (Color -> config -> config) -> Logic config
color fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = ColorLogic { getter = getter, setter = setter }
    }


{-| Creates the logic for Section values
-}
section : String -> Logic config
section sectionStr =
    { fieldName = ""
    , label = sectionStr
    , kind =
        SectionLogic
            { getter = \_ -> ()
            , setter = \_ config -> config
            }
    }


encodeColor : Color -> JE.Value
encodeColor col =
    col
        |> Color.toRgba
        |> (\{ red, green, blue, alpha } ->
                JE.object
                    [ ( "r", JE.float red )
                    , ( "g", JE.float green )
                    , ( "b", JE.float blue )
                    , ( "a", JE.float alpha )
                    ]
           )


tuple2Encoder : (a -> JE.Value) -> (b -> JE.Value) -> ( a, b ) -> JE.Value
tuple2Encoder enc1 enc2 ( val1, val2 ) =
    -- from https://stackoverflow.com/a/52676142
    JE.list identity [ enc1 val1, enc2 val2 ]


formatPoweredInt : Int -> Int -> String
formatPoweredInt power val =
    Round.round -power (toFloat val)


formatPoweredFloat : Int -> Float -> String
formatPoweredFloat power val =
    Round.round -power val


poweredInt : Int -> Int -> Int
poweredInt power val =
    round <| Round.roundNum -power (toFloat val)


poweredFloat : Int -> Float -> Float
poweredFloat power val =
    Round.roundNum -power val



-- JSON encode/decoder stuff


colorValDecoder : Decoder Color
colorValDecoder =
    Decode.map4 Color.rgba
        (Decode.field "r" Decode.float)
        (Decode.field "g" Decode.float)
        (Decode.field "b" Decode.float)
        (Decode.field "a" Decode.float)



-- VIEW


viewField :
    { hoveredLabel : String -> Bool -> msg
    , onMouseMove : Int -> msg
    , changedConfigForm : String -> Field -> msg
    }
    -> ViewOptions
    -> Field
    -> Int
    -> ConfigTypes.Logic config
    -> Bool
    -> Element msg
viewField { hoveredLabel, onMouseMove, changedConfigForm } options field i logic isActive =
    let
        defaultAttrs =
            [ style "width" (px (inputFieldVertPadding options))

            -- style "width" (pxInt options.inputWidth)
            , style "height" (px (inputFieldVertPadding options))
            ]
    in
    case field of
        StringField stringField ->
            row [ width fill, spaceEvenly ]
                [ textInputHelper
                    []
                    { label =
                        Input.labelLeft
                            [ width fill
                            , height fill
                            , Font.alignLeft
                            , Misc.userSelect "none"
                            , Element.pointer
                            ]
                            (Element.el [ width fill, centerY ]
                                (Element.text logic.label)
                            )
                    , valStr = stringField.val
                    , onChange =
                        \newStr ->
                            changedConfigForm logic.fieldName (StringField { stringField | val = newStr })
                    }
                ]

        IntField intField ->
            row ([ width fill, Font.alignLeft ] ++ resizeAttrs hoveredLabel logic)
                [ row
                    [ width fill
                    , height fill
                    , paddingXY 5 0
                    , Font.color (rgb255 33 33 33)
                        |> Misc.attrIf isActive
                    , Background.color (Misc.toElementColor viewOptions.labelHighlightBgColor)
                        |> Misc.attrIf isActive
                    ]
                    [ Element.el [ width fill, height fill ] (Element.html <| slider onMouseMove [ Html.text logic.label ])
                    , makePowerEl changedConfigForm
                        options
                        logic.fieldName
                        intField.power
                        (IntField
                            { intField
                                | power = intField.power - 1 |> max 0
                                , str = formatPoweredInt (intField.power - 1 |> max 0) intField.val
                                , val = poweredInt (intField.power - 1 |> max 0) intField.val
                            }
                        )
                        (IntField
                            { intField
                                | power = intField.power + 1
                                , str = formatPoweredInt (intField.power + 1) intField.val
                                , val = poweredInt (intField.power + 1) intField.val
                            }
                        )
                        (intField.power <= 0)
                        |> Misc.showIf isActive
                    ]
                , Element.el
                    [ width
                        (fill
                            |> Element.maximum 100
                        )
                    ]
                    (textInputHelper
                        (Font.center :: incrementalAttrs changedConfigForm String.fromInt IntField logic.fieldName intField)
                        { label = Input.labelHidden logic.fieldName
                        , valStr = intField.str
                        , onChange =
                            \newStr ->
                                changedConfigForm logic.fieldName <|
                                    IntField
                                        { intField
                                            | str = newStr
                                            , val =
                                                case String.toInt newStr of
                                                    Just num ->
                                                        num

                                                    Nothing ->
                                                        intField.val
                                        }
                        }
                    )
                ]

        FloatField floatField ->
            row (width fill :: resizeAttrs hoveredLabel logic)
                [ Element.row
                    [ width fill
                    , height fill
                    , paddingXY 5 0
                    , Font.color (rgb255 33 33 33)
                        |> Misc.attrIf isActive
                    , Background.color (Misc.toElementColor viewOptions.labelHighlightBgColor)
                        |> Misc.attrIf isActive
                    ]
                    [ Element.html <| slider onMouseMove [ Html.text logic.label ]
                    , makePowerEl changedConfigForm
                        options
                        logic.fieldName
                        floatField.power
                        (FloatField
                            { floatField
                                | power = floatField.power - 1
                                , str = formatPoweredFloat (floatField.power - 1) floatField.val
                                , val = poweredFloat (floatField.power - 1) floatField.val
                            }
                        )
                        (FloatField
                            { floatField
                                | power = floatField.power + 1
                                , str = formatPoweredFloat (floatField.power + 1) floatField.val
                                , val = poweredFloat (floatField.power + 1) floatField.val
                            }
                        )
                        False
                        |> Misc.showIf isActive
                    ]
                , Element.el
                    [ width
                        (fill
                            |> Element.maximum 100
                        )
                    ]
                    (textInputHelper
                        (Font.center :: incrementalAttrs changedConfigForm String.fromFloat FloatField logic.fieldName floatField)
                        { label = Input.labelHidden logic.fieldName
                        , valStr = floatField.str
                        , onChange =
                            \newStr ->
                                changedConfigForm logic.fieldName <|
                                    FloatField
                                        { floatField
                                            | str = newStr
                                            , val =
                                                case String.toFloat newStr of
                                                    Just num ->
                                                        num

                                                    Nothing ->
                                                        floatField.val
                                        }
                        }
                    )
                ]

        BoolField ({ val } as boolField) ->
            row
                [ width fill
                , spaceEvenly
                , centerY
                ]
                [ Input.checkbox
                    [ width fill ]
                    { checked = val
                    , icon =
                        \b ->
                            Element.html <|
                                Html.input
                                    (defaultAttrs
                                        ++ [ Html.Attributes.type_ "checkbox"
                                           , Html.Attributes.checked b
                                           , Html.Attributes.style "margin-left" "auto"
                                           , Html.Attributes.style "cursor" "pointer"
                                           ]
                                    )
                                    []
                    , onChange = \newBool -> changedConfigForm logic.fieldName (BoolField { boolField | val = newBool })
                    , label =
                        Input.labelLeft
                            [ Font.alignLeft
                            , width fill
                            , centerY
                            , Misc.userSelect "none"
                            ]
                            (Element.text logic.label)
                    }
                ]

        ColorField colorField ->
            row
                [ width fill ]
                [ Element.text logic.label
                , closeEl changedConfigForm options colorField i logic.fieldName
                , viewColorField changedConfigForm options colorField logic.fieldName
                ]

        SectionField _ ->
            row
                [ Font.bold
                , paddingEach
                    { top = options.sectionSpacing
                    , right = 0
                    , bottom = 5
                    , left = 0
                    }
                ]
                [ Element.text logic.label ]


closeEl : (String -> Field -> msg) -> { r | fontSize : Int } -> ColorFieldData -> Int -> String -> Element msg
closeEl changedConfigForm options colorFieldData i fieldName =
    let
        maybeCloseMsg =
            let
                meta =
                    case colorFieldData.meta of
                        ColorFieldMeta metaData ->
                            metaData
            in
            if meta.isOpen then
                Just
                    (changedConfigForm
                        fieldName
                        (ColorField { colorFieldData | meta = ColorFieldMeta { meta | isOpen = False } })
                    )

            else
                Nothing
    in
    case maybeCloseMsg of
        Just msg ->
            Input.button
                [ Background.color (rgba255 255 255 255 0.2)
                , Border.width 1
                , Border.color (rgba255 0 0 0 0.9)
                , Border.rounded 4
                , width (Element.px (round (1.5 * toFloat options.fontSize)))
                , height (Element.px (round (1.5 * toFloat options.fontSize)))
                , Element.htmlAttribute <| Html.Attributes.tabindex (1 + i)
                ]
                { onPress = Just msg
                , label = el [ centerX, centerY ] (Element.text "❌")
                }

        Nothing ->
            Element.none


makePowerEl : (String -> Field -> msg) -> ViewOptions -> String -> Int -> Field -> Field -> Bool -> Element msg
makePowerEl changedConfigForm options fieldName power newIncField newDecField isDownDisabled =
    Element.el [ Element.alignRight ] <|
        Element.html <|
            Html.div
                [ style "height" "100%"
                , style "box-sizing" "border-box"
                , style "display" "flex"
                , style "align-items" "center"
                , style "padding-left" (px (0.45 * inputFieldVertPadding options))
                , style "font-size" (px (0.8 * toFloat options.fontSize))
                , style "background" (Color.toCssString options.labelHighlightBgColor)
                , style "background"
                    ([ "linear-gradient(to right,"
                     , "transparent,"
                     , Color.toCssString options.labelHighlightBgColor ++ " 10%,"
                     , Color.toCssString options.labelHighlightBgColor
                     ]
                        |> String.join " "
                    )
                , style "pointer-events" "none"
                ]
                [ Html.span
                    [ style "padding" "5px 0"
                    , style "pointer-events" "none"
                    , style "user-select" "none"
                    ]
                    -- label
                    [ Html.text (formattedPower power) ]
                , Html.span
                    [ style "font-size" (0.8 * toFloat options.fontSize |> px)
                    , style "top" "1px"
                    , style "pointer-events" "all"
                    , Pointer.onWithOptions "pointerdown"
                        { stopPropagation = True
                        , preventDefault = True
                        }
                        (\_ -> changedConfigForm fieldName newIncField)
                    , if isDownDisabled then
                        style "opacity" "0.4"

                      else
                        style "cursor" "pointer"
                    ]
                    -- down btn
                    [ Html.text "↙️" ]
                , Html.span
                    [ style "font-size" (0.8 * toFloat options.fontSize |> px)
                    , style "top" "1px"
                    , style "pointer-events" "all"
                    , Pointer.onWithOptions "pointerdown"
                        { stopPropagation = True
                        , preventDefault = True
                        }
                        (\_ -> changedConfigForm fieldName newDecField)
                    , style "cursor" "pointer"
                    ]
                    -- up btn
                    [ Html.text "↗️" ]
                ]


resizeAttrs : (String -> Bool -> msg) -> Logic config -> List (Element.Attribute msg)
resizeAttrs hoveredLabel logic =
    [ Events.onMouseEnter (hoveredLabel logic.fieldName True)
    , Events.onMouseLeave (hoveredLabel logic.fieldName False)

    --, Html.Events.onMouseDown (ClickedPointerLockLabel logic.fieldName)
    , Misc.cursor "ew-resize"
    ]


inputFieldVertPadding : ViewOptions -> Float
inputFieldVertPadding options =
    toFloat options.fontSize * options.inputSpacing


incrementalAttrs : (String -> Field -> msg) -> (number -> String) -> ({ r | val : number, str : String } -> Field) -> String -> { r | val : number, str : String } -> List (Element.Attribute msg)
incrementalAttrs changedConfigForm numToString wrapper fieldName data =
    [ Element.htmlAttribute <|
        Html.Events.on "keydown"
            (Decode.map
                (\key ->
                    let
                        maybeNewNum =
                            case key of
                                "ArrowUp" ->
                                    Just <| data.val + 1

                                "ArrowDown" ->
                                    Just <| data.val - 1

                                _ ->
                                    Nothing
                    in
                    changedConfigForm fieldName
                        (wrapper
                            (case maybeNewNum of
                                Just newNum ->
                                    { data
                                        | val = newNum
                                        , str = numToString newNum
                                    }

                                Nothing ->
                                    data
                            )
                        )
                )
                (Decode.field "key" Decode.string)
            )
    , Element.htmlAttribute <| style "font-variant-numeric" "tabular-nums"
    ]


viewColorField : (String -> Field -> msg) -> ViewOptions -> ColorFieldData -> String -> Element msg
viewColorField changedConfigForm options data fieldName =
    let
        meta =
            case data.meta of
                ColorFieldMeta m ->
                    m

        defaultAttrs =
            [ style "width" (pxInt options.inputWidth)
            , style "height" (px (inputFieldVertPadding options))
            ]
    in
    row [ width fill ]
        [ Element.html <|
            if meta.isOpen then
                ColorPicker.view data.val meta.state
                    |> Html.map
                        (\pickerMsg ->
                            let
                                ( newPickerState, newColor ) =
                                    ColorPicker.update
                                        pickerMsg
                                        data.val
                                        meta.state
                            in
                            changedConfigForm fieldName
                                (ColorField
                                    { data
                                        | val = newColor |> Maybe.withDefault data.val
                                        , meta =
                                            ColorFieldMeta
                                                { state = newPickerState
                                                , isOpen = meta.isOpen
                                                }
                                    }
                                )
                        )

            else
                Html.div
                    (defaultAttrs
                        ++ [ style "background" (Color.toCssString data.val)
                           , style "width" "100%"
                           , style "border" "1px solid rgba(0,0,0,0.3)"
                           , style "border-radius" "3px"
                           , style "box-sizing" "border-box"
                           , Html.Events.onMouseDown
                                (changedConfigForm
                                    fieldName
                                    (ColorField
                                        { data
                                            | meta =
                                                ColorFieldMeta
                                                    { state = meta.state
                                                    , isOpen = True
                                                    }
                                        }
                                    )
                                )
                           ]
                    )
                    []
        ]


textInputHelper :
    List (Element.Attribute msg)
    ->
        { label : Input.Label msg
        , valStr : String
        , onChange : String -> msg
        }
    -> Element msg
textInputHelper attrs { label, valStr, onChange } =
    Input.text (Background.color (rgb255 60 72 85) :: attrs)
        { onChange = onChange
        , placeholder = Nothing
        , text = valStr
        , label = label
        }



-- VIEW OPTIONS


{-| Options for viewing the config form.
-}
type alias ViewOptions =
    { fontSize : Int
    , rowSpacing : Int
    , inputWidth : Int
    , inputSpacing : Float
    , labelHighlightBgColor : Color
    , sectionSpacing : Int
    }


{-| Default options for viewing the config form.
-}
viewOptions : ViewOptions
viewOptions =
    { fontSize = 18
    , rowSpacing = 2
    , inputWidth = 80
    , inputSpacing = 1.4
    , labelHighlightBgColor = Color.rgb 0.8 0.8 1
    , sectionSpacing = 10
    }


{-| Update the font size in px. Default is 18.
-}
withFontSize : Int -> ViewOptions -> ViewOptions
withFontSize val options =
    { options | fontSize = val }


{-| Update the row spacing in px. Default is 2.
-}
withRowSpacing : Int -> ViewOptions -> ViewOptions
withRowSpacing val options =
    { options | rowSpacing = val }


{-| Update the width of inputs in px. Default is 80.
-}
withInputWidth : Int -> ViewOptions -> ViewOptions
withInputWidth val options =
    { options | inputWidth = val }


{-| Update the inner spacing of inputs by a ratio of its font size. Default is 1.40.
-}
withInputSpacing : Float -> ViewOptions -> ViewOptions
withInputSpacing val options =
    { options | inputSpacing = val }


{-| Update the row color when hovering field labels that are pointerlock-able. Default is yellow: (0.8, 0.8, 1).
-}
withLabelHighlightBgColor : Color -> ViewOptions -> ViewOptions
withLabelHighlightBgColor val options =
    { options | labelHighlightBgColor = val }


{-| Update the extra top spacing for sections in px. Default is 20.
-}
withSectionSpacing : Int -> ViewOptions -> ViewOptions
withSectionSpacing val options =
    { options | sectionSpacing = val }



-- MISC INTERNAL


px : Float -> String
px num =
    String.fromFloat num ++ "px"


pxInt : Int -> String
pxInt num =
    String.fromInt num ++ "px"


formattedPower : Int -> String
formattedPower power =
    let
        numStr =
            if power >= 0 then
                String.fromInt (10 ^ power)

            else
                "0." ++ String.repeat (-1 - power) "0" ++ "1"
    in
    "x" ++ numStr
