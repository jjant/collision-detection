module ConfigForm exposing
    ( viewOptions, withFontSize, withRowSpacing, withInputWidth, withInputSpacing, withLabelHighlightBgColor, withSectionSpacing
    , int, float, string, bool, color, section
    , colorValDecoder, encodeColor, formatPoweredFloat, formatPoweredInt, tuple2Encoder, viewBoolField, viewColorField, viewFloatField, viewIntField, viewSectionField, viewStringField
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
import ConfigForm.BuiltInTypes exposing (FloatFieldData)
import ConfigForm.UI exposing (ViewOptions, inputFieldVertPadding, makePowerEl, moveFloat, moveInt, poweredFloat, px, pxInt, resizeAttrs, textInputHelper)
import ConfigTypes exposing (BoolFieldData, ColorFieldData, ColorFieldMeta(..), Field(..), IntFieldData, Logic, LogicKind(..), StringFieldData)
import Element exposing (Element, centerX, centerY, el, fill, height, paddingEach, paddingXY, rgb255, rgba255, row, spaceEvenly, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as JE
import Misc
import Round
import UI exposing (slider)
import Vec2 exposing (direction)


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



-- JSON encode/decoder stuff


colorValDecoder : Decoder Color
colorValDecoder =
    Decode.map4 Color.rgba
        (Decode.field "r" Decode.float)
        (Decode.field "g" Decode.float)
        (Decode.field "b" Decode.float)
        (Decode.field "a" Decode.float)



-- VIEW


viewStringField : { changedConfigForm : String -> Field -> msg, label : String, fieldName : String, stringField : StringFieldData } -> Element msg
viewStringField { changedConfigForm, label, fieldName, stringField } =
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
                        (Element.text label)
                    )
            , text = stringField.val
            , onChange =
                \newStr ->
                    changedConfigForm fieldName (StringField { stringField | val = newStr })
            }
        ]


viewIntField :
    { hoveredLabel : String -> Bool -> msg
    , changedConfigForm : IntFieldData -> msg
    , fieldName : String
    , intField : IntFieldData
    , label : String
    , isActive : Bool
    , options : ViewOptions
    }
    -> Element msg
viewIntField { hoveredLabel, fieldName, intField, label, isActive, changedConfigForm, options } =
    let
        onKeyDown direction =
            changedConfigForm { intField | val = intField.val + dirToNum direction }
    in
    row
        ([ width fill
         , Font.alignLeft
         , Background.color (Misc.toElementColor viewOptions.labelHighlightBgColor)
            |> Misc.attrIf isActive
         ]
            ++ resizeAttrs (hoveredLabel fieldName)
        )
        [ row
            [ width fill
            , height fill
            , Font.color (rgb255 33 33 33)
                |> Misc.attrIf isActive
            ]
            [ Element.el [ width fill, height fill ]
                (Element.html <|
                    slider (\dx -> changedConfigForm { intField | val = moveInt dx intField })
                        [ Html.text label
                        , makePowerEl
                            changedConfigForm
                            options
                            intField.power
                            { intField
                                | power = intField.power - 1 |> max 0
                                , val = poweredInt (intField.power - 1 |> max 0) intField.val
                            }
                            { intField
                                | power = intField.power + 1
                                , val = poweredInt (intField.power + 1) intField.val
                            }
                            (intField.power <= 0)
                            |> Misc.showHtmlIf isActive
                        ]
                )
            ]
        , Element.el
            [ width (fill |> Element.maximum 100)
            , paddingXY 2 2
            ]
            (textInputHelper
                (Font.center
                    :: incrementalAttrs onKeyDown
                )
                { label = Input.labelHidden fieldName
                , text = formatPoweredInt intField.power intField.val
                , onChange =
                    \newStr ->
                        changedConfigForm
                            { intField
                                | val =
                                    case String.toInt newStr of
                                        Just num ->
                                            num

                                        Nothing ->
                                            intField.val
                            }
                }
            )
        ]


dirToNum : Direction -> number
dirToNum direction =
    case direction of
        Up ->
            1

        Down ->
            -1


viewColorField :
    { label : String
    , changedConfigForm : String -> Field -> msg
    , options : ViewOptions
    , colorField : ColorFieldData
    , fieldName : String
    , index : Int
    }
    -> Element msg
viewColorField { label, changedConfigForm, options, colorField, fieldName, index } =
    row
        [ width fill
        , height (Element.px 30)
        , spacing 100
        ]
        [ Element.text label
        , row [ width fill, height fill ]
            [ closeEl changedConfigForm options colorField index fieldName
            , viewColorPicker changedConfigForm options colorField fieldName
            ]
        ]


viewFloatField :
    { hoveredLabel : String -> Bool -> msg
    , changedConfigForm : FloatFieldData -> msg
    , options : ViewOptions
    , fieldName : String
    , label : String
    , floatField : FloatFieldData
    , isActive : Bool
    }
    -> Element msg
viewFloatField { hoveredLabel, changedConfigForm, options, fieldName, label, floatField, isActive } =
    let
        onKeyDown direction =
            changedConfigForm { floatField | val = floatField.val + dirToNum direction }
    in
    row (width fill :: resizeAttrs (hoveredLabel fieldName))
        [ Element.row
            [ width fill
            , height fill
            , paddingXY 5 0
            , Font.color (rgb255 33 33 33)
                |> Misc.attrIf isActive
            , Background.color (Misc.toElementColor viewOptions.labelHighlightBgColor)
                |> Misc.attrIf isActive
            ]
            [ Element.html <| slider (\dx -> changedConfigForm { floatField | val = moveFloat dx floatField }) [ Html.text label ]
            , Element.html <|
                (makePowerEl
                    changedConfigForm
                    options
                    floatField.power
                    { floatField
                        | power = floatField.power - 1
                        , val = poweredFloat (floatField.power - 1) floatField.val
                    }
                    { floatField
                        | power = floatField.power + 1
                        , val = poweredFloat (floatField.power + 1) floatField.val
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
                (Font.center :: incrementalAttrs onKeyDown)
                { label = Input.labelHidden fieldName
                , text = formatPoweredFloat floatField.power floatField.val
                , onChange =
                    \newStr ->
                        changedConfigForm
                            { floatField
                                | val =
                                    case String.toFloat newStr of
                                        Just num ->
                                            num

                                        Nothing ->
                                            floatField.val
                            }
                }
            )
        ]


viewBoolField : { options : ViewOptions, changedConfigForm : String -> Field -> msg, fieldName : String, label : String, boolField : BoolFieldData } -> Element msg
viewBoolField { options, changedConfigForm, fieldName, label, boolField } =
    let
        defaultAttrs =
            [ style "width" (px (inputFieldVertPadding options))
            , style "height" (px (inputFieldVertPadding options))
            ]
    in
    row
        [ width fill
        , spaceEvenly
        , centerY
        ]
        [ Input.checkbox
            [ width fill ]
            { checked = boolField.val
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
            , onChange = \newBool -> changedConfigForm fieldName (BoolField { boolField | val = newBool })
            , label =
                Input.labelLeft
                    [ Font.alignLeft
                    , width fill
                    , centerY
                    , Misc.userSelect "none"
                    ]
                    (Element.text label)
            }
        ]


viewSectionField : { options : ViewOptions, label : String } -> Element msg
viewSectionField { options, label } =
    row
        [ Font.bold
        , paddingEach
            { top = options.sectionSpacing
            , right = 0
            , bottom = 5
            , left = 0
            }
        ]
        [ Element.text label ]


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


type Direction
    = Up
    | Down


directionDecoder : Decoder Direction
directionDecoder =
    Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowUp" ->
                        Decode.succeed Up

                    "ArrowDown" ->
                        Decode.succeed Down

                    _ ->
                        Decode.fail "Ignored key"
            )


incrementalAttrs : (Direction -> msg) -> List (Element.Attribute msg)
incrementalAttrs onKey =
    [ Element.htmlAttribute <|
        Html.Events.on "keydown" (Decode.map onKey <| Decode.field "key" directionDecoder)
    , Element.htmlAttribute <| style "font-variant-numeric" "tabular-nums"
    ]


viewColorPicker : (String -> Field -> msg) -> ViewOptions -> ColorFieldData -> String -> Element msg
viewColorPicker changedConfigForm options data fieldName =
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
    row
        [ width fill
        , height fill
        , Element.htmlAttribute <| style "z-index" "2"
        ]
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
                           , style "height" "100%"
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



-- VIEW OPTIONS


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
