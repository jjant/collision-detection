module ConfigForm.View exposing
    ( viewBoolField
    , viewColorField
    , viewFloatField
    , viewIntField
    , viewSectionField
    , viewStringField
    )

import Color
import ColorPicker
import ConfigForm.BuiltInTypes exposing (BoolFieldData, ColorFieldData, ColorFieldMeta(..), FloatFieldData, IntFieldData, StringFieldData)
import ConfigForm.Options exposing (ViewOptions)
import ConfigForm.ViewHelpers exposing (formatPoweredFloat, formatPoweredInt, inputFieldVertPadding, makePowerEl, moveFloat, moveInt, poweredFloat, poweredInt, px, pxInt, resizeAttrs, textInputHelper)
import Element exposing (Element, centerX, centerY, el, fill, height, paddingEach, paddingXY, rgb255, rgba255, row, spaceEvenly, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Misc
import UI exposing (slider)


viewStringField :
    { changedConfigForm : StringFieldData -> msg
    , label : String
    , stringField : StringFieldData
    }
    -> Element msg
viewStringField { changedConfigForm, label, stringField } =
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
            , onChange = \newStr -> changedConfigForm { stringField | val = newStr }
            }
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
    , changedConfigForm : ColorFieldData -> msg
    , options : ViewOptions
    , colorField : ColorFieldData
    , index : Int
    }
    -> Element msg
viewColorField { label, changedConfigForm, options, colorField, index } =
    row
        [ width fill
        , height (Element.px 30)
        , spacing 100
        ]
        [ Element.text label
        , row [ width fill, height fill ]
            [ closeEl changedConfigForm options colorField index
            , viewColorPicker changedConfigForm options colorField
            ]
        ]


viewBoolField :
    { options : ViewOptions
    , changedConfigForm : Bool -> msg
    , label : String
    , boolField : BoolFieldData
    }
    -> Element msg
viewBoolField { options, changedConfigForm, label, boolField } =
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
            , onChange = changedConfigForm
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


closeEl : (ColorFieldData -> msg) -> ViewOptions -> ColorFieldData -> Int -> Element msg
closeEl changedConfigForm options colorFieldData index =
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
                    (changedConfigForm { colorFieldData | meta = ColorFieldMeta { meta | isOpen = False } })

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
                , Element.htmlAttribute <| Html.Attributes.tabindex (1 + index)
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


viewColorPicker : (ColorFieldData -> msg) -> ViewOptions -> ColorFieldData -> Element msg
viewColorPicker changedConfigForm options data =
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
                            changedConfigForm
                                { data
                                    | val = newColor |> Maybe.withDefault data.val
                                    , meta = ColorFieldMeta { meta | state = newPickerState }
                                }
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
                           , Html.Events.onMouseDown (changedConfigForm { data | meta = ColorFieldMeta { meta | isOpen = True } })
                           ]
                    )
                    []
        ]


viewIntField :
    { hoveredLabel : Bool -> msg
    , changedConfigForm : IntFieldData -> msg
    , intField : IntFieldData
    , label : String
    , isActive : Bool
    , options : ViewOptions
    }
    -> Element msg
viewIntField { hoveredLabel, intField, label, isActive, changedConfigForm, options } =
    let
        onDownButton =
            { intField
                | power = intField.power - 1 |> max 0
                , val = poweredInt (intField.power - 1 |> max 0) intField.val
            }

        onUpButton =
            { intField
                | power = intField.power + 1
                , val = poweredInt (intField.power + 1) intField.val
            }
    in
    viewNumericField
        { hoveredLabel = hoveredLabel
        , field = intField
        , label = label
        , isActive = isActive
        , changedConfigForm = changedConfigForm
        , options = options
        , onDownButton = onDownButton
        , onUpButton = onUpButton
        , downButtonDisabled = intField.power <= 0
        , format = \f -> formatPoweredInt f.power f.val
        , fromString = String.toInt
        , increment = \x f -> { f | val = f.val + x }
        , move = moveInt
        , power = .power
        , setValue = \x f -> { f | val = x }
        }


viewFloatField :
    { hoveredLabel : Bool -> msg
    , changedConfigForm : FloatFieldData -> msg
    , options : ViewOptions
    , label : String
    , floatField : FloatFieldData
    , isActive : Bool
    }
    -> Element msg
viewFloatField { hoveredLabel, changedConfigForm, options, label, floatField, isActive } =
    let
        onDownButton =
            { floatField
                | power = floatField.power - 1
                , val = poweredFloat (floatField.power - 1) floatField.val
            }

        onUpButton =
            { floatField
                | power = floatField.power + 1
                , val = poweredFloat (floatField.power + 1) floatField.val
            }
    in
    viewNumericField
        { hoveredLabel = hoveredLabel
        , field = floatField
        , label = label
        , isActive = isActive
        , changedConfigForm = changedConfigForm
        , options = options
        , onDownButton = onDownButton
        , onUpButton = onUpButton
        , downButtonDisabled = False
        , format = \f -> formatPoweredFloat f.power f.val
        , fromString = String.toFloat
        , increment = \x f -> { f | val = f.val + x }
        , move = moveFloat
        , power = .power
        , setValue = \x f -> { f | val = x }
        }


viewNumericField :
    { hoveredLabel : Bool -> msg
    , changedConfigForm : fieldData -> msg
    , field : fieldData
    , move : Int -> fieldData -> number
    , power : fieldData -> Int
    , increment : number -> fieldData -> fieldData
    , setValue : number -> fieldData -> fieldData
    , format : fieldData -> String
    , label : String
    , isActive : Bool
    , options : ViewOptions
    , downButtonDisabled : Bool
    , fromString : String -> Maybe number
    , onDownButton : fieldData
    , onUpButton : fieldData
    }
    -> Element msg
viewNumericField { hoveredLabel, increment, setValue, field, fromString, label, move, format, isActive, power, changedConfigForm, options, downButtonDisabled, onDownButton, onUpButton } =
    let
        onKeyDown direction =
            changedConfigForm (increment (dirToNum direction) field)
    in
    row
        ([ width fill
         , Font.alignLeft
         , Background.color (Misc.toElementColor options.labelHighlightBgColor)
            |> Misc.attrIf isActive
         ]
            ++ resizeAttrs hoveredLabel
        )
        [ row
            [ width fill
            , height fill
            , Font.color (rgb255 33 33 33)
                |> Misc.attrIf isActive
            ]
            [ Element.el [ width fill, height fill ]
                (Element.html <|
                    slider (\dx -> changedConfigForm (setValue (move dx field) field))
                        [ Html.text label
                        , makePowerEl
                            changedConfigForm
                            options
                            (power field)
                            onDownButton
                            onUpButton
                            downButtonDisabled
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
                { label = Input.labelHidden label
                , text = format field
                , onChange =
                    \newStr ->
                        newStr
                            |> fromString
                            |> Maybe.map (\v -> setValue v field)
                            |> Maybe.withDefault field
                            |> changedConfigForm
                }
            )
        ]
