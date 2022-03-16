module ConfigForm.UI exposing
    ( ViewOptions
    , formattedPower
    , inputFieldVertPadding
    , makePowerEl
    , moveFloat
    , moveInt
    , poweredFloat
    , px
    , pxInt
    , resizeAttrs
    , textInputHelper
    )

import Color exposing (Color)
import Element exposing (Element, rgb255)
import Element.Background as Background
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Round


type alias ViewOptions =
    { fontSize : Int
    , rowSpacing : Int
    , inputWidth : Int
    , inputSpacing : Float
    , labelHighlightBgColor : Color
    , sectionSpacing : Int
    }


makePowerEl : (field -> msg) -> ViewOptions -> Int -> field -> field -> Bool -> Html msg
makePowerEl changedConfigForm options power newIncField newDecField isDownDisabled =
    Html.div
        [ style "margin-left" "auto"
        , style "height" "100%"
        , style "box-sizing" "border-box"
        , style "display" "flex"
        , style "align-items" "center"
        , style "padding-right" "5px"
        , style "font-size" (px (0.8 * toFloat options.fontSize))
        , style "pointer-events" "none"
        ]
        [ Html.span
            [ style "padding" "0 5px"
            , style "pointer-events" "none"
            , style "user-select" "none"
            ]
            -- label
            [ Html.text (formattedPower power) ]
        , Html.span
            ([ style "font-size" (0.8 * toFloat options.fontSize |> px)
             , style "top" "1px"
             , style "pointer-events" "all"
             , Pointer.onWithOptions "pointerdown"
                { stopPropagation = True
                , preventDefault = True
                }
                (\_ -> changedConfigForm newIncField)
             ]
                ++ (if isDownDisabled then
                        [ style "cursor" "not-allowed", style "opacity" "0.4" ]

                    else
                        [ style "cursor" "pointer" ]
                   )
            )
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
                (\_ -> changedConfigForm newDecField)
            , style "cursor" "pointer"
            ]
            -- up btn
            [ Html.text "↗️" ]
        ]


textInputHelper : List (Element.Attribute msg) -> { label : Input.Label msg, text : String, onChange : String -> msg } -> Element msg
textInputHelper attrs { label, text, onChange } =
    Input.text (Background.color (rgb255 60 72 85) :: attrs)
        { onChange = onChange
        , placeholder = Nothing
        , text = text
        , label = label
        }


px : Float -> String
px num =
    String.fromFloat num ++ "px"


pxInt : Int -> String
pxInt num =
    String.fromInt num ++ "px"


inputFieldVertPadding : ViewOptions -> Float
inputFieldVertPadding options =
    toFloat options.fontSize * options.inputSpacing


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


poweredFloat : Int -> Float -> Float
poweredFloat power val =
    Round.roundNum -power val


resizeAttrs : (Bool -> msg) -> List (Element.Attribute msg)
resizeAttrs hoveredLabel =
    [ Events.onMouseEnter (hoveredLabel True)
    , Events.onMouseLeave (hoveredLabel False)

    --, Html.Events.onMouseDown (ClickedPointerLockLabel fieldName)
    ]


moveFloat : Int -> { r | val : Float, power : Int } -> Float
moveFloat num field =
    field.val + toFloat (num * (10 ^ field.power))


moveInt : Int -> { r | val : Int, power : Int } -> Int
moveInt num field =
    field.val + (num * (10 ^ field.power))
