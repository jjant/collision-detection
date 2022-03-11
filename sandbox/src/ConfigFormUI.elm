module ConfigFormUI exposing
    ( ViewOptions
    , formattedPower
    , inputFieldVertPadding
    , makePowerEl
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
import Html
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Misc
import Round


type alias ViewOptions =
    { fontSize : Int
    , rowSpacing : Int
    , inputWidth : Int
    , inputSpacing : Float
    , labelHighlightBgColor : Color
    , sectionSpacing : Int
    }


makePowerEl : (field -> msg) -> ViewOptions -> Int -> field -> field -> Bool -> Element msg
makePowerEl changedConfigForm options power newIncField newDecField isDownDisabled =
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
                        (\_ -> changedConfigForm newIncField)
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
                        (\_ -> changedConfigForm newDecField)
                    , style "cursor" "pointer"
                    ]
                    -- up btn
                    [ Html.text "↗️" ]
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
    , Misc.cursor "ew-resize"
    ]
