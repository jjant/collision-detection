module Hierarchy exposing (list, view)

import Array exposing (Array)
import Body exposing (Body, Shape)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (OptionState(..))
import Html.Attributes
import Misc exposing (setTranslation)
import Unwrap
import Vec2 exposing (Vec2, vec2)


type ShapeKind
    = Circle
    | Rectangle


setShape : ShapeKind -> Body -> Body
setShape shapeKind body =
    case ( shapeKind, body.shape ) of
        ( Circle, Body.Circle _ ) ->
            body

        ( Rectangle, Body.Rectangle _ ) ->
            body

        _ ->
            { body | shape = defaultShape shapeKind }


list : (Int -> msg) -> Maybe Int -> Array Body -> Element msg
list selectBody selectedBody bodies =
    el
        [ width fill
        , Background.color (rgb255 51 60 78)
        , Border.color (rgb255 26 30 41)
        , Border.width 2
        , padding 5
        ]
        (Input.radio
            [ Background.color (rgb255 38 44 59)
            , Border.color (rgb255 26 30 41)
            , Border.width 2
            , width fill
            , Font.color (rgb255 192 195 201)
            , Font.size 18
            , spacing 10
            , paddingXY 5 10
            ]
            { onChange = selectBody
            , options =
                bodies
                    |> Array.indexedMap (\idx _ -> bodyInput idx)
                    |> Array.toList
            , selected = selectedBody
            , label = Input.labelAbove [] (text "Bodies")
            }
        )


bodyInput : Int -> Input.Option Int msg
bodyInput id =
    let
        styles opState =
            case opState of
                Idle ->
                    []

                Focused ->
                    []

                Selected ->
                    [ Border.color (rgb255 230 230 230)
                    , Border.width 1
                    , Background.color (rgba255 200 200 200 0.15)
                    ]
    in
    Input.optionWith id
        (\optionState ->
            el
                ([ Element.htmlAttribute (Html.Attributes.style "user-select" "none")
                 , width fill
                 , Font.alignLeft
                 , paddingXY 2 4
                 ]
                    ++ styles optionState
                )
                (text <| "Body " ++ String.fromInt id)
        )


view : (Body -> msg) -> Maybe Body -> Element msg
view onChange maybeBody =
    column
        [ width fill
        , height (px 300)
        ]
        (maybeBody
            |> Maybe.map
                (\body ->
                    [ text <| Vec2.toString body.transform.translation
                    , vec2Input (\translation -> onChange <| setTranslation translation body) body.transform.translation
                    , shapeRadio (\newShapeKind -> onChange (setShape newShapeKind body)) body.shape
                    , shapeInput body.shape
                    ]
                )
            |> Maybe.withDefault []
        )


defaultShape : ShapeKind -> Shape
defaultShape shape =
    case shape of
        Circle ->
            Body.Circle { radius = 80 }

        Rectangle ->
            Body.Rectangle { halfExtents = vec2 100 75 }


selectedShape : Shape -> ShapeKind
selectedShape shape =
    case shape of
        Body.Circle _ ->
            Circle

        Body.Rectangle _ ->
            Rectangle


shapeInput : Shape -> Element msg
shapeInput shape =
    case shape of
        Body.Circle { radius } ->
            text <| Debug.toString radius

        Body.Rectangle { halfExtents } ->
            text <| Debug.toString halfExtents


shapeRadio : (ShapeKind -> msg) -> Shape -> Element msg
shapeRadio onShapeChange shape =
    Input.radio []
        { onChange = onShapeChange
        , options = options
        , selected = Just <| selectedShape shape
        , label = Input.labelAbove [] (text "Shape")
        }


options : List (Input.Option ShapeKind msg)
options =
    [ Input.option Circle (text "Circle")
    , Input.option Rectangle (text "Rectangle")
    ]


vec2Input : (Vec2 -> msg) -> Vec2 -> Element msg
vec2Input onChange vec =
    Input.text []
        { text = String.fromFloat vec.x
        , onChange =
            \s ->
                String.toFloat s
                    |> Unwrap.maybe
                    |> (\x -> Vec2.setX x vec)
                    |> onChange
        , placeholder = Nothing
        , label = Input.labelBelow [] (text "x")
        }
