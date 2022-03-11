module Hierarchy exposing (list, view)

import Array exposing (Array)
import Body exposing (Body, Shape)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (OptionState(..))
import Html
import Html.Attributes
import Misc exposing (setTranslation, updateTransform)
import UI
import Vec2 exposing (Vec2, vec2)


type ShapeKind
    = Circle
    | Rectangle


setShapeKind : ShapeKind -> Body -> Body
setShapeKind shapeKind body =
    case ( shapeKind, body.shape ) of
        ( Circle, Body.Circle _ ) ->
            body

        ( Rectangle, Body.Rectangle _ ) ->
            body

        _ ->
            { body | shape = defaultShape shapeKind }


setShape : Shape -> Body -> Body
setShape shape body =
    { body | shape = shape }


list : (Int -> msg) -> Maybe Int -> Array Body -> Element msg
list selectBody selectedBody bodies =
    el
        [ width
            (fill
                |> Element.minimum 350
            )
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
            , height (Element.minimum 300 fill)
            , scrollbarY
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
            , label =
                Input.labelAbove
                    [ Font.alignLeft
                    , Font.color (rgb255 177 180 187)
                    , paddingXY 0 5
                    ]
                    (text "Bodies")
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
        , Background.color (rgb255 51 60 78)
        , Border.color (rgb255 26 30 41)
        , Border.width 2
        , padding 5
        , Font.color (rgb255 192 195 201)
        ]
        (maybeBody
            |> Maybe.map
                (\body ->
                    [ vec2Input "Translation" (\translation -> onChange <| setTranslation translation body) body.transform.translation
                    , floatInput "Rotation" (\deltaRotation -> onChange <| updateTransform (\t -> { t | rotation = t.rotation + deltaRotation }) body) body.transform.rotation
                    , shapeRadio (\newShapeKind -> onChange (setShapeKind newShapeKind body)) body.shape
                    , shapeInput (\newShape -> onChange (setShape newShape body)) body.shape
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


shapeInput : (Shape -> msg) -> Shape -> Element msg
shapeInput onChange shape =
    case shape of
        Body.Circle { radius } ->
            floatInput "Radius" (\deltaRadius -> onChange (Body.Circle { radius = radius + deltaRadius })) radius

        Body.Rectangle { halfExtents } ->
            vec2Input "Half Extents" (\newExtents -> onChange (Body.Rectangle { halfExtents = newExtents })) halfExtents


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


vec2Input : String -> (Vec2 -> msg) -> Vec2 -> Element msg
vec2Input label onChange vec =
    Element.row [ width fill ]
        [ text label
        , Element.column [ width fill, Background.color (rgb255 255 0 255) ]
            [ floatInput "x" (\deltaX -> Vec2.add vec (vec2 deltaX 0) |> onChange) vec.x
            , floatInput "y" (\deltaY -> Vec2.add vec (vec2 0 deltaY) |> onChange) vec.y
            ]
        ]


floatInput : String -> (Float -> msg) -> Float -> Element msg
floatInput label onChange float =
    Element.row [ width fill ]
        [ text label
        , Element.html <|
            UI.slider
                (onChange << toFloat)
                [ Html.text <| String.fromFloat float ]
        ]
