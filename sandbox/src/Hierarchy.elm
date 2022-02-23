module Hierarchy exposing (view)

import Body exposing (Body, Shape)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Vec2 exposing (vec2)


type ShapeKind
    = Circle
    | Rectangle


setShape : Shape -> Body -> Body
setShape shape body =
    { body | shape = shape }


view : (Body -> msg) -> Body -> Element msg
view onChange ({ transform, shape } as body) =
    column
        [ Background.color (rgb255 238 238 204)
        , height fill
        , width (px 300)
        ]
        [ text <| Debug.toString transform
        , shapeRadio (\newShape -> onChange (setShape newShape body)) shape
        , shapeInput shape
        ]


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


shapeRadio : (Shape -> msg) -> Shape -> Element msg
shapeRadio onShapeChange shape =
    Input.radio []
        { onChange = onShapeChange << defaultShape
        , options = options
        , selected = Just <| selectedShape shape
        , label = Input.labelAbove [] (text "Shape")
        }


options : List (Input.Option ShapeKind msg)
options =
    [ Input.option Circle (text "Circle")
    , Input.option Rectangle (text "Rectangle")
    ]
