module Hierarchy exposing (list, view)

import Array exposing (Array)
import Body exposing (Body, Shape)
import ConfigForm.Custom
import ConfigForm.Options
import ConfigForm.View
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
                 , Border.width 1

                 -- Add transparent border so that when items are selected they don't get shifted
                 , Border.color (rgba 0 0 0 0)
                 ]
                    ++ styles optionState
                )
                (text <| "Body " ++ String.fromInt id)
        )


view : msg -> (Body -> msg) -> Maybe Body -> Element msg
view noop onChange maybeBody =
    column
        [ width fill
        , Background.color (rgb255 51 60 78)
        , Border.color (rgb255 26 30 41)
        , Border.width 2
        , padding 5
        , Font.color (rgb255 192 195 201)
        ]
        (maybeBody
            |> Maybe.map
                (\body ->
                    [ vec2Input noop "Translation" (\translation -> onChange <| setTranslation translation body) body.transform.translation
                    , floatInput noop -2 "Rotation" (\newRotation -> onChange <| updateTransform (\t -> { t | rotation = newRotation }) body) body.transform.rotation
                    , shapeRadio (\newShapeKind -> onChange (setShapeKind newShapeKind body)) body.shape
                    , shapeInput noop (\newShape -> onChange (setShape newShape body)) body.shape
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


shapeInput : msg -> (Shape -> msg) -> Shape -> Element msg
shapeInput noop onChange shape =
    case shape of
        Body.Circle { radius } ->
            floatInput noop 0 "Radius" (\newRadius -> onChange (Body.Circle { radius = newRadius })) radius

        Body.Rectangle { halfExtents } ->
            vec2Input noop "Half Extents" (\newExtents -> onChange (Body.Rectangle { halfExtents = newExtents })) halfExtents


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


vec2Input : msg -> String -> (Vec2 -> msg) -> Vec2 -> Element msg
vec2Input noop label onChange vec =
    ConfigForm.Custom.viewVec2Field
        { hoveredLabel = \_ -> noop
        , changedConfigForm = \{ val } -> onChange val
        , label = label
        , fieldName = ""
        , options = ConfigForm.Options.viewOptions
        , field = { power = 1, val = vec }
        , index = 0
        , isActive = False
        }


floatInput : msg -> Int -> String -> (Float -> msg) -> Float -> Element msg
floatInput noop power label onChange float =
    ConfigForm.View.viewFloatField
        { hoveredLabel = \_ -> noop
        , changedConfigForm = \{ val } -> onChange val
        , options = ConfigForm.Options.viewOptions
        , label = label
        , floatField = { power = power, val = float }
        , isActive = False
        }
