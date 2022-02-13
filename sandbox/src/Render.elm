module Render exposing
    ( body
    , circle
    , render
    ,  vector
       -- , rectangle

    )

import Body exposing (Body, Shape(..))
import Mat3 exposing (Mat3)
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Vec2 exposing (Vec2, vec2)


render : List (Svg.Attribute msg) -> List (Mat3 -> Svg msg) -> Mat3 -> Svg msg
render attrs children toScreen =
    Svg.svg attrs
        (List.map (\f -> f toScreen) children)


vector attrs args toScreen =
    let
        start =
            Mat3.transformPoint toScreen args.base

        end =
            Mat3.transformPoint toScreen (Vec2.add args.base args.vector)
    in
    Svg.line
        (attrs
            ++ [ Svg.x1 (String.fromFloat start.x)
               , Svg.y1 (String.fromFloat start.y)
               , Svg.x2 (String.fromFloat end.x)
               , Svg.y2 (String.fromFloat end.y)
               , Svg.stroke "red"
               ]
        )
        []


body : List (Svg.Attribute msg) -> Body -> Mat3 -> Svg msg
body attrs { transform, shape } toScreen =
    case shape of
        Circle { radius } ->
            circle attrs
                { position = transform.translation -- not sure if fine
                , radius = radius
                }
                toScreen


circle : List (Svg.Attribute msg) -> { position : Vec2, radius : Float } -> Mat3 -> Svg msg
circle attrs { position, radius } toScreen =
    let
        screenPosition =
            Mat3.transformPoint toScreen position

        screenRadius =
            -- Pretty hacky
            Mat3.transformVector toScreen (vec2 0 radius)
                |> Vec2.length
    in
    Svg.circle
        (attrs
            ++ [ Svg.cx (String.fromFloat screenPosition.x)
               , Svg.cy (String.fromFloat screenPosition.y)
               , Svg.r (String.fromFloat screenRadius)
               ]
        )
        []
