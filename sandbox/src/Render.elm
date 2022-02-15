module Render exposing
    ( aabb
    , body
    , circle
    , rectangle
    , render
    , vector
    )

import AABB exposing (AABB)
import Body exposing (Body, Shape(..))
import Isometry exposing (Isometry)
import Mat3 exposing (Mat3)
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Vec2 exposing (Vec2, vec2)


render : List (Svg.Attribute msg) -> List (Mat3 -> Svg msg) -> Mat3 -> Svg msg
render attrs children toScreen =
    Svg.svg attrs
        (List.map (\f -> f toScreen) children)


vector : List (Svg.Attribute msg) -> { base : Vec2, vector : Vec2 } -> Mat3 -> Svg msg
vector attrs args toScreen =
    let
        start =
            Mat3.transformPoint toScreen args.base

        end =
            Mat3.transformPoint toScreen (Vec2.add args.base args.vector)

        dir =
            Vec2.direction { from = end, to = start }
                |> Vec2.scale (Vec2.distance start end / 5)

        endCap1 =
            Vec2.rotate -(pi / 4) dir

        endCap2 =
            Vec2.rotate (pi / 4) dir
    in
    Svg.g
        [ Svg.stroke "red"
        , Svg.strokeWidth "2"
        , Svg.strokeLinecap "round"
        ]
        [ line attrs { from = start, to = end }
        , line attrs { from = end, to = Vec2.add endCap1 end }
        , line attrs { from = end, to = Vec2.add endCap2 end }
        ]


line : List (Svg.Attribute msg) -> { from : Vec2, to : Vec2 } -> Svg msg
line attrs { from, to } =
    -- Takes screen space positions
    Svg.line
        (attrs
            ++ [ Svg.x1 (String.fromFloat from.x)
               , Svg.y1 (String.fromFloat from.y)
               , Svg.x2 (String.fromFloat to.x)
               , Svg.y2 (String.fromFloat to.y)
               ]
        )
        []


body : List (Svg.Attribute msg) -> Body -> Mat3 -> Svg msg
body attrs { transform, shape } =
    case shape of
        Circle { radius } ->
            circle attrs
                { position = transform.translation -- not sure if fine
                , radius = radius
                }

        Rectangle { halfExtents } ->
            rectangle attrs
                { transform = transform
                , halfExtents = halfExtents
                }


aabb : List (Svg.Attribute msg) -> AABB -> Mat3 -> Svg msg
aabb attrs ({ min, max } as aabb_) =
    rectangle attrs
        { transform =
            { translation = AABB.center aabb_
            , rotation = 0
            }
        , halfExtents =
            Vec2.sub max min
                |> Vec2.scale 0.5
        }


rectangle : List (Svg.Attribute msg) -> { transform : Isometry, halfExtents : Vec2 } -> Mat3 -> Svg msg
rectangle attrs { transform, halfExtents } toScreen =
    -- Deal with rotation
    let
        localPoints =
            [ Vec2.scale -1 halfExtents
            , Vec2.scaleY -1 halfExtents
            , halfExtents
            , Vec2.scaleX -1 halfExtents
            ]

        points =
            localPoints
                |> List.map (Isometry.apply transform)
                |> List.map (Mat3.transformPoint toScreen)

        svgPoints =
            points
                |> List.map (\p -> String.fromFloat p.x ++ ", " ++ String.fromFloat p.y)
                |> String.join " "
    in
    Svg.polygon
        (attrs
            ++ [ Svg.points svgPoints
               ]
        )
        []


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
