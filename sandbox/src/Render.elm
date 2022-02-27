module Render exposing
    ( Renderable
    , aabb
    , body
    , circle
    , gizmo
    , rectangle
    , render
    , vector
    )

import AABB exposing (AABB)
import Body exposing (Body, Shape(..))
import Isometry exposing (Isometry)
import Json.Decode as Decode
import Mat3 exposing (Mat3)
import Misc exposing (mouseDecoder)
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Svg.Events
import Vec2 exposing (Vec2, vec2)


type Renderable msg
    = Renderable (Mat3 -> Svg msg)


render : (Vec2 -> msg) -> List (Svg.Attribute msg) -> List (Renderable msg) -> Mat3 -> Svg msg
render onClick attrs children toScreen =
    Svg.svg (Svg.Events.on "click" (Decode.map onClick mouseDecoder) :: attrs)
        (List.map (\(Renderable f) -> f toScreen) children)


vector : List (Svg.Attribute msg) -> { base : Vec2, vector : Vec2 } -> Renderable msg
vector attrs args =
    Renderable (vector_ attrs args)


vector_ : List (Svg.Attribute msg) -> { base : Vec2, vector : Vec2 } -> Mat3 -> Svg msg
vector_ attrs args toScreen =
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


body : List (Svg.Attribute msg) -> Body -> Renderable msg
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


aabb : List (Svg.Attribute msg) -> AABB -> Renderable msg
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


rectangle : List (Svg.Attribute msg) -> { transform : Isometry, halfExtents : Vec2 } -> Renderable msg
rectangle attrs rect =
    Renderable (rectangle_ attrs rect)


gizmo : List (Svg.Attribute msg) -> Vec2 -> Renderable msg
gizmo attrs pos =
    Renderable <|
        rectangle_ ([ Svg.class "gizmo", Svg.fill "magenta" ] ++ attrs)
            { transform = { translation = pos, rotation = 0 }
            , halfExtents = vec2 15 15
            }


rectangle_ : List (Svg.Attribute msg) -> { transform : Isometry, halfExtents : Vec2 } -> Mat3 -> Svg msg
rectangle_ attrs { transform, halfExtents } toScreen =
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


circle : List (Svg.Attribute msg) -> { position : Vec2, radius : Float } -> Renderable msg
circle attrs circ =
    Renderable (circle_ attrs circ)


circle_ : List (Svg.Attribute msg) -> { position : Vec2, radius : Float } -> Mat3 -> Svg msg
circle_ attrs { position, radius } toScreen =
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
