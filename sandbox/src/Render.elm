module Render exposing
    ( Renderable
    , aabb
    , body
    , circle
    , gizmo
    , group
    , line
    , none
    , polygon
    , rectangle
    , render
    , text
    , vector
    )

import AABB exposing (AABB)
import Body exposing (Body, Shape(..))
import Html.Attributes
import Isometry exposing (Isometry)
import Json.Decode as Decode
import Mat3 exposing (Mat3)
import Misc exposing (mouseDecoder)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Vec2 exposing (Vec2, vec2)


type Renderable msg
    = Renderable (Mat3 -> Svg msg)


render : (Vec2 -> msg) -> List (Svg.Attribute msg) -> List (Renderable msg) -> Mat3 -> Svg msg
render onClick attrs children toScreen =
    Svg.svg
        ([ Svg.Events.on "click" (Decode.map onClick mouseDecoder)
         , Html.Attributes.style "background" "white"
         ]
            ++ attrs
        )
        (List.map (\(Renderable f) -> f toScreen) children)


none : Renderable msg
none =
    group [] []


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
        [ Svg.Attributes.stroke "red"
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.strokeLinecap "round"
        ]
        [ screenLine attrs { from = start, to = end }
        , screenLine attrs { from = end, to = Vec2.add endCap1 end }
        , screenLine attrs { from = end, to = Vec2.add endCap2 end }
        ]


line : List (Svg.Attribute msg) -> { from : Vec2, to : Vec2 } -> Renderable msg
line attrs { from, to } =
    Renderable
        (\toScreen ->
            screenLine attrs
                { from = Mat3.transformPoint toScreen from
                , to = Mat3.transformPoint toScreen to
                }
        )


screenLine : List (Svg.Attribute msg) -> { from : Vec2, to : Vec2 } -> Svg msg
screenLine attrs { from, to } =
    -- Takes screen space positions
    Svg.line
        (attrs
            ++ [ Svg.Attributes.x1 (String.fromFloat from.x)
               , Svg.Attributes.y1 (String.fromFloat from.y)
               , Svg.Attributes.x2 (String.fromFloat to.x)
               , Svg.Attributes.y2 (String.fromFloat to.y)
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
        rectangle_ ([ Svg.Attributes.class "gizmo", Svg.Attributes.fill "magenta" ] ++ attrs)
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
            ++ [ Svg.Attributes.points svgPoints
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
            ++ [ Svg.Attributes.cx (String.fromFloat screenPosition.x)
               , Svg.Attributes.cy (String.fromFloat screenPosition.y)
               , Svg.Attributes.r (String.fromFloat screenRadius)
               ]
        )
        []


group : List (Svg.Attribute msg) -> List (Renderable msg) -> Renderable msg
group attrs renderables =
    Renderable
        (\toScreen ->
            Svg.g attrs
                (List.map (\(Renderable f) -> f toScreen) renderables)
        )


text : List (Svg.Attribute msg) -> { position : Vec2, text : String } -> Renderable msg
text attrs args =
    Renderable
        (\toScreen ->
            let
                screenPosition =
                    Mat3.transformPoint toScreen args.position
            in
            Svg.text_
                ([ Svg.Attributes.x (String.fromFloat screenPosition.x)
                 , Svg.Attributes.y (String.fromFloat screenPosition.y)
                 ]
                    ++ attrs
                )
                [ Svg.text args.text ]
        )


polygon : List (Svg.Attribute msg) -> List Vec2 -> Renderable msg
polygon attrs points =
    Renderable
        (\toScreen ->
            let
                screenPoints =
                    points
                        |> List.map (Mat3.transformPoint toScreen)

                svgPoints =
                    screenPoints
                        |> List.map (\{ x, y } -> String.fromFloat x ++ "," ++ String.fromFloat y)
                        |> String.join " "
            in
            Svg.polygon (Svg.Attributes.points svgPoints :: attrs) []
        )
