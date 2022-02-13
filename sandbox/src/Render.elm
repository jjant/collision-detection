module Render exposing
    ( body
    ,  circle
       -- , rectangle

    , render
    )

import Body exposing (Body)
import Mat3 exposing (Mat3)
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Vec2 exposing (Vec2, vec2)
import Vec4


render : List (Svg.Attribute msg) -> List (Mat3 -> Svg msg) -> Mat3 -> Svg msg
render attrs children toScreen =
    Svg.svg attrs
        (List.map (\f -> f toScreen) children)


body : List (Svg.Attribute msg) -> Body -> Mat3 -> Svg msg
body attrs { transform, shape } toScreen =
    case shape of
        { radius } ->
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



-- circle : CircleUniforms -> Entity
-- circle uniforms =
--     WebGL.entityWith
--         [ WebGL.Settings.Blend.add
--             WebGL.Settings.Blend.srcAlpha
--             WebGL.Settings.Blend.oneMinusSrcAlpha
--         ]
--         vertexShader
--         circleFragmentShader
--         rectangleMesh
-- uniforms
-- circleFragmentShader : Shader {} CircleUniforms Varyings
-- circleFragmentShader =
--     [glsl|
-- precision highp float;
-- varying vec2 vPos;
-- uniform mat4 model;
-- uniform vec4 color;
-- uniform float thickness;
-- uniform float fade;
-- void main() {
--     vec2 pos = vPos * 2.0;
--     float dist = length(pos);
--     float circleAlpha = smoothstep(0.0, fade, 1.0 - dist);
--     circleAlpha *= smoothstep(fade +  thickness,thickness , 1.0 - dist);
--     gl_FragColor = vec4(color.rgb, color.a * circleAlpha);
-- }
-- |]
-- rectangleMesh : WebGL.Mesh { pos : Vec2.Vec2 }
-- rectangleMesh =
--     WebGL.indexedTriangles
--         [ { pos = vec2 -0.5 -0.5 }
--         , { pos = vec2 -0.5 0.5 }
--         , { pos = vec2 0.5 0.5 }
--         , { pos = vec2 0.5 -0.5 }
--         ]
--         [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
-- type alias Varyings =
--     { vPos : Vec2.Vec2
--     }
-- rectangle : Uniforms -> WebGL.Entity
-- rectangle uniforms =
--     WebGL.entity vertexShader fragmentShader rectangleMesh uniforms
-- vertexShader : Shader { pos : Vec2.Vec2 } { uniforms | model : Math.Matrix4.Mat4, projection : Math.Matrix4.Mat4 } Varyings
-- vertexShader =
--     [glsl|
-- precision highp float;
-- attribute vec2 pos;
-- uniform mat4 model;
-- uniform mat4 projection;
-- varying vec2 vPos;
-- void main() {
--     vPos = pos;
--     gl_Position = projection * model * vec4(pos.x, pos.y, 0.0, 1.0);
-- }
-- |]
-- -- fragmentShader : Shader {} {} {}
-- -- fragmentShader =
-- --     [glsl|
-- -- void main() {
-- --     gl_FragColor = vec4(1.0, 0.0 ,0.0,1.0);
-- -- }
-- -- |]
