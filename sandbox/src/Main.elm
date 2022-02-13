module Main exposing (main)

import Body exposing (Body, Shape(..))
import Browser
import Browser.Events
import Circle
import Color
import Config exposing (Config)
import ConfigForm exposing (ConfigForm)
import Html as Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Mat3 exposing (Mat3)
import Mat4
import Msg exposing (Msg(..))
import Render
import Svg
import Svg.Attributes as Svg
import Vec2 exposing (Vec2, vec2)
import Vec3 exposing (vec3)
import Vec4


type alias Flags =
    Json.Encode.Value


type alias Model =
    { translate : Vec3.Vec3
    , scale : Vec3.Vec3
    , rotation : Vec3.Vec3
    , config : Config
    , configForm : ConfigForm
    , mouse : Vec2.Vec2
    }


init : Flags -> ( Model, Cmd Msg )
init elmConfigUiFlags =
    let
        -- Initialize your config and configForm,
        -- passing in defaults for any empty config fields
        ( config, configForm ) =
            ConfigForm.init
                { flags = elmConfigUiFlags
                , logics = Config.logics
                , emptyConfig =
                    Config.empty
                        { int = 1
                        , float = 1
                        , string = "SORRY IM NEW HERE"
                        , bool = True
                        , color = Color.rgba 1 0 1 1 -- hot pink!
                        }
                }
    in
    ( { config = config
      , configForm = configForm
      , translate = vec3 0 0 0
      , scale = vec3 1 1 1
      , rotation = vec3 0 0 0
      , mouse = vec2 0 0
      }
    , Cmd.none
    )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- Browser.Events.onMouseMove mouseDecoder
    Sub.none


mouseDecoder : Decoder Msg
mouseDecoder =
    Decode.map2
        (\x y ->
            MouseMove (vec2 x y)
        )
        (Decode.at [ "offsetX" ] Decode.float)
        (Decode.at [ "offsetY" ] Decode.float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Increment ->
                    model

                Decrement ->
                    model

                ConfigFormMsg configFormMsg ->
                    let
                        ( newConfig, newConfigForm ) =
                            ConfigForm.update
                                Config.logics
                                model.config
                                model.configForm
                                configFormMsg
                    in
                    { model
                        | config = newConfig
                        , configForm = newConfigForm
                    }

                MouseMove mouse ->
                    { model | mouse = mouse }
    in
    ( newModel, Cmd.none )


viewProjection : { width : Float, height : Float } -> Mat3
viewProjection size =
    Mat3.mul (Mat3.viewport size)
        (Mat3.mul (Mat3.orthographic size)
            (Mat3.lookAt
                { centerOfAttention = Vec2.zero
                , upDirection = Vec2.up
                }
            )
        )


view : Model -> Html Msg
view model =
    let
        width =
            1280

        height =
            toFloat width / aspect

        aspect =
            16 / 9

        transform =
            Mat4.mul
                (Mat4.translate
                    (vec3
                        model.config.x
                        model.config.y
                        model.config.z
                    )
                )
                (Mat4.scale
                    (vec3
                        model.config.sx
                        model.config.sy
                        model.config.sz
                    )
                )

        mousePos =
            Vec2.vec2 (model.mouse.x - width / 2)
                (height / 2 - model.mouse.y)

        mouseBody : Body
        mouseBody =
            { transform =
                { translation = mousePos
                , rotation = 0
                }
            , shape = Circle { radius = 40 }
            }

        circle1 : Body
        circle1 =
            { transform =
                { translation = Vec2.vec2 model.config.x model.config.y
                , rotation = 0
                }
            , shape = Circle { radius = 100 }
            }

        result =
            Body.projectPoint mousePos circle1

        contact =
            Body.contact circle1 mouseBody
    in
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "height" "100%"
        ]
        [ div
            -- some nice styles to render it on the right side of the viewport
            [ Html.Attributes.style "padding" "12px"
            , Html.Attributes.style "background" "#eec"
            , Html.Attributes.style "border" "1px solid #444"
            , Html.Attributes.style "height" "calc(100% - 80px)"
            , style "margin-left" "32px"
            ]
            [ ConfigForm.view
                ConfigForm.viewOptions
                Config.logics
                model.configForm
                |> Html.map ConfigFormMsg

            -- As a developer, you'll want to save your tweaks to your config.json.
            -- You can copy/paste the content from this textarea to your config.json.
            -- Then the next time a new user loads your app, they'll see your updated config.
            , Html.textarea []
                [ ConfigForm.encode model.configForm
                    |> Json.Encode.encode 2
                    |> Html.text
                ]
            ]
        , Render.render
            [ Html.Attributes.width (round width)
            , Html.Attributes.height (round height)
            , Html.Attributes.style "border" "1px solid blue"
            , Html.Attributes.style "margin" "0 auto"
            , Html.Events.on "mousemove" mouseDecoder
            ]
            ([ Render.body
                [ Svg.fill "none"
                , Svg.strokeWidth "5"
                , Svg.stroke "red"
                ]
                circle1

             -- , Render.circle [ Svg.fill "blue" ] { position = result.point, radius = 5 }
             , Render.body [ Svg.fill "none", Svg.stroke "black", Svg.strokeWidth "3" ] mouseBody
             ]
                ++ (contact
                        |> Maybe.map
                            (\{ world1, world2, normal } ->
                                [ Render.circle [ Svg.fill "magenta" ] { position = world1, radius = 5 }
                                , Render.circle [ Svg.fill "magenta" ] { position = world2, radius = 5 }
                                , Render.vector [] { base = world1, vector = Vec2.scale 50 normal }
                                ]
                            )
                        |> Maybe.withDefault []
                   )
            )
            (viewProjection { width = width, height = height })

        --     [ Circle.render
        --         { color =
        --             if result.isInside then
        --                 Vec4.vec4 1 0 0 1
        --             else
        --                 Vec4.vec4 0 0 1 1
        --         , projection = Math.Matrix4.makeOrtho2D -(width / 2) (width / 2) -(height / 2) (height / 2)
        --         , thickness = 0.1
        --         }
        --         c1
        --         iso1
        --     , Circle.render
        --         { color = Vec4.vec4 0.25 1 0 1
        --         , projection = Math.Matrix4.makeOrtho2D -(width / 2) (width / 2) -(height / 2) (height / 2)
        --         , thickness = 1
        --         }
        --         { radius = 10 }
        --         { translation = result.point
        --         , rotation = 0
        --         }
        --     , Circle.render
        --         { color = Vec4.vec4 1 0.2 1 1
        --         , projection = Math.Matrix4.makeOrtho2D -(width / 2) (width / 2) -(height / 2) (height / 2)
        --         , thickness = 1
        --         }
        --         { radius = 5 }
        --         { translation = mousePos
        --         , rotation = 0
        --         }
        --     ]
        ]



-- circle :
--     { center : Vec2.Vec2
--     , radius : Float
--     , color : Vec4.Vec4
--     , projection : Math.Matrix4.Mat4
--     , thickness : Float
--     }
--     -> WebGL.Entity
-- circle { center, radius, color, projection, thickness } =
--     let
--         transform =
--             Math.Matrix4.makeTranslate3 (Vec2.getX center) (Vec2.getY center) 0
--                 |> Math.Matrix4.scale3 radius radius radius
--     in
--     Main.Render.circle
--         { model = transform
--         , projection = projection
--         , thickness = 1.0 --thickness
--         , fade = 0.0
--         , color = color
--         }
