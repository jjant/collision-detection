module Main exposing (main)

import Body exposing (Body, Shape(..))
import Browser
import Browser.Events
import Camera exposing (Camera)
import Color
import Config exposing (Config)
import ConfigForm exposing (ConfigForm)
import Element exposing (Element, fill, paddingXY, row, spaceEvenly, width)
import Fps
import Hierarchy
import Html as Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Keys exposing (Keys)
import Mat3
import Misc exposing (listIf)
import Msg exposing (Msg(..))
import Render
import Svg
import Svg.Attributes as Svg
import Vec2 exposing (Vec2, vec2)
import Vec3 exposing (vec3)


type alias Flags =
    Json.Encode.Value


type alias Model =
    { translate : Vec3.Vec3
    , scale : Vec3.Vec3
    , rotation : Vec3.Vec3
    , config : Config
    , configForm : ConfigForm
    , mouse : Vec2
    , camera : Camera
    , keys : Keys
    , fps : Fps.Model
    , bodies : List Body
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
      , camera =
            Camera.new
                { position = Vec2.zero
                , viewportSize = vec2 width height
                }
      , keys = Keys.init
      , fps = Fps.init 20
      , bodies =
            [ { transform =
                    { translation = vec2 config.x config.y
                    , rotation = 0
                    }
              , shape = Rectangle { halfExtents = vec2 100 50 }
              }
            , { transform =
                    { translation = vec2 150 150
                    , rotation = 0
                    }
              , shape = Circle { radius = 50 }
              }
            ]
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
    Sub.batch
        [ Sub.map KeysMsg Keys.subscriptions
        , Browser.Events.onAnimationFrameDelta Tick
        , Sub.map FpsMsg Fps.subscriptions
        ]


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

                KeysMsg keysMsg ->
                    { model | keys = Keys.update keysMsg model.keys }

                Tick dt ->
                    { model | camera = Camera.tick dt model.keys model.camera }

                FpsMsg fpsMsg ->
                    { model | fps = Fps.update fpsMsg model.fps }

                ChangeBody body ->
                    -- { model | circle1 = body }
                    model
    in
    ( newModel, Cmd.none )


view : Model -> Html Msg
view model =
    let
        mousePosition =
            Mat3.transformPoint (Camera.inverseMatrix model.camera) model.mouse

        mouseBody : Body
        mouseBody =
            { transform =
                { translation = mousePosition
                , rotation = 0
                }
            , shape = Circle { radius = 10 }

            -- , shape = Rectangle { halfExtents = vec2 10 10 }
            }
    in
    Element.layout [] <|
        row
            [ spaceEvenly
            , Element.width fill
            , paddingXY 80 80
            ]
            [ Element.html <|
                div
                    -- some nice styles to render it on the right side of the viewport
                    [ Html.Attributes.style "padding" "12px"
                    , Html.Attributes.style "background" "#eec"
                    , Html.Attributes.style "border" "1px solid #444"
                    , Html.Attributes.style "height" "calc(100% - 80px)"
                    , style "margin-left" "32px"
                    , style "display" "flex"
                    , style "flex-direction" "column"
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
                    , Html.text <|
                        Debug.toString <|
                            Maybe.map (\{ average } -> round average) <|
                                Fps.fps
                                    model.fps
                    ]
            , Element.html <|
                Render.render
                    [ Html.Attributes.width (round width)
                    , Html.Attributes.height (round height)
                    , Html.Attributes.style "border" "1px solid blue"
                    , Html.Attributes.style "margin" "0 auto"
                    , Html.Events.on "mousemove" mouseDecoder
                    ]
                    (Render.body [ Svg.fill "none", Svg.stroke "black", Svg.strokeWidth "3" ] mouseBody
                        :: renderBodies model.bodies
                        ++ listIf model.config.showSupportPoints (supportPoints mousePosition model.bodies)
                        ++ listIf model.config.showPointProjections (pointProjections mousePosition model.bodies)
                    )
                    (Camera.matrix model.camera)

            -- , Hierarchy.view ChangeBody circle1
            ]


width : number
width =
    1280


height : Float
height =
    toFloat width / aspect


aspect : Float
aspect =
    16 / 9


supportPoints : Vec2 -> List Body -> List (Render.Renderable msg)
supportPoints mousePosition bodies =
    List.concatMap (supportPoint mousePosition) bodies


supportPoint : Vec2 -> Body -> List (Render.Renderable msg)
supportPoint mousePosition body =
    [ Render.circle []
        { position =
            Body.supportPoint
                (Vec2.direction { from = body.transform.translation, to = mousePosition })
                body
        , radius = 5
        }
    , Render.vector
        []
        { base = body.transform.translation
        , vector =
            Vec2.direction { from = body.transform.translation, to = mousePosition }
                |> Vec2.scale 50
        }
    ]


pointProjections mousePosition bodies =
    List.map (pointProjection mousePosition) bodies


pointProjection : Vec2 -> Body -> Render.Renderable msg
pointProjection mousePosition body =
    let
        projection =
            Body.projectPoint mousePosition body
    in
    Render.circle
        [ Svg.fill <|
            if projection.isInside then
                "red"

            else
                "blue"
        ]
        { position = projection.point, radius = 5 }



-- ++ (contact
--                                 |> Maybe.map
--                                     (\{ world1, world2, normal, depth } ->
--                                         [ Render.circle [ Svg.fill "magenta" ] { position = world1, radius = 5 }
--                                         , Render.circle [ Svg.fill "magenta" ] { position = world2, radius = 5 }
--                                         , Render.vector [] { base = world1, vector = Vec2.scale depth normal }
--                                         ]
--                                     )
--                                 |> Maybe.withDefault []
--                            )


renderBodies : List Body -> List (Render.Renderable msg)
renderBodies =
    List.map
        (Render.body
            [ Svg.fill "none"
            , Svg.strokeWidth "5"
            , Svg.stroke "red"
            ]
        )
