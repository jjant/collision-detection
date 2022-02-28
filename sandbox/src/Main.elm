module Main exposing (main)

import Array exposing (Array)
import Body exposing (Body, Shape(..))
import Browser
import Browser.Events
import Camera exposing (Camera)
import Color
import Config exposing (Config)
import ConfigForm exposing (ConfigForm)
import Draggable
import Element exposing (column, fill, paddingXY, px, rgb255, row, spaceEvenly, width)
import Element.Background as Background
import Fps
import Hierarchy
import Html as Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode
import Keys exposing (Keys)
import Mat3
import Misc exposing (listIf, mouseDecoder)
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
    , viewportSize : Vec2
    , mouse : Vec2
    , camera : Camera
    , keys : Keys
    , fps : Fps.Model
    , bodies : Array Body
    , selectedBody : Maybe Int
    , drag : Draggable.State ()
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
                        , color = Color.rgb255 32 37 49 -- hot pink!
                        }
                }

        width : number
        width =
            1280

        height : Float
        height =
            toFloat width / aspect

        aspect : Float
        aspect =
            16 / 9
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
      , viewportSize = vec2 width height
      , keys = Keys.init
      , fps = Fps.init 20
      , bodies =
            Array.fromList
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
      , selectedBody = Just 0
      , drag = Draggable.init
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
subscriptions model =
    Sub.batch
        [ Sub.map KeysMsg Keys.subscriptions
        , Browser.Events.onAnimationFrameDelta Tick
        , Sub.map FpsMsg Fps.subscriptions
        , Draggable.subscriptions DragMsg model.drag
        ]


dragConfig : Draggable.Config () Msg
dragConfig =
    Draggable.basicConfig (\( x, y ) -> OnDragBy (vec2 x y))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
            ( { model
                | config = newConfig
                , configForm = newConfigForm
              }
            , Cmd.none
            )

        MouseMove mouse ->
            ( { model | mouse = mouse }, Cmd.none )

        MouseClick mouse ->
            let
                worldSpaceMouse =
                    Mat3.transformPoint (Camera.inverseMatrix model.camera) mouse

                newSelectedBody =
                    -- TODO: Make this not horribly slow
                    Array.indexedMap
                        (\idx body ->
                            if Debug.log "" (Body.projectPoint worldSpaceMouse body).isInside then
                                idx

                            else
                                -1
                        )
                        model.bodies
                        |> Array.filter (\a -> a >= 0)
                        |> Array.get 0
            in
            ( { model | selectedBody = newSelectedBody }, Cmd.none )

        KeysMsg keysMsg ->
            ( { model | keys = Keys.update keysMsg model.keys }, Cmd.none )

        Tick dt ->
            ( { model | camera = Camera.tick dt model.keys model.camera }, Cmd.none )

        FpsMsg fpsMsg ->
            ( { model | fps = Fps.update fpsMsg model.fps }, Cmd.none )

        ChangeBody body ->
            ( { model
                | bodies =
                    model.selectedBody
                        |> Maybe.map (\idx -> Array.set idx body model.bodies)
                        |> Maybe.withDefault model.bodies
              }
            , Cmd.none
            )

        SelectBody selectedBody_ ->
            ( { model | selectedBody = Just selectedBody_ }, Cmd.none )

        OnDragBy screenSpaceDelta ->
            let
                worldSpaceDelta =
                    Mat3.transformVector (Camera.inverseMatrix model.camera) screenSpaceDelta
                        |> Debug.log "delta"

                newModel =
                    updateSelectedBody
                        (Misc.updateTranslation (Vec2.add worldSpaceDelta))
                        model
            in
            ( newModel, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


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
            }
    in
    Element.layout
        [ Background.color <| Misc.toElementColor model.config.backgroundColor
        ]
    <|
        row
            [ spaceEvenly
            , Element.width fill
            , paddingXY 20 80
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
                    MouseClick
                    [ Html.Attributes.width (round model.viewportSize.x)
                    , Html.Attributes.height (round model.viewportSize.y)
                    , Html.Attributes.style "border" "1px solid blue"
                    , Html.Attributes.style "margin" "0 auto"
                    , Html.Events.on "mousemove" (Decode.map MouseMove mouseDecoder)
                    ]
                    (Render.body [ Svg.fill "none", Svg.stroke "black", Svg.strokeWidth "3" ] mouseBody
                        :: renderBodies model.bodies
                        ++ listIf model.config.showSupportPoints (supportPoints mousePosition model.bodies)
                        ++ listIf model.config.showPointProjections (pointProjections mousePosition model.bodies)
                        ++ (selectedBody model
                                |> Maybe.map (\{ transform } -> [ Render.gizmo [ Draggable.mouseTrigger () DragMsg ] transform.translation ])
                                |> Maybe.withDefault []
                           )
                    )
                    (Camera.matrix model.camera)
            , column
                [ Background.color (rgb255 238 238 204)
                , width (px 400)
                ]
                [ Hierarchy.list SelectBody model.selectedBody model.bodies
                , Hierarchy.view ChangeBody (selectedBody model)
                ]
            ]


updateSelectedBody : (Body -> Body) -> Model -> Model
updateSelectedBody fn ({ bodies } as model) =
    let
        newBodies =
            selectedBody model
                |> Maybe.map fn
                |> Maybe.andThen
                    (\b ->
                        model.selectedBody
                            |> Maybe.map (Tuple.pair b)
                    )
                |> Maybe.map
                    (\( body, id ) ->
                        Array.set id body bodies
                    )
                |> Maybe.withDefault bodies
    in
    { model | bodies = newBodies }


selectedBody : Model -> Maybe Body
selectedBody model =
    model.selectedBody
        |> Maybe.andThen (\idx -> Array.get idx model.bodies)


supportPoints : Vec2 -> Array Body -> List (Render.Renderable msg)
supportPoints mousePosition bodies =
    bodies
        |> mapToList (supportPoint mousePosition)
        |> List.concat


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


pointProjections : Vec2 -> Array Body -> List (Render.Renderable msg)
pointProjections mousePosition bodies =
    mapToList (pointProjection mousePosition) bodies


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


renderBodies : Array Body -> List (Render.Renderable msg)
renderBodies =
    mapToList
        (Render.body
            [ Svg.fill "none"
            , Svg.strokeWidth "5"
            , Svg.stroke "red"
            ]
        )


mapToList : (a -> b) -> Array a -> List b
mapToList f arr =
    Array.foldr (\a acc -> f a :: acc) [] arr
