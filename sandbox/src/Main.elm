module Main exposing (main)

import Array exposing (Array)
import Body exposing (Body, Polytope(..), Shape(..))
import Browser
import Browser.Events
import CSOPoint exposing (CSOPoint)
import Camera exposing (Camera)
import Color
import Config exposing (Config)
import ConfigForm
import ConfigFormGeneric exposing (ConfigForm)
import ConvexHull
import Draggable
import Element
    exposing
        ( alignTop
        , column
        , el
        , fill
        , height
        , padding
        , paddingXY
        , rgb255
        , row
        , spacing
        , spacingXY
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Fps
import Hierarchy
import Html as Html exposing (Html)
import Html.Attributes
import Html.Events exposing (..)
import Isometry exposing (Isometry)
import Json.Decode as Decode
import Json.Encode
import Keys exposing (Keys)
import List.Extra
import Mat3
import Misc exposing (listIf, mouseDecoder)
import Msg exposing (Msg(..))
import Rectangle exposing (Rectangle)
import Render exposing (Renderable)
import Svg
import Svg.Attributes as Svg
import Unwrap
import Vec2 exposing (Vec2, vec2)
import Vec3 exposing (vec3)
import VoronoiSimplex exposing (IncompleteSimplex(..), Simplex(..))


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
            ConfigFormGeneric.init
                { flags = elmConfigUiFlags
                , logics = Config.logics
                , emptyConfig =
                    Config.empty
                        { int = 1
                        , float = 1
                        , string = "SORRY IM NEW HERE"
                        , bool = True
                        , color = Color.rgb255 32 37 49 -- hot pink!
                        , vec2 = vec2 -3 -1234
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
            -- gridWorld
            world
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
                    ConfigFormGeneric.update
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
                            if (Body.projectPoint worldSpaceMouse body).isInside then
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
            ( { model
                | camera = Camera.tick dt model.keys model.camera
              }
            , Cmd.none
            )

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

        b1 =
            Array.get 0 model.bodies
                |> Unwrap.maybe

        b2 =
            Array.get 1 model.bodies
                |> Unwrap.maybe

        getRect b =
            Unwrap.maybe <|
                case b.shape of
                    Body.Rectangle rect ->
                        Just rect

                    _ ->
                        Nothing

        getCircle b =
            Unwrap.maybe <|
                case b.shape of
                    Circle circ ->
                        Just circ

                    _ ->
                        Nothing

        pos12 =
            Isometry.compose
                (Isometry.invert b1.transform)
                b2.transform

        res =
            Body.gjkIntersection pos12
                (Body.localSupportPoint b1.shape)
                (Body.localSupportPoint b2.shape)

        polytope : Maybe (Polytope CSOPoint)
        polytope =
            (case res.simplex of
                Three { a, b, c } ->
                    Just ( a, b, c )

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (\( a, b, c ) ->
                        Body.epa (Polytope a b c Array.empty)
                            pos12
                            (Body.localSupportPoint b1.shape)
                            (Body.localSupportPoint b2.shape)
                    )

        mouseBody : Body
        mouseBody =
            { transform =
                { translation = mousePosition
                , rotation = 0
                }
            , shape = Circle { radius = 3 }
            }
    in
    Element.layout
        [ Background.color <| Misc.toElementColor model.config.backgroundColor
        ]
    <|
        row
            [ Element.width fill
            , paddingXY 10 10
            , spacing 10
            ]
            [ column
                [ height fill
                , spacingXY 0 10
                ]
                [ Hierarchy.list SelectBody model.selectedBody model.bodies
                , column
                    [ Background.color (rgb255 51 60 78)
                    , width fill
                    , Border.color (rgb255 26 30 41)
                    , Border.width 2
                    , padding 5
                    , Font.color (rgb255 192 195 201)
                    ]
                    [ ConfigFormGeneric.view
                        ConfigForm.viewOptions
                        Config.logics
                        model.configForm
                        |> Element.map ConfigFormMsg

                    -- As a developer, you'll want to save your tweaks to your config.json.
                    -- You can copy/paste the content from this textarea to your config.json.
                    -- Then the next time a new user loads your app, they'll see your updated config.
                    , Element.html <|
                        Html.textarea []
                            [ ConfigFormGeneric.encode model.configForm
                                |> Json.Encode.encode 2
                                |> Html.text
                            ]
                    , el []
                        (Fps.fps model.fps
                            |> Maybe.map (\{ average } -> round average)
                            |> Maybe.map (\avg -> String.fromInt avg)
                            |> Maybe.withDefault "Loading"
                            |> Element.text
                        )
                    ]
                ]
            , el [ alignTop ]
                (Element.html <|
                    Render.render
                        MouseClick
                        [ Html.Attributes.width (round model.viewportSize.x)
                        , Html.Attributes.height (round model.viewportSize.y)
                        , Html.Attributes.style "border" "1px solid blue"
                        , Html.Attributes.style "background" (Color.toCssString model.config.sceneBackground)
                        , Html.Events.on "mousemove" (Decode.map MouseMove mouseDecoder)
                        ]
                        (Render.body [ Svg.fill "none", Svg.stroke "black", Svg.strokeWidth "3" ] mouseBody
                            :: axis
                            :: renderBodies
                                [ Svg.stroke
                                    (case res.colliding of
                                        True ->
                                            Color.toCssString model.config.collidingBodiesOutline

                                        False ->
                                            "black"
                                    )
                                ]
                                model.bodies
                            :: listIf model.config.showSupportPoints (supportPoints mousePosition model.bodies)
                            ++ listIf model.config.showGjkSimplex [ renderSimplex b1.transform res.simplex ]
                            ++ listIf model.config.showMinkowskiDifference
                                (minkowskiDifference pos12 (getRect b1) (getRect b2)
                                    -- |> List.map (Isometry.apply b1.transform)
                                    |> ConvexHull.convexHull
                                    |> (\points ->
                                            [ Render.polygon
                                                [ Svg.stroke "lightgreen"
                                                , Svg.fill "none"
                                                , Svg.strokeWidth "2"
                                                ]
                                                points
                                            , Render.group [ Svg.fill "lightgreen" ]
                                                (List.map (Render.point []) points)
                                            ]
                                       )
                                )
                            ++ listIf model.config.showEpaPolytope
                                (case polytope of
                                    Just (Polytope a b c rest) ->
                                        [ Render.polygon
                                            [ Svg.stroke "red"
                                            , Svg.fill "none"
                                            , Svg.strokeWidth "3"
                                            ]
                                            (List.map .point (a :: b :: c :: Array.toList rest))
                                        ]

                                    _ ->
                                        []
                                )
                            ++ listIf model.config.showPointProjections (pointProjections mousePosition model.bodies)
                            ++ listIf model.config.showContactPoints (contactPoints model.bodies)
                            ++ (selectedBody model
                                    |> Maybe.map (\{ transform } -> [ Render.gizmo [ Draggable.mouseTrigger () DragMsg ] transform.translation ])
                                    |> Maybe.withDefault []
                               )
                        )
                        (Camera.matrix model.camera)
                )
            , column
                [ Background.color (rgb255 238 238 204)
                , width fill
                ]
                [ Hierarchy.view ChangeBody (selectedBody model)
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


contactPoints : Array Body -> List (Render.Renderable msg)
contactPoints bodies =
    -- TODO: Handle more than 2 bodies
    Maybe.map2 contactPoint
        (Array.get 0 bodies)
        (Array.get 1 bodies)
        |> Maybe.withDefault []


contactPoint : Body -> Body -> List (Render.Renderable msg)
contactPoint body1 body2 =
    Body.contact body1 body2
        |> Maybe.map
            (\{ point1, point2, normal1, normal2, dist } ->
                let
                    world1 =
                        Isometry.apply body1.transform point1

                    world2 =
                        Isometry.apply body2.transform point2

                    depth =
                        -dist
                in
                [ Render.circle [ Svg.fill "magenta" ] { position = world1, radius = 5 }
                , Render.circle [ Svg.fill "magenta" ] { position = world2, radius = 5 }
                , Render.vector [] { base = world1, vector = Vec2.scale depth normal1 }
                ]
            )
        |> Maybe.withDefault []


renderBodies : List (Svg.Attribute msg) -> Array Body -> Render.Renderable msg
renderBodies attrs bodies =
    Render.group
        ([ Svg.fill "none"
         , Svg.strokeWidth "5"
         , Svg.stroke "red"
         ]
            ++ attrs
        )
        (mapToList (Render.body []) bodies)


mapToList : (a -> b) -> Array a -> List b
mapToList f arr =
    Array.foldr (\a acc -> f a :: acc) [] arr


world : Array Body
world =
    Array.fromList
        [ { transform =
                { translation = vec2 0 0
                , rotation = 0
                }
          , shape = Body.Rectangle { halfExtents = vec2 100 50 }

          --   , shape = Circle { radius = 50 }
          }
        , { transform =
                { translation = vec2 0 75
                , rotation = 0
                }
          , shape = Body.Rectangle { halfExtents = vec2 40 30 }

          --   , shape = Circle { radius = 50 }
          }
        ]


gridWorld : Array Body
gridWorld =
    Array.initialize (5 * 5) <|
        \i ->
            { transform =
                { translation = vec2 (toFloat (i // 5) * 75) (toFloat (modBy 5 i) * 75)
                , rotation = 0
                }
            , shape = Body.Rectangle { halfExtents = vec2 25 25 }
            }


axis : Renderable msg
axis =
    -- TODO: Implement properly
    Render.group
        [ Svg.strokeWidth "2", Svg.stroke "gray" ]
        [ Render.line []
            { from = vec2 -1000 0, to = vec2 1000 0 }
        , Render.line []
            { from = vec2 0 -1000, to = vec2 0 1000 }
        , Render.group []
            (List.range -20 20
                |> List.map
                    (\index ->
                        let
                            i =
                                toFloat index

                            tickDistance =
                                50
                        in
                        if i == 0 then
                            Render.group [] []

                        else
                            Render.group [ Svg.stroke "black", Html.Attributes.style "user-select" "none" ]
                                [ Render.line []
                                    { from = vec2 (i * tickDistance) -10
                                    , to = vec2 (i * tickDistance) 10
                                    }
                                , Render.line []
                                    { from = vec2 -10 (i * tickDistance)
                                    , to = vec2 10 (i * tickDistance)
                                    }
                                , Render.text
                                    [ Svg.strokeWidth "1"
                                    , Svg.fontSize "15"
                                    , Svg.fontWeight "100"
                                    ]
                                    { position = vec2 (i * tickDistance + 2) -20
                                    , text = String.fromFloat <| i * tickDistance
                                    }
                                , Render.text
                                    [ Svg.strokeWidth "1"
                                    , Svg.fontSize "15"
                                    , Svg.fontWeight "100"
                                    ]
                                    { position = vec2 -35 (i * tickDistance + 5)
                                    , text = String.fromFloat <| i * tickDistance
                                    }
                                ]
                    )
            )
        ]


renderSimplex : Isometry -> Simplex -> Renderable msg
renderSimplex transform simplex =
    let
        points =
            case simplex of
                Two { a, b } ->
                    [ a, b ]

                Three { a, b, c } ->
                    [ a, b, c ]

        numPoints =
            List.length points

        color i =
            Color.hsl (toFloat i / toFloat numPoints) 1 0.5
    in
    Render.group []
        [ Render.polygon
            [ Svg.stroke "black"
            , Svg.strokeWidth "9"
            , Svg.fill "none"
            ]
            (List.map .point points)
        , Render.group [] <|
            List.indexedMap
                (\index p ->
                    Render.group [ Svg.fill (Color.toCssString (color index)) ]
                        [ Render.point [] p.point
                        , Render.point [] (Isometry.apply transform p.orig1)
                        , Render.point [] (Isometry.apply transform p.orig2)
                        ]
                )
                points
        ]


minkowskiDifference : Isometry -> Rectangle -> Rectangle -> List Vec2
minkowskiDifference pos12 rect1 rect2 =
    let
        points1 =
            rectPoints Isometry.identity rect1

        points2 =
            rectPoints pos12 rect2
    in
    List.Extra.lift2 (\p1 p2 -> Vec2.sub p1 p2) points1 points2


rectPoints : Isometry -> Rectangle -> List Vec2
rectPoints iso { halfExtents } =
    -- Counter-clockwise order
    let
        localPoints =
            [ vec2 -halfExtents.x -halfExtents.y
            , vec2 halfExtents.x -halfExtents.y
            , vec2 halfExtents.x halfExtents.y
            , vec2 -halfExtents.x halfExtents.y
            ]
    in
    List.map (Isometry.apply iso) localPoints
