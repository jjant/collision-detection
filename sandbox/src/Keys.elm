module Keys exposing
    ( Keys
    , Msg
    , init
    , subscriptions
    , toDirection
    , update
    )

import Browser.Events
import Json.Decode as Decode exposing (Decoder)
import Vec2 exposing (Vec2, vec2)


type alias Keys =
    { up : Bool
    , right : Bool
    , down : Bool
    , left : Bool
    }


toOne : Bool -> number
toOne x =
    if x then
        1

    else
        0


toDirection : Keys -> Vec2
toDirection keys =
    let
        dx =
            toOne keys.right - toOne keys.left

        dy =
            toOne keys.up - toOne keys.down
    in
    vec2 dx dy
        |> Vec2.normalize


type Key
    = Up
    | Right
    | Left
    | Down


type Msg
    = KeyChange Bool Key


init : Keys
init =
    Keys False False False False


update : Msg -> Keys -> Keys
update (KeyChange isDown key) keys =
    case key of
        Up ->
            { keys | up = isDown }

        Right ->
            { keys | right = isDown }

        Left ->
            { keys | left = isDown }

        Down ->
            { keys | down = isDown }


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Decode.field "key" keyDecoder
            |> Decode.map (KeyChange True)
            |> Browser.Events.onKeyDown
        , Decode.field "key" keyDecoder
            |> Decode.map (KeyChange False)
            |> Browser.Events.onKeyUp
        ]


keyDecoder : Decoder Key
keyDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "w" ->
                        Decode.succeed Up

                    "d" ->
                        Decode.succeed Right

                    "s" ->
                        Decode.succeed Down

                    "a" ->
                        Decode.succeed Left

                    _ ->
                        Decode.fail "Wrong key"
            )
