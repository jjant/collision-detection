module UI exposing (slider)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as Decode


slider : (Int -> msg) -> List (Html msg) -> Html msg
slider onMouseMove children =
    Html.node "elm-config-ui-slider"
        [ Html.Events.on "pl"
            (Decode.at [ "detail", "x" ] Decode.int
                |> Decode.map onMouseMove
            )
        , style "user-select" "none"
        , style "width" "100%"
        , style "display" "block"
        ]
        children
