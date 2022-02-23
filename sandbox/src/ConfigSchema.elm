module ConfigSchema exposing (myConfigFields)

import ConfigFormGenerator exposing (Kind(..))
import Html exposing (Html)


myConfigFields : List ( String, Kind )
myConfigFields =
    [ ( "Translate", SectionKind )
    , ( "Translation X", FloatKind "x" )
    , ( "Translation Y", FloatKind "y" )
    , ( "Support points", BoolKind "showSupportPoints" )
    , ( "Point projections", BoolKind "showPointProjections" )
    ]


main : Html msg
main =
    let
        generatedElmCode =
            ConfigFormGenerator.toFile myConfigFields

        _ =
            Debug.log generatedElmCode ""
    in
    Html.text ""
