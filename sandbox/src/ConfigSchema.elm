module ConfigSchema exposing (myConfigFields)

import ConfigFormGenerator exposing (Kind(..))
import Html exposing (Html)


myConfigFields : List ( String, Kind )
myConfigFields =
    [ ( "Translate", SectionKind )
    , ( "Translation X", FloatKind "x" )
    , ( "Translation Y", FloatKind "y" )
    , ( "Rotation 1", FloatKind "rotation1" )
    , ( "Support points", BoolKind "showSupportPoints" )
    , ( "Point projections", BoolKind "showPointProjections" )
    , ( "Contact points", BoolKind "showContactPoints" )
    , ( "Editor UI", SectionKind )
    , ( "Background color", ColorKind "backgroundColor" )
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
