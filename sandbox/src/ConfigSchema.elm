module ConfigSchema exposing (myConfigFields)

import ConfigFormGenerator exposing (Kind(..))
import Html exposing (Html)


myConfigFields : List ( String, Kind )
myConfigFields =
    [ ( "Visualise", SectionKind )
    , ( "Support points", BoolKind "showSupportPoints" )
    , ( "Point projections", BoolKind "showPointProjections" )
    , ( "Contact points", BoolKind "showContactPoints" )
    , ( "Editor UI", SectionKind )
    , ( "Background color", ColorKind "backgroundColor" )
    , ( "My custom thing", CustomKind { fieldName = "myKind", logicName = "Vec2" } )
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
