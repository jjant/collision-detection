port module ConfigSchema exposing (myConfigFields)

import ConfigFormGenerator exposing (Kind(..))
import Html exposing (Html)
import Platform


port generateFile : ( String, String ) -> Cmd msg


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


main : Program {} {} {}
main =
    let
        generatedElmCode =
            ConfigFormGenerator.toFile myConfigFields
    in
    Platform.worker
        { init = \_ -> ( {}, generateFile ( "fileName", generatedElmCode ) )
        , update = \_ _ -> ( {}, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
