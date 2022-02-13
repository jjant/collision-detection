module ConfigSchema exposing (myConfigFields)

import ConfigFormGenerator exposing (Kind(..))
import Html exposing (Html)


myConfigFields : List ( String, Kind )
myConfigFields =
    [ ( "Translate", SectionKind )
    , ( "Translation X", FloatKind "x" )
    , ( "Translation Y", FloatKind "y" )
    , ( "Translation Z", FloatKind "z" )
    , ( "Scale", SectionKind )
    , ( "Scale X", FloatKind "sx" )
    , ( "Scale Y", FloatKind "sy" )
    , ( "Scale Z", FloatKind "sz" )
    , ( "Render params", SectionKind )
    , ( "Thickness", FloatKind "thickness" )
    , ( "Fade", FloatKind "fade" )
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
