port module ConfigSchema exposing (main)

import ConfigFormGenerator exposing (Kind(..))
import Platform


port generateFile : ( String, String ) -> Cmd msg


myConfigFields : List ( String, Kind )
myConfigFields =
    [ ( "Visualise", SectionKind )
    , ( "Support points", BoolKind "showSupportPoints" )
    , ( "Point projections", BoolKind "showPointProjections" )
    , ( "Contact points", BoolKind "showContactPoints" )
    , ( "GJK Simplex", BoolKind "showGjkSimplex" )
    , ( "EPA Polytope", BoolKind "showEpaPolytope" )
    , ( "Minkowski difference", BoolKind "showMinkowskiDifference" )
    , ( "Editor UI", SectionKind )
    , ( "Editor background", ColorKind "backgroundColor" )
    , ( "Scene background", ColorKind "sceneBackground" )
    , ( "Colliding bodies outline", ColorKind "collidingBodiesOutline" )
    , ( "Custom Kinds", SectionKind )
    , ( "My custom thing", CustomKind { fieldName = "myKind", logicName = "Vec2" } )
    ]


main : Program {} {} {}
main =
    let
        generateElmCode =
            ConfigFormGenerator.toFiles myConfigFields
                |> List.map (Tuple.mapFirst ((++) "./src/"))
                |> List.map generateFile
                |> Cmd.batch
    in
    Platform.worker
        { init = \_ -> ( {}, generateElmCode )
        , update = \_ _ -> ( {}, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
