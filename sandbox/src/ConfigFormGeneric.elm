module ConfigFormGeneric exposing (..)

import Config
import ConfigTypes exposing (Field(..))
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import OrderedDict exposing (OrderedDict)


{-| InitOptions are used to initialize your config and ConfigForm.

    { flags = flagsFromJavascript
    , logics = Config.logics
    , emptyConfig = Config.empty
    }

`Config` is your generated module that was made using [ ConfigFormGenerator](ConfigFormGenerator).

-}
type alias InitOptions config =
    { flags : Encode.Value
    , logics : List (ConfigTypes.Logic config)
    , emptyConfig : config
    }


type alias Flags =
    { file : Encode.Value
    , localStorage : Encode.Value
    }


{-| ConfigForm is the state of the config form. Keep it in your model along with the `config` record.
-}
type ConfigForm
    = ConfigForm
        { fields : OrderedDict String Field
        , fileFields : Dict String Field
        , activeField : Maybe ( FieldState, String )
        }


type FieldState
    = Hovering
    | Dragging


{-| `init` will create both a valid `Config` and `ConfigForm`.
-}
init : InitOptions config -> ( config, ConfigForm )
init options =
    let
        { file, localStorage } =
            decodeFlags
                options.flags

        fileFields =
            decodeFields
                options.logics
                file

        localStorageFields =
            decodeFields
                options.logics
                localStorage

        mergedFields =
            options.logics
                |> List.map
                    (\logic ->
                        ( logic.fieldName
                        , Dict.get logic.fieldName localStorageFields
                            |> Maybe.withDefault
                                (Dict.get logic.fieldName fileFields
                                    |> Maybe.withDefault
                                        (Config.emptyField logic options.emptyConfig)
                                )
                        )
                    )
                |> OrderedDict.fromList
    in
    ( Config.configFromFields options.logics mergedFields options.emptyConfig
    , ConfigForm
        { fields = mergedFields
        , fileFields = fileFields
        , activeField = Nothing
        }
    )


{-| Encodes the current data of your config form to be persisted, including meta-data. This is typically used to save to localStorage.
-}
encodeFields : OrderedDict String Field -> Encode.Value
encodeFields fields =
    fields
        |> OrderedDict.toList
        |> List.filterMap
            (\( fieldName, field ) ->
                Config.encodeField field
                    |> Maybe.map (\json -> ( fieldName, json ))
            )
        |> Encode.object


{-| Encodes the current Config (with some metadata) in your ConfigForm. Usually used for both localStorage and as a .json file.
-}
encode : ConfigForm -> Encode.Value
encode (ConfigForm configForm) =
    Encode.object
        [ ( "fields", encodeFields configForm.fields )
        ]


decodeFlags : Encode.Value -> Flags
decodeFlags json =
    let
        decoder =
            Decode.map2 Flags
                (Decode.field "file" Decode.value)
                (Decode.field "localStorage" Decode.value)
    in
    Decode.decodeValue decoder json
        |> Result.withDefault
            { file = Encode.object []
            , localStorage = Encode.object []
            }


decodeFields : List (ConfigTypes.Logic config) -> Encode.Value -> Dict String Field
decodeFields logics json =
    logics
        |> List.filterMap
            (\logic ->
                Config.decodeField logic json
                    |> Maybe.map
                        (\field ->
                            ( logic.fieldName, field )
                        )
            )
        |> Dict.fromList
