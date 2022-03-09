module ConfigFormGeneric exposing (..)

import Config
import ConfigForm
import ConfigTypes exposing (Field(..))
import Dict exposing (Dict)
import Element exposing (Element, column, fill, row, spaceEvenly, spacingXY, width)
import Element.Background as Background
import Element.Font as Font
import Html
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Misc
import OrderedDict exposing (OrderedDict)
import Unwrap


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


{-| A Msg is an opaque type for ConfigForm to communicate with your app through ConfigForm.update.
-}
type Msg config
    = ChangedConfigForm String Field
    | ClickedPointerLockLabel String
    | HoveredLabel String Bool
    | MouseMove Int
    | MouseUp


{-| ConfigForm is the state of the config form. Keep it in your model along with the `config` record.
-}
type ConfigForm
    = ConfigForm
        { fields : OrderedDict String Field
        , fileFields : Dict String Field
        , activeField : Maybe ( FieldState, String )
        }


{-| When you receive a Config.Msg, update your `Config` and `ConfigForm` using this. It returns a new `Config` and `ConfigForm`, plus possible json to pass through ports for pointerlock.

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ConfigFormMsg configFormMsg ->
                let
                    ( newConfig, newConfigForm, maybeJsonCmd ) =
                        ConfigForm.update
                            Config.logics
                            model.config
                            model.configForm
                            configFormMsg

                    newModel =
                        { model
                            | config = newConfig
                            , configForm = newConfigForm
                        }
                in
                ( newModel
                , Cmd.batch
                    [ saveToLocalStorageCmd newModel
                    , case maybeJsonCmd of
                        Just jsonCmd ->
                            sendToPort
                                (Json.Encode.object
                                    [ ( "id", Json.Encode.string "CONFIG" )
                                    , ( "val", jsonCmd )
                                    ]
                                )

                        Nothing ->
                            Cmd.none
                    ]
                )

-}
update : List (ConfigTypes.Logic config) -> config -> ConfigForm -> Msg config -> ( config, ConfigForm )
update logics config (ConfigForm configForm) msg =
    case msg of
        ChangedConfigForm fieldName field ->
            let
                newConfigForm =
                    configForm.fields
                        |> OrderedDict.insert fieldName field

                newConfig =
                    Config.configFromFields logics newConfigForm config
            in
            ( newConfig
            , ConfigForm { configForm | fields = newConfigForm }
            )

        ClickedPointerLockLabel fieldName ->
            ( config
            , ConfigForm { configForm | activeField = Just ( Dragging, fieldName ) }
            )

        HoveredLabel fieldName didEnter ->
            ( config
            , ConfigForm
                { configForm
                    | activeField =
                        -- chrome triggers a mouseleave when entering pointerlock,
                        -- so check if you're dragging first, and don't change anything if so
                        case configForm.activeField of
                            Just ( Dragging, _ ) ->
                                configForm.activeField

                            _ ->
                                if didEnter then
                                    Just ( Hovering, fieldName )

                                else
                                    Nothing
                }
            )

        MouseMove num ->
            let
                newConfigForm =
                    case configForm.activeField of
                        Just ( state, fieldName ) ->
                            { configForm
                                | fields =
                                    configForm.fields
                                        |> OrderedDict.update fieldName
                                            (\maybeField ->
                                                case maybeField of
                                                    Just (IntField data) ->
                                                        let
                                                            newVal =
                                                                data.val
                                                                    + (num * (10 ^ data.power))
                                                        in
                                                        Just
                                                            (IntField
                                                                { data
                                                                    | val = newVal
                                                                    , str = ConfigForm.formatPoweredInt data.power newVal
                                                                }
                                                            )

                                                    Just (FloatField data) ->
                                                        let
                                                            newVal =
                                                                data.val
                                                                    + toFloat (num * (10 ^ data.power))
                                                        in
                                                        Just
                                                            (FloatField
                                                                { data | val = newVal, str = ConfigForm.formatPoweredFloat data.power newVal }
                                                            )

                                                    _ ->
                                                        Nothing
                                            )
                            }

                        Nothing ->
                            configForm

                newConfig =
                    Config.configFromFields logics newConfigForm.fields config
            in
            ( newConfig
            , ConfigForm newConfigForm
            )

        MouseUp ->
            ( config
            , ConfigForm
                { configForm
                    | activeField =
                        configForm.activeField
                            |> Maybe.map (\( _, fieldName ) -> ( Hovering, fieldName ))
                }
            )


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


{-| View the config form.
-}



-- view : ViewOptions -> List (ConfigTypes.Logic config) -> ConfigForm -> Element (Msg config)


view viewOptions logics ((ConfigForm configForm) as configFormType) =
    column [ width fill, Font.size viewOptions.fontSize ]
        [ column [ width fill, spacingXY 0 viewOptions.rowSpacing ]
            (logics
                |> List.indexedMap
                    (\i logic ->
                        let
                            field =
                                OrderedDict.get logic.fieldName configForm.fields
                                    |> Unwrap.maybe
                        in
                        row
                            ([ width fill, spaceEvenly ]
                                ++ (case configForm.activeField of
                                        Just ( _, fieldName ) ->
                                            if fieldName == logic.fieldName then
                                                [ Background.color (Misc.toElementColor viewOptions.labelHighlightBgColor)
                                                ]

                                            else
                                                []

                                        Nothing ->
                                            []
                                   )
                            )
                            [ ConfigForm.viewLabel { hoveredLabel = HoveredLabel, onMouseMove = MouseMove, changedConfigForm = ChangedConfigForm } viewOptions field i logic
                            , ConfigForm.viewChanger ChangedConfigForm viewOptions field i logic
                            ]
                    )
            )
        , Element.html <|
            Html.div [ Html.Attributes.id "elm-config-ui-pointerlock" ] []
        , Element.html <|
            Html.node "elm-config-ui-json"
                [ Html.Attributes.attribute
                    "data-encoded-config"
                    (configForm
                        |> ConfigForm
                        |> encode
                        |> Encode.encode 2
                    )
                ]
                []
        ]


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
