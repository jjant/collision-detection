module ConfigForm.Generic exposing (..)

{-| Entrypoint to the actual configform component functions (update, etc)
-}

import ConfigForm.UI exposing (ViewOptions)
import Dict exposing (Dict)
import Element exposing (Element, column, fill, row, spaceEvenly, spacingXY, width)
import Element.Font as Font
import Html
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import OrderedDict exposing (OrderedDict)
import Unwrap


{-| InitOptions are used to initialize your config and ConfigForm.

    { flags = flagsFromJavascript
    , logics = Config.logics
    , emptyConfig = Config.empty
    }

`Config` is your generated module that was made using [ ConfigFormGenerator](ConfigFormGenerator).

-}
type alias InitOptions logic field config =
    { flags : Encode.Value
    , logics : List logic
    , emptyConfig : config
    , decodeField : logic -> Decode.Value -> Maybe field
    , configFromFields : ConfigFromFields logic field config
    , emptyField : logic -> config -> field
    }


type alias Flags =
    { file : Encode.Value
    , localStorage : Encode.Value
    }


{-| A Msg is an opaque type for ConfigForm to communicate with your app through ConfigForm.update.
-}
type Msg field config
    = ChangedConfigForm String field
    | ClickedPointerLockLabel String
    | HoveredLabel String Bool
    | MouseUp


{-| ConfigForm is the state of the config form. Keep it in your model along with the `config` record.
-}
type ConfigForm field
    = ConfigForm
        { fields : OrderedDict String field
        , fileFields : Dict String field
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
update : ConfigFromFields (Logic logic) field config -> List (Logic logic) -> config -> ConfigForm field -> Msg field config -> ( config, ConfigForm field )
update configFromFields logics config (ConfigForm configForm) msg =
    case msg of
        ChangedConfigForm fieldName field ->
            let
                newConfigForm =
                    configForm.fields
                        |> OrderedDict.insert fieldName field

                newConfig =
                    configFromFields logics newConfigForm config
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


type alias ConfigFromFields logic field config =
    List logic -> OrderedDict String field -> config -> config


{-| `init` will create both a valid `Config` and `ConfigForm`.
-}
init : InitOptions (Logic logic) field config -> ( config, ConfigForm field )
init ({ decodeField, configFromFields, emptyField } as options) =
    let
        { file, localStorage } =
            decodeFlags options.flags

        fileFields =
            decodeFields
                decodeField
                options.logics
                file

        localStorageFields =
            decodeFields
                decodeField
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
                                        (emptyField logic options.emptyConfig)
                                )
                        )
                    )
                |> OrderedDict.fromList
    in
    ( configFromFields options.logics mergedFields options.emptyConfig
    , ConfigForm
        { fields = mergedFields
        , fileFields = fileFields
        , activeField = Nothing
        }
    )


type alias ViewField field logic config =
    { hoveredLabel : String -> Bool -> Msg field config
    , changedConfigForm : String -> field -> Msg field config
    }
    -> ViewOptions
    -> field
    -> Int
    -> logic
    -> Bool
    -> Element (Msg field config)


type alias Logic logic =
    { logic | fieldName : String }


{-| View the config form.
-}
view : (field -> Maybe Encode.Value) -> ViewField field (Logic logic) config -> ViewOptions -> List (Logic logic) -> ConfigForm field -> Element (Msg field config)
view encodeField viewField viewOptions logics (ConfigForm configForm) =
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
                            [ width fill
                            , spaceEvenly
                            ]
                            [ viewField
                                { hoveredLabel = HoveredLabel, changedConfigForm = ChangedConfigForm }
                                viewOptions
                                field
                                i
                                logic
                                (isActive configForm.activeField logic.fieldName)
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
                        |> encode encodeField
                        |> Encode.encode 2
                    )
                ]
                []
        ]


{-| Encodes the current data of your config form to be persisted, including meta-data. This is typically used to save to localStorage.
-}
encodeFields : (field -> Maybe Encode.Value) -> OrderedDict String field -> Encode.Value
encodeFields encodeField fields =
    fields
        |> OrderedDict.toList
        |> List.filterMap
            (\( fieldName, field ) ->
                encodeField field
                    |> Maybe.map (\json -> ( fieldName, json ))
            )
        |> Encode.object


{-| Encodes the current Config (with some metadata) in your ConfigForm. Usually used for both localStorage and as a .json file.
-}
encode : (field -> Maybe Encode.Value) -> ConfigForm field -> Encode.Value
encode encodeField (ConfigForm configForm) =
    Encode.object
        [ ( "fields", encodeFields encodeField configForm.fields )
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


decodeFields : (Logic logic -> Decode.Value -> Maybe field) -> List (Logic logic) -> Encode.Value -> Dict String field
decodeFields decodeField logics json =
    logics
        |> List.filterMap
            (\logic ->
                decodeField logic json
                    |> Maybe.map
                        (\field ->
                            ( logic.fieldName, field )
                        )
            )
        |> Dict.fromList


isActive : Maybe ( x, String ) -> String -> Bool
isActive activeField currentField =
    case activeField of
        Just ( _, fieldName ) ->
            fieldName == currentField

        Nothing ->
            False
