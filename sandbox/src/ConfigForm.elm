module ConfigForm exposing
    ( ConfigForm, init, InitOptions
    , Msg
    , update
    , encode
    , view
    , viewOptions, withFontSize, withRowSpacing, withInputWidth, withInputSpacing, withLabelHighlightBgColor, withSectionSpacing
    , int, float, string, bool, color, section
    , encodeColor, tuple2Encoder
    )

{-| Note: The `config` in the following type signatures is a record of all your config values, like...

    type alias Config =
        { headerFontSize : Int
        , bodyFontSize : Int
        , backgroundColor : Color
        }

Also, `Value` is shorthand for `Json.Encode.Value`.

@docs ConfigForm, init, InitOptions


# Msg

@docs Msg


# Update

@docs update


# Encoding

@docs encode


# View

@docs view


# View options

@docs viewOptions, withFontSize, withRowSpacing, withInputWidth, withInputSpacing, withLabelHighlightBgColor, withSectionSpacing


# Used only by generated Config code

@docs int, float, string, bool, color, section

-}

import Color exposing (Color)
import ColorPicker
import ConfigTypes exposing (ColorFieldMeta(..), Field(..), Logic, LogicKind(..))
import Dict exposing (Dict)
import Element exposing (Element, centerX, centerY, column, el, fill, height, rgba255, row, spaceEvenly, spacingXY, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as JD
import Json.Encode as JE
import Misc
import OrderedDict exposing (OrderedDict)
import Round
import UI exposing (slider)


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


{-| InitOptions are used to initialize your config and ConfigForm.

    { flags = flagsFromJavascript
    , logics = Config.logics
    , emptyConfig = Config.empty
    }

`Config` is your generated module that was made using [ ConfigFormGenerator](ConfigFormGenerator).

-}
type alias InitOptions config =
    { flags : JE.Value
    , logics : List (ConfigTypes.Logic config)
    , emptyConfig : config
    }


type alias Flags =
    { file : JE.Value
    , localStorage : JE.Value
    }


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


{-| Creates the logic for Int values
-}
int : String -> String -> (config -> Int) -> (Int -> config -> config) -> Logic config
int fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = IntLogic { getter = getter, setter = setter }
    }


{-| Creates the logic for Float values
-}
float : String -> String -> (config -> Float) -> (Float -> config -> config) -> Logic config
float fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = FloatLogic { getter = getter, setter = setter }
    }


{-| Creates the logic for String values
-}
string : String -> String -> (config -> String) -> (String -> config -> config) -> Logic config
string fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = StringLogic { getter = getter, setter = setter }
    }


{-| Creates the logic for Bool values
-}
bool : String -> String -> (config -> Bool) -> (Bool -> config -> config) -> Logic config
bool fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = BoolLogic { getter = getter, setter = setter }
    }


{-| Creates the logic for Color values
-}
color : String -> String -> (config -> Color) -> (Color -> config -> config) -> Logic config
color fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = ColorLogic { getter = getter, setter = setter }
    }


{-| Creates the logic for Section values
-}
section : String -> Logic config
section sectionStr =
    { fieldName = ""
    , label = sectionStr
    , kind =
        SectionLogic
            { getter = \_ -> ()
            , setter = \_ config -> config
            }
    }


{-| A Msg is an opaque type for ConfigForm to communicate with your app through ConfigForm.update.
-}
type Msg config
    = ChangedConfigForm String Field
    | ClickedPointerLockLabel String
    | HoveredLabel String Bool
    | MouseMove Int
    | MouseUp


{-| Encodes the current Config (with some metadata) in your ConfigForm. Usually used for both localStorage and as a .json file.
-}
encode : ConfigForm -> JE.Value
encode (ConfigForm configForm) =
    JE.object
        [ ( "fields", encodeFields configForm.fields )
        ]


encodeColor : Color -> JE.Value
encodeColor col =
    col
        |> Color.toRgba
        |> (\{ red, green, blue, alpha } ->
                JE.object
                    [ ( "r", JE.float red )
                    , ( "g", JE.float green )
                    , ( "b", JE.float blue )
                    , ( "a", JE.float alpha )
                    ]
           )


{-| Encodes the current data of your config form to be persisted, including meta-data. This is typically used to save to localStorage.
-}
encodeFields : OrderedDict String Field -> JE.Value
encodeFields fields =
    fields
        |> OrderedDict.toList
        |> List.filterMap
            (\( fieldName, field ) ->
                encodeField field
                    |> Maybe.map (\json -> ( fieldName, json ))
            )
        |> JE.object


tuple2Encoder : (a -> JE.Value) -> (b -> JE.Value) -> ( a, b ) -> JE.Value
tuple2Encoder enc1 enc2 ( val1, val2 ) =
    -- from https://stackoverflow.com/a/52676142
    JE.list identity [ enc1 val1, enc2 val2 ]


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
update : List (Logic config) -> config -> ConfigForm -> Msg config -> ( config, ConfigForm )
update logics config (ConfigForm configForm) msg =
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
                                                                    , str = formatPoweredInt data.power newVal
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
                                                                { data
                                                                    | val = newVal
                                                                    , str = formatPoweredFloat data.power newVal
                                                                }
                                                            )

                                                    _ ->
                                                        Nothing
                                            )
                            }

                        Nothing ->
                            configForm

                newConfig =
                    configFromFields
                        logics
                        newConfigForm.fields
                        config
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


configFromFields : List (Logic config) -> OrderedDict String Field -> config -> config
configFromFields logics configForm config =
    logics
        |> List.foldl
            (\logic newConfig ->
                let
                    maybeField =
                        OrderedDict.get logic.fieldName configForm
                in
                case ( maybeField, logic.kind ) of
                    ( Just (IntField data), IntLogic { setter } ) ->
                        setter data.val newConfig

                    ( Just (FloatField data), FloatLogic { setter } ) ->
                        setter data.val newConfig

                    ( Just (StringField data), StringLogic { setter } ) ->
                        setter data.val newConfig

                    ( Just (BoolField data), BoolLogic { setter } ) ->
                        setter data.val newConfig

                    ( Just (ColorField data), ColorLogic { setter } ) ->
                        setter data.val newConfig

                    _ ->
                        newConfig
            )
            config


formatPoweredInt : Int -> Int -> String
formatPoweredInt power val =
    Round.round -power (toFloat val)


formatPoweredFloat : Int -> Float -> String
formatPoweredFloat power val =
    Round.round -power val


poweredInt : Int -> Int -> Int
poweredInt power val =
    round <| Round.roundNum -power (toFloat val)


poweredFloat : Int -> Float -> Float
poweredFloat power val =
    Round.roundNum -power val


decodeFields : List (Logic config) -> JE.Value -> Dict String Field
decodeFields logics json =
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


decodeField : Logic config -> JE.Value -> Maybe Field
decodeField logic json =
    case logic.kind of
        IntLogic _ ->
            let
                decoder =
                    JD.at [ "fields", logic.fieldName ]
                        (JD.map2
                            Tuple.pair
                            (JD.index 0 JD.int)
                            (JD.index 1 JD.int)
                        )
            in
            case JD.decodeValue decoder json of
                Ok ( val, power ) ->
                    { val = val
                    , str = formatPoweredInt power val
                    , power = power
                    }
                        |> IntField
                        |> Just

                Err _ ->
                    Nothing

        FloatLogic _ ->
            let
                decoder =
                    JD.at [ "fields", logic.fieldName ]
                        (JD.map2 Tuple.pair
                            (JD.index 0 JD.float)
                            (JD.index 1 JD.int)
                        )
            in
            case JD.decodeValue decoder json of
                Ok ( val, power ) ->
                    { val = val
                    , str = formatPoweredFloat power val
                    , power = power
                    }
                        |> FloatField
                        |> Just

                Err _ ->
                    Nothing

        StringLogic _ ->
            let
                decoder =
                    JD.at [ "fields", logic.fieldName ] JD.string
            in
            case JD.decodeValue decoder json of
                Ok val ->
                    { val = val
                    }
                        |> StringField
                        |> Just

                Err _ ->
                    Nothing

        BoolLogic _ ->
            let
                decoder =
                    JD.at [ "fields", logic.fieldName ] JD.bool
            in
            case JD.decodeValue decoder json of
                Ok val ->
                    { val = val
                    }
                        |> BoolField
                        |> Just

                Err _ ->
                    Nothing

        ColorLogic _ ->
            let
                decoder =
                    JD.at [ "fields", logic.fieldName ] colorValDecoder
            in
            case JD.decodeValue decoder json of
                Ok val ->
                    { val = val
                    , meta =
                        ColorFieldMeta
                            { state = ColorPicker.empty
                            , isOpen = False
                            }
                    }
                        |> ColorField
                        |> Just

                Err _ ->
                    Nothing

        SectionLogic _ ->
            logic.fieldName
                |> SectionField
                |> Just



-- JSON encode/decoder stuff


decodeFlags : JE.Value -> Flags
decodeFlags json =
    let
        decoder =
            JD.map2 Flags
                (JD.field "file" JD.value)
                (JD.field "localStorage" JD.value)
    in
    JD.decodeValue decoder json
        |> Result.withDefault
            { file = JE.object []
            , localStorage = JE.object []
            }


decodeConfig : List (Logic config) -> config -> Flags -> config
decodeConfig logics emptyConfig { file, localStorage } =
    let
        buildConfig json tmpConfig =
            logics
                |> List.foldl
                    (\logic config ->
                        case logic.kind of
                            IntLogic { setter } ->
                                case JD.decodeValue (JD.field logic.fieldName JD.int) json of
                                    Ok intVal ->
                                        setter intVal config

                                    Err _ ->
                                        config

                            FloatLogic { setter } ->
                                case JD.decodeValue (JD.field logic.fieldName JD.float) json of
                                    Ok floatVal ->
                                        setter floatVal config

                                    Err _ ->
                                        config

                            StringLogic { setter } ->
                                case JD.decodeValue (JD.field logic.fieldName JD.string) json of
                                    Ok str ->
                                        setter str config

                                    Err _ ->
                                        config

                            BoolLogic { setter } ->
                                case JD.decodeValue (JD.field logic.fieldName JD.bool) json of
                                    Ok str ->
                                        setter str config

                                    Err _ ->
                                        config

                            ColorLogic { setter } ->
                                case JD.decodeValue (JD.field logic.fieldName colorValDecoder) json of
                                    Ok col ->
                                        setter col config

                                    Err _ ->
                                        config

                            SectionLogic _ ->
                                config
                    )
                    tmpConfig
    in
    emptyConfig
        |> buildConfig file
        |> buildConfig localStorage


colorValDecoder : JD.Decoder Color
colorValDecoder =
    JD.map4 Color.rgba
        (JD.field "r" JD.float)
        (JD.field "g" JD.float)
        (JD.field "b" JD.float)
        (JD.field "a" JD.float)



-- VIEW


{-| View the config form.
-}
view : ViewOptions -> List (Logic config) -> ConfigForm -> Element (Msg config)
view options logics ((ConfigForm configForm) as configFormType) =
    column [ width fill, Font.size options.fontSize ]
        [ column [ width fill, spacingXY 0 options.rowSpacing ]
            (logics
                |> List.indexedMap
                    (\i logic ->
                        row
                            ([ width fill, spaceEvenly ]
                                ++ (case configForm.activeField of
                                        Just ( state, fieldName ) ->
                                            if fieldName == logic.fieldName then
                                                [ Background.color (Misc.toElementColor options.labelHighlightBgColor)
                                                ]

                                            else
                                                []

                                        Nothing ->
                                            []
                                   )
                            )
                            [ viewLabel options configFormType i logic
                            , viewChanger options configFormType i logic
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
                        |> JE.encode 2
                    )
                ]
                []
        ]


viewLabel : ViewOptions -> ConfigForm -> Int -> Logic config -> Element (Msg config)
viewLabel options configForm i logic =
    case logic.kind of
        StringLogic _ ->
            row
                []
                [ Element.text logic.label ]

        IntLogic _ ->
            row
                (List.map Element.htmlAttribute (resizeAttrs options configForm logic))
                [ Element.html <| slider MouseMove [ Html.text logic.label ]
                , Element.html <| powerEl options configForm logic
                ]

        FloatLogic _ ->
            row
                (List.map Element.htmlAttribute (resizeAttrs options configForm logic))
                [ Element.html <| slider MouseMove [ Html.text logic.label ]
                , Element.html <| powerEl options configForm logic
                ]

        BoolLogic _ ->
            row
                []
                [ Element.text logic.label ]

        ColorLogic _ ->
            row
                [ width fill ]
                [ Element.text logic.label
                , closeEl options configForm i logic.fieldName
                ]

        SectionLogic _ ->
            row
                [ Font.bold

                -- , style "padding" (pxInt options.sectionSpacing ++ " 0 5px 0")
                ]
                [ Element.text logic.label ]


closeEl : { r | fontSize : Int } -> ConfigForm -> Int -> String -> Element (Msg config)
closeEl options (ConfigForm configForm) i fieldName =
    let
        maybeCloseMsg =
            case OrderedDict.get fieldName configForm.fields of
                Just (ColorField data) ->
                    let
                        shouldShow =
                            case data.meta of
                                ColorFieldMeta meta ->
                                    meta.isOpen
                    in
                    if shouldShow then
                        let
                            meta =
                                case data.meta of
                                    ColorFieldMeta m ->
                                        m
                        in
                        Just
                            (ChangedConfigForm
                                fieldName
                                (ColorField
                                    { data
                                        | meta =
                                            ColorFieldMeta
                                                { meta
                                                    | isOpen = False
                                                }
                                    }
                                )
                            )

                    else
                        Nothing

                _ ->
                    Nothing
    in
    case maybeCloseMsg of
        Just msg ->
            Input.button
                [ Background.color (rgba255 255 255 255 0.2)
                , Border.width 1
                , Border.color (rgba255 0 0 0 0.9)
                , Border.rounded 4
                , width (Element.px (round (1.5 * toFloat options.fontSize)))
                , height (Element.px (round (1.5 * toFloat options.fontSize)))

                -- , Html.Attributes.tabindex (1 + i)
                ]
                { onPress = Just msg
                , label = el [ centerX, centerY ] (Element.text "❌")
                }

        Nothing ->
            Element.none


formattedPower : Int -> String
formattedPower power =
    let
        numStr =
            if power >= 0 then
                String.fromInt (10 ^ power)

            else
                "0." ++ String.repeat (-1 - power) "0" ++ "1"
    in
    "x" ++ numStr


powerEl : ViewOptions -> ConfigForm -> Logic config -> Html (Msg config)
powerEl options (ConfigForm configForm) logic =
    let
        makePowerEl power newIncField newDecField isDownDisabled =
            Html.div
                [ style "position" "absolute"
                , style "top" "0px"
                , style "right" "0"
                , style "height" "100%"
                , style "box-sizing" "border-box"
                , style "display" "flex"
                , style "align-items" "center"
                , style "padding-left" (px (0.45 * inputFieldVertPadding options))
                , style "font-size" (px (0.8 * toFloat options.fontSize))
                , style "background" (Color.toCssString options.labelHighlightBgColor)
                , style "background"
                    ([ "linear-gradient(to right,"
                     , "transparent,"
                     , Color.toCssString options.labelHighlightBgColor ++ " 10%,"
                     , Color.toCssString options.labelHighlightBgColor
                     ]
                        |> String.join " "
                    )
                , style "pointer-events" "none"
                ]
                [ Html.span
                    [ style "padding" "5px 0"
                    , style "pointer-events" "none"
                    , style "user-select" "none"
                    ]
                    -- label
                    [ Html.text (formattedPower power) ]
                , Html.span
                    [ style "font-size" (0.8 * toFloat options.fontSize |> px)
                    , style "top" "1px"
                    , style "pointer-events" "all"
                    , Pointer.onWithOptions "pointerdown"
                        { stopPropagation = True
                        , preventDefault = True
                        }
                        (\_ -> ChangedConfigForm logic.fieldName newIncField)
                    , if isDownDisabled then
                        style "opacity" "0.4"

                      else
                        style "cursor" "pointer"
                    ]
                    -- down btn
                    [ Html.text "↙️" ]
                , Html.span
                    [ style "font-size" (0.8 * toFloat options.fontSize |> px)
                    , style "top" "1px"
                    , style "pointer-events" "all"
                    , Pointer.onWithOptions "pointerdown"
                        { stopPropagation = True
                        , preventDefault = True
                        }
                        (\_ -> ChangedConfigForm logic.fieldName newDecField)
                    , style "cursor" "pointer"
                    ]
                    -- up btn
                    [ Html.text "↗️" ]
                ]

        el =
            case OrderedDict.get logic.fieldName configForm.fields of
                Just (IntField data) ->
                    makePowerEl data.power
                        (IntField
                            { data
                                | power = data.power - 1 |> max 0
                                , str = formatPoweredInt (data.power - 1 |> max 0) data.val
                                , val = poweredInt (data.power - 1 |> max 0) data.val
                            }
                        )
                        (IntField
                            { data
                                | power = data.power + 1
                                , str = formatPoweredInt (data.power + 1) data.val
                                , val = poweredInt (data.power + 1) data.val
                            }
                        )
                        (data.power <= 0)

                Just (FloatField data) ->
                    makePowerEl data.power
                        (FloatField
                            { data
                                | power = data.power - 1
                                , str = formatPoweredFloat (data.power - 1) data.val
                                , val = poweredFloat (data.power - 1) data.val
                            }
                        )
                        (FloatField
                            { data
                                | power = data.power + 1
                                , str = formatPoweredFloat (data.power + 1) data.val
                                , val = poweredFloat (data.power + 1) data.val
                            }
                        )
                        False

                _ ->
                    Html.text ""
    in
    case configForm.activeField of
        Just ( state, fieldName ) ->
            if fieldName == logic.fieldName then
                el

            else
                Html.text ""

        Nothing ->
            Html.text ""


resizeAttrs : ViewOptions -> ConfigForm -> Logic config -> List (Html.Attribute (Msg config))
resizeAttrs options configForm logic =
    [ Html.Events.onMouseEnter (HoveredLabel logic.fieldName True)
    , Html.Events.onMouseLeave (HoveredLabel logic.fieldName False)

    --, Html.Events.onMouseDown (ClickedPointerLockLabel logic.fieldName)
    , style "cursor" "ew-resize"
    , style "height" "100%"
    , style "position" "relative"
    ]


inputFieldVertPadding : ViewOptions -> Float
inputFieldVertPadding options =
    toFloat options.fontSize * options.inputSpacing


viewChanger : ViewOptions -> ConfigForm -> Int -> Logic config -> Element (Msg config)
viewChanger options (ConfigForm configForm) i logic =
    let
        defaultAttrs =
            [ style "width" (pxInt options.inputWidth)
            , style "height" (px (inputFieldVertPadding options))
            ]

        tabAttrs =
            [ Html.Attributes.tabindex (1 + i)
            ]

        incrementalAttrs strToNum wrapper data =
            [ Html.Events.on "keydown"
                (JD.map
                    (\key ->
                        let
                            maybeNewNum =
                                case key of
                                    38 ->
                                        Just <| data.val + 1

                                    40 ->
                                        Just <| data.val - 1

                                    _ ->
                                        Nothing
                        in
                        ChangedConfigForm logic.fieldName
                            (wrapper
                                (case maybeNewNum of
                                    Just newNum ->
                                        { data
                                            | val = newNum
                                            , str = strToNum newNum
                                        }

                                    Nothing ->
                                        data
                                )
                            )
                    )
                    Html.Events.keyCode
                )
            , style "font-variant-numeric" "tabular-nums"
            ]

        maybeField =
            OrderedDict.get logic.fieldName configForm.fields

        colspan =
            case maybeField of
                Just (SectionField _) ->
                    0

                _ ->
                    1
    in
    case maybeField of
        Just (StringField data) ->
            Element.html <|
                Html.td []
                    [ textInputHelper
                        { label = logic.label
                        , valStr = data.val
                        , attrs = defaultAttrs ++ tabAttrs
                        , setterMsg =
                            \newStr ->
                                ChangedConfigForm
                                    logic.fieldName
                                    (StringField { data | val = newStr })
                        }
                    ]

        Just (BoolField data) ->
            Element.html <|
                Html.td []
                    [ Html.input
                        (defaultAttrs
                            ++ tabAttrs
                            ++ [ Html.Attributes.type_ "checkbox"
                               , Html.Attributes.checked data.val
                               , Html.Events.onCheck
                                    (\newBool ->
                                        ChangedConfigForm
                                            logic.fieldName
                                            (BoolField { data | val = newBool })
                                    )
                               ]
                        )
                        []
                    ]

        Just (IntField data) ->
            Element.html <|
                Html.td []
                    [ textInputHelper
                        { label = logic.label
                        , valStr = data.str
                        , attrs =
                            defaultAttrs
                                ++ tabAttrs
                                ++ incrementalAttrs String.fromInt IntField data
                                ++ (if String.toInt data.str == Nothing then
                                        [ style "background" "1,0,0,0.3)" ]

                                    else
                                        []
                                   )
                        , setterMsg =
                            \newStr ->
                                ChangedConfigForm
                                    logic.fieldName
                                <|
                                    IntField
                                        { data
                                            | str = newStr
                                            , val =
                                                case String.toInt newStr of
                                                    Just num ->
                                                        num

                                                    Nothing ->
                                                        data.val
                                        }
                        }
                    ]

        Just (FloatField data) ->
            Element.html <|
                Html.td []
                    [ textInputHelper
                        { label = logic.label
                        , valStr = data.str
                        , attrs =
                            defaultAttrs
                                ++ tabAttrs
                                ++ incrementalAttrs String.fromFloat FloatField data
                                ++ (if String.toFloat data.str == Nothing then
                                        [ style "background" "rgba(1,0,0,0.3)" ]

                                    else
                                        []
                                   )
                        , setterMsg =
                            \newStr ->
                                ChangedConfigForm logic.fieldName <|
                                    FloatField
                                        { data
                                            | str = newStr
                                            , val =
                                                case String.toFloat newStr of
                                                    Just num ->
                                                        num

                                                    Nothing ->
                                                        data.val
                                        }
                        }
                    ]

        Just (ColorField data) ->
            let
                meta =
                    case data.meta of
                        ColorFieldMeta m ->
                            m
            in
            row [ width fill ]
                [ Element.html <|
                    if meta.isOpen then
                        ColorPicker.view data.val meta.state
                            |> Html.map
                                (\pickerMsg ->
                                    let
                                        ( newPickerState, newColor ) =
                                            ColorPicker.update
                                                pickerMsg
                                                data.val
                                                meta.state
                                    in
                                    ChangedConfigForm logic.fieldName
                                        (ColorField
                                            { data
                                                | val = newColor |> Maybe.withDefault data.val
                                                , meta =
                                                    ColorFieldMeta
                                                        { state = newPickerState
                                                        , isOpen = meta.isOpen
                                                        }
                                            }
                                        )
                                )

                    else
                        Html.div
                            (defaultAttrs
                                ++ [ style "background" (Color.toCssString data.val)
                                   , style "width" "100%"
                                   , style "border" "1px solid rgba(0,0,0,0.3)"
                                   , style "border-radius" "3px"
                                   , style "box-sizing" "border-box"
                                   , Html.Events.onMouseDown
                                        (ChangedConfigForm
                                            logic.fieldName
                                            (ColorField
                                                { data
                                                    | meta =
                                                        ColorFieldMeta
                                                            { state = meta.state
                                                            , isOpen = True
                                                            }
                                                }
                                            )
                                        )
                                   ]
                            )
                            []
                ]

        Just (SectionField str) ->
            Element.none

        Nothing ->
            Element.none


textInputHelper :
    { label : String
    , valStr : String
    , attrs : List (Html.Attribute (Msg config))
    , setterMsg : String -> Msg config
    }
    -> Html (Msg config)
textInputHelper { label, valStr, attrs, setterMsg } =
    Html.input
        ([ Html.Attributes.value valStr
         , Html.Events.onInput setterMsg
         , style "font-size" "inherit"
         , style "height" "200px"
         ]
            ++ attrs
        )
        []



-- VIEW OPTIONS


{-| Options for viewing the config form.
-}
type alias ViewOptions =
    { fontSize : Int
    , rowSpacing : Int
    , inputWidth : Int
    , inputSpacing : Float
    , labelHighlightBgColor : Color
    , sectionSpacing : Int
    }


{-| Default options for viewing the config form.
-}
viewOptions : ViewOptions
viewOptions =
    { fontSize = 18
    , rowSpacing = 2
    , inputWidth = 80
    , inputSpacing = 1.4
    , labelHighlightBgColor = Color.rgb 0.8 0.8 1
    , sectionSpacing = 10
    }


{-| Update the font size in px. Default is 18.
-}
withFontSize : Int -> ViewOptions -> ViewOptions
withFontSize val options =
    { options | fontSize = val }


{-| Update the row spacing in px. Default is 2.
-}
withRowSpacing : Int -> ViewOptions -> ViewOptions
withRowSpacing val options =
    { options | rowSpacing = val }


{-| Update the width of inputs in px. Default is 80.
-}
withInputWidth : Int -> ViewOptions -> ViewOptions
withInputWidth val options =
    { options | inputWidth = val }


{-| Update the inner spacing of inputs by a ratio of its font size. Default is 1.40.
-}
withInputSpacing : Float -> ViewOptions -> ViewOptions
withInputSpacing val options =
    { options | inputSpacing = val }


{-| Update the row color when hovering field labels that are pointerlock-able. Default is yellow: (0.8, 0.8, 1).
-}
withLabelHighlightBgColor : Color -> ViewOptions -> ViewOptions
withLabelHighlightBgColor val options =
    { options | labelHighlightBgColor = val }


{-| Update the extra top spacing for sections in px. Default is 20.
-}
withSectionSpacing : Int -> ViewOptions -> ViewOptions
withSectionSpacing val options =
    { options | sectionSpacing = val }



-- MISC INTERNAL


px : Float -> String
px num =
    String.fromFloat num ++ "px"


pxInt : Int -> String
pxInt num =
    String.fromInt num ++ "px"
