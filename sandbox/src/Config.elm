-- GENERATED CODE, DO NOT EDIT BY HAND!


module Config exposing (Config, configFromFields, decodeField, empty, emptyField, encodeField, logics, viewField)

import Color exposing (Color)
import ColorPicker
import ConfigFormUI exposing (ViewOptions)
import ConfigForm exposing (viewBoolField, viewColorField, viewFloatField, viewIntField, viewStringField, viewSectionField)
import ConfigForm.Custom
import ConfigTypes exposing (ColorFieldMeta(..), Field(..), Logic, LogicKind(..))
import Element exposing (Element)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import OrderedDict exposing (OrderedDict)


type alias Config =
    { myFloat : Float
    , myInt : Int
    , myString : String
    , showSupportPoints : Bool
    , showPointProjections : Bool
    , showContactPoints : Bool
    , backgroundColor : Color
    , sceneBackground : Color
    , collidingBodiesOutline : Color
    , myKind : ConfigForm.Custom.Vec2
    }


empty : Defaults -> Config
empty defaults =
    { myFloat = defaults.float
    , myInt = defaults.int
    , myString = defaults.string
    , showSupportPoints = defaults.bool
    , showPointProjections = defaults.bool
    , showContactPoints = defaults.bool
    , backgroundColor = defaults.color
    , sceneBackground = defaults.color
    , collidingBodiesOutline = defaults.color
    , myKind = defaults.vec2
    }


logics : List (Logic Config)
logics =
    [ ConfigForm.float
        "myFloat"
        "My Float"
        .myFloat
        (\a c -> { c | myFloat = a })
    , ConfigForm.int
        "myInt"
        "My Int"
        .myInt
        (\a c -> { c | myInt = a })
    , ConfigForm.string
        "myString"
        "My String"
        .myString
        (\a c -> { c | myString = a })
    , ConfigForm.section
        "Visualise"
    , ConfigForm.bool
        "showSupportPoints"
        "Support points"
        .showSupportPoints
        (\a c -> { c | showSupportPoints = a })
    , ConfigForm.bool
        "showPointProjections"
        "Point projections"
        .showPointProjections
        (\a c -> { c | showPointProjections = a })
    , ConfigForm.bool
        "showContactPoints"
        "Contact points"
        .showContactPoints
        (\a c -> { c | showContactPoints = a })
    , ConfigForm.section
        "Editor UI"
    , ConfigForm.color
        "backgroundColor"
        "Editor background"
        .backgroundColor
        (\a c -> { c | backgroundColor = a })
    , ConfigForm.color
        "sceneBackground"
        "Scene background"
        .sceneBackground
        (\a c -> { c | sceneBackground = a })
    , ConfigForm.color
        "collidingBodiesOutline"
        "Colliding bodies outline"
        .collidingBodiesOutline
        (\a c -> { c | collidingBodiesOutline = a })
    , ConfigForm.section
        "Custom Kinds"
    , vec2
        "myKind"
        "My custom thing"
        .myKind
        (\a c -> { c | myKind = a })
    ]


vec2 : String -> String -> (config -> ConfigForm.Custom.Vec2) -> (ConfigForm.Custom.Vec2 -> config -> config) -> Logic config
vec2 fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = Vec2Logic { getter = getter, setter = setter }
    }


encodeField : Field -> Maybe Value
encodeField field =
    case field of
        IntField data ->
            ( data.val, data.power )
                |> ConfigForm.tuple2Encoder Encode.int Encode.int
                |> Just

        FloatField data ->
            ( data.val, data.power )
                |> ConfigForm.tuple2Encoder Encode.float Encode.int
                |> Just

        StringField data ->
            Encode.string data.val
                |> Just

        BoolField data ->
            Encode.bool data.val
                |> Just

        ColorField data ->
            ConfigForm.encodeColor data.val
                |> Just

        SectionField _ ->
            Nothing

        Vec2Field data ->
            ConfigForm.Custom.encodeVec2 data


type alias Defaults =
    { int : Int
    , float : Float
    , string : String
    , bool : Bool
    , color : Color
    , vec2 : ConfigForm.Custom.Vec2
    }


emptyField : Logic config -> config -> Field
emptyField logic emptyConfig =
    case logic.kind of
        IntLogic { getter } ->
            IntField
                { val = getter emptyConfig
                , power = 0
                }

        FloatLogic { getter } ->
            FloatField
                { val = getter emptyConfig
                , power = 0
                }

        StringLogic { getter } ->
            StringField
                { val = getter emptyConfig
                }

        BoolLogic { getter } ->
            BoolField
                { val = getter emptyConfig
                }

        ColorLogic { getter } ->
            ColorField
                { val = getter emptyConfig
                , meta =
                    ColorFieldMeta
                        { state = ColorPicker.empty
                        , isOpen = False
                        }
                }

        SectionLogic _ ->
            SectionField logic.fieldName

        Vec2Logic lens ->
            Vec2Field <| ConfigForm.Custom.emptyVec2 { fieldName = logic.fieldName, label = logic.label, getter = lens.getter } emptyConfig


configFromFields : List (Logic config) -> OrderedDict String Field -> config -> config
configFromFields logics_ configForm config =
    logics_
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

                    ( Just (Vec2Field data), Vec2Logic { setter } ) ->
                        setter data.val newConfig

                    _ ->
                        newConfig
            )
            config




decodeField : Logic config -> Decode.Value -> Maybe Field
decodeField logic json =
    case logic.kind of
        IntLogic _ ->
            let
                decoder =
                    Decode.at [ "fields", logic.fieldName ]
                        (Decode.map2
                            Tuple.pair
                            (Decode.index 0 Decode.int)
                            (Decode.index 1 Decode.int)
                        )
            in
            case Decode.decodeValue decoder json of
                Ok ( val, power ) ->
                    { val = val
                    , power = power
                    }
                        |> IntField
                        |> Just

                Err _ ->
                    Nothing

        FloatLogic _ ->
            let
                decoder =
                    Decode.at [ "fields", logic.fieldName ]
                        (Decode.map2 Tuple.pair
                            (Decode.index 0 Decode.float)
                            (Decode.index 1 Decode.int)
                        )
            in
            case Decode.decodeValue decoder json of
                Ok ( val, power ) ->
                    { val = val
                    , power = power
                    }
                        |> FloatField
                        |> Just

                Err _ ->
                    Nothing

        StringLogic _ ->
            let
                decoder =
                    Decode.at [ "fields", logic.fieldName ] Decode.string
            in
            case Decode.decodeValue decoder json of
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
                    Decode.at [ "fields", logic.fieldName ] Decode.bool
            in
            case Decode.decodeValue decoder json of
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
                    Decode.at [ "fields", logic.fieldName ] ConfigForm.colorValDecoder
            in
            case Decode.decodeValue decoder json of
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

        Vec2Logic _ ->
            let
                decoder =
                    Decode.at [ "fields", logic.fieldName ] ConfigForm.Custom.decodeVec2Field
            in
            case Decode.decodeValue decoder json of
                Ok field ->
                    Just <| Vec2Field field

                Err _ ->
                    Nothing


viewField :
    { hoveredLabel : String -> Bool -> msg
    , changedConfigForm : String -> Field -> msg
    }
    -> ViewOptions
    -> Field
    -> Int
    -> ConfigTypes.Logic config
    -> Bool
    -> Element msg
viewField { hoveredLabel, changedConfigForm } options field i logic isActive =
    case field of
        StringField stringField ->
            viewStringField
                { changedConfigForm = changedConfigForm
                , fieldName = logic.fieldName
                , label = logic.label
                , stringField = stringField
                }

        IntField intField ->
            viewIntField
                { hoveredLabel = hoveredLabel
                , changedConfigForm = \f -> changedConfigForm logic.fieldName (IntField f)
                , fieldName = logic.fieldName
                , label = logic.label
                , intField = intField
                , isActive = isActive
                , options = options
                }

        FloatField floatField ->
            viewFloatField
                { hoveredLabel = hoveredLabel
                , changedConfigForm = \f -> changedConfigForm logic.fieldName (FloatField f)
                , options = options
                , fieldName = logic.fieldName
                , label = logic.label
                , floatField = floatField
                , isActive = isActive
                }

        BoolField boolField ->
            viewBoolField
                { options = options
                , changedConfigForm = changedConfigForm
                , fieldName = logic.fieldName
                , label = logic.label
                , boolField = boolField
                }

        ColorField colorField ->
            viewColorField
                { changedConfigForm = changedConfigForm
                , label = logic.label
                , fieldName = logic.fieldName
                , options = options
                , colorField = colorField
                , index = i
                }

        SectionField _ ->
            viewSectionField
                { options = options
                , label = logic.label
                }

        Vec2Field field_ ->
            ConfigForm.Custom.viewVec2Field
                { hoveredLabel = hoveredLabel logic.fieldName
                , changedConfigForm = \f -> changedConfigForm logic.fieldName (Vec2Field f)
                , label = logic.label
                , fieldName = logic.fieldName
                , options = options
                , field = field_
                , index = i
                , isActive = isActive
                }
