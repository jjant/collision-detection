-- GENERATED CODE, DO NOT EDIT BY HAND!


module Config exposing (Config, empty, emptyField, encodeField, logics, configFromFields)

import Color exposing (Color)
import ColorPicker
import ConfigForm
import ConfigForm.Custom
import ConfigTypes exposing (ColorFieldMeta(..), Field(..), Logic, LogicKind(..))
import Json.Encode as Encode exposing (Value)
import OrderedDict exposing (OrderedDict)


type alias Config =
    { showSupportPoints : Bool
    , showPointProjections : Bool
    , showContactPoints : Bool
    , backgroundColor : Color
    , myKind : ConfigForm.Custom.Vec2
    }


empty : Defaults -> Config
empty defaults =
    { showSupportPoints = defaults.bool
    , showPointProjections = defaults.bool
    , showContactPoints = defaults.bool
    , backgroundColor = defaults.color
    , myKind = defaults.vec2
    }


logics : List (Logic Config)
logics =
    [ ConfigForm.section
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
        "Background color"
        .backgroundColor
        (\a c -> { c | backgroundColor = a })
    , vec2
        "myKind"
        "My custom thing"
        .myKind
        (\a c -> { c | myKind = a })
    ]


vec2 : String -> String -> (config -> ConfigForm.Custom.Vec2) -> (ConfigForm.Custom.Vec2 -> config -> config) -> Logic logicKind
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
                , str = getter emptyConfig |> String.fromInt
                , power = 0
                }

        FloatLogic { getter } ->
            FloatField
                { val = getter emptyConfig
                , str = getter emptyConfig |> String.fromFloat
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

