-- GENERATED CODE, DO NOT EDIT BY HAND!


module Config exposing (Config, decodeField, empty, emptyField, encodeField, logics, configFromFields)

import Color exposing (Color)
import ColorPicker
import ConfigForm
import ConfigForm.Custom
import ConfigTypes exposing (ColorFieldMeta(..), Field(..), Logic, LogicKind(..))
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import OrderedDict exposing (OrderedDict)


type alias Config =
    { myFloat : Float
    , myInt : Int
    , showSupportPoints : Bool
    , showPointProjections : Bool
    , showContactPoints : Bool
    , backgroundColor : Color
    }


empty : Defaults -> Config
empty defaults =
    { myFloat = defaults.float
    , myInt = defaults.int
    , showSupportPoints = defaults.bool
    , showPointProjections = defaults.bool
    , showContactPoints = defaults.bool
    , backgroundColor = defaults.color
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
        "Background color"
        .backgroundColor
        (\a c -> { c | backgroundColor = a })
    ]





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




type alias Defaults =
    { int : Int
    , float : Float
    , string : String
    , bool : Bool
    , color : Color
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
                    , str = ConfigForm.formatPoweredInt power val
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
                    , str = ConfigForm.formatPoweredFloat power val
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

