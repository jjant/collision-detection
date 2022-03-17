module ConfigForm.Generator exposing
    ( Kind(..)
    , toFiles
    )

{-| Imagine being able to add a field to the config form with just one line! It can be done if you use code generation.

Use `ConfigFormGenerator` in your `ConfigSchema.elm` to make a `Config.elm` file (it can be excluded from your `src/` directory if you want, since it won't be compiled directly with your other elm files):

    -- ConfigSchema.elm


    import ConfigFormGenerator exposing (Kind(..))
    import Html exposing (Html)

    myConfigFields : List ( String, Kind )
    myConfigFields =
        [ ( "Header Font Size", IntKind "headerFontSize" )
        , ( "Body Font Size", IntKind "bodyFontSize" )
        , ( "Background Color", ColorKind "bgColor" )
        ]

    main : Html msg
    main =
        let
            generatedElmCode =
                ConfigFormGenerator.toFiles myConfigFields

            _ =
                Debug.log generatedElmCode ""
        in
        Html.text ""

When compiled, it makes an elm app whose sole purpose is to `console.log` the elm code needed for a `Config.elm` file. To generate it, run something like this:

```shell
# Compile schema file to tmp js:
elm make ConfigSchema.elm --output=~/tmp/tmp.js > /dev/null

# Run compiled js with node, which logs out generated elm code, and save to Config.elm:
node ~/tmp/tmp.js > Config.elm 2>/dev/null
```


# How to automate with a watcher script

```shell
#!/bin/bash

CONFIG_SCHEMA_ELMFILE=ConfigSchema.elm
CONFIG_ELMFILE=Config.elm
TMP_JS=~/tmp/gen-config.js
MAIN_ELMFILE=Main.elm
SERVER_DIR=public/
MAIN_JS_OUTPUT=public/js/main.js

GENERATE_ARGS="$CONFIG_SCHEMA_ELMFILE $TMP_JS $CONFIG_ELMFILE"

# Command for generating Config.elm from ConfigSchema.elm
generate_config () {
  CONFIG_SCHEMA_ELMFILE=$1
  TMP_JS=$2
  CONFIG_ELMFILE=$3

  # Use `elm make` to make an elm app that console.logs the generated Config.elm code
  elm make $CONFIG_SCHEMA_ELMFILE --output=$TMP_JS > /dev/null && \
    # Run it with `node` to print the output and write to Config.elm
    node $TMP_JS > $CONFIG_ELMFILE 2>/dev/null
}
export -f generate_config

# Generate the config initially, just in case it doesn't exist
generate_config $GENERATE_ARGS

# Watch for config changes
chokidar $CONFIG_SCHEMA_ELMFILE --command "generate_config $GENERATE_ARGS" &

# Watch for elm changes
elm-live $MAIN_ELMFILE --dir=$SERVER_DIR -- --output=$MAIN_JS_OUTPUT &

wait
```

This will watch for changes to `ConfigSchema.elm` and generate a `Config.elm` file. Make sure you have the following installed, too:

```shell
# (use --save-dev instead of --global if you only need it locally for one project)
npm install --global elm elm-live@next chokidir
```

@docs Kind
@docs toFiles

-}

import Set exposing (Set)


{-| Use these to define what kind of value your field is. For all values except `SectionKind`, the `String` is the field's camelCase variable name for both your `Config` record and its JSON representation, such as "headerFontSize".

`SectionKind` is just for visually organizing your fields.

-}
type Kind
    = IntKind String
    | FloatKind String
    | StringKind String
    | BoolKind String
    | ColorKind String
    | SectionKind
    | CustomKind { fieldName : String, logicName : String }


{-| Generates the elm code for your Config module given a list of labels and field kinds.
-}
toFiles : List ( String, Kind ) -> List ( String, String )
toFiles data =
    let
        customKinds =
            gatherCustomTypes data
    in
    [ ( "ConfigForm/Types.elm"
      , ([ """-- GENERATED CODE, DO NOT EDIT BY HAND!


module ConfigForm.Types exposing (Field(..), Lens, Logic, LogicKind(..))

import Color exposing (Color)
import ConfigForm.BuiltInTypes
import ConfigForm.Custom
"""
         , """type alias Logic config =
    { fieldName : String
    , label : String
    , kind : LogicKind config
    }
"""
         , logicKindType data
         , """type alias Lens big small =
    { getter : big -> small
    , setter : small -> big -> big
    }
"""
         , fieldTypes data
         ]
            |> String.join "\n\n"
        )
            ++ "\n"
      )
    , ( "ConfigForm/Config.Elm"
      , [ header
        , typeAlias data
        , empty data
        , logics data
        , toLogic
        , customLogics data
        , encodeField customKinds
        , defaults customKinds
        , emptyField customKinds
        , configFromFields customKinds
        , decodeField customKinds
        , viewField customKinds
        ]
            |> String.join "\n\n\n"
      )
    ]


header : String
header =
    let
        moduleDeclaration =
            """
-- GENERATED CODE, DO NOT EDIT BY HAND!


module ConfigForm.Config exposing (Config, configFromFields, decodeField, empty, emptyField, encodeField, logics, viewField)
"""

        imports =
            """
import Color exposing (Color)
import ColorPicker
import ConfigForm.View exposing (viewBoolField, viewColorField, viewFloatField, viewIntField, viewStringField, viewSectionField)
import ConfigForm.BuiltInTypes exposing (BuiltInLogic, ColorFieldMeta(..), Lens)
import ConfigForm.Custom
import ConfigForm.Options exposing (ViewOptions)
import ConfigForm.Types exposing (Field(..), Logic, LogicKind(..))
import Element exposing (Element)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import OrderedDict exposing (OrderedDict)
"""
    in
    moduleDeclaration
        ++ imports
        |> String.trim


typeAlias : List ( String, Kind ) -> String
typeAlias data =
    let
        pre =
            "type alias Config ="

        middle =
            data
                |> List.map Tuple.second
                |> List.filterMap typeAliasEntry
                |> List.indexedMap
                    (\i entry ->
                        let
                            pre_ =
                                if i == 0 then
                                    "    { "

                                else
                                    "    , "
                        in
                        pre_ ++ entry
                    )
                |> String.join "\n"

        post =
            "    }"
    in
    [ pre
    , middle
    , post
    ]
        |> String.join "\n"


typeAliasEntry : Kind -> Maybe String
typeAliasEntry kind =
    case ( kindToFieldName kind, kindToType kind ) of
        ( Just fieldName, Just type_ ) ->
            Just (fieldName ++ " : " ++ type_)

        _ ->
            Nothing


empty : List ( String, Kind ) -> String
empty data =
    let
        pre =
            """
empty : Defaults -> Config
empty defaults =
"""
                |> String.trim

        middle =
            data
                |> List.map Tuple.second
                |> List.filterMap emptyEntry
                |> List.indexedMap
                    (\i entry ->
                        let
                            pre_ =
                                if i == 0 then
                                    "    { "

                                else
                                    "    , "
                        in
                        pre_ ++ entry
                    )
                |> String.join "\n"

        post =
            "    }"
    in
    [ pre
    , middle
    , post
    ]
        |> String.join "\n"


emptyEntry : Kind -> Maybe String
emptyEntry kind =
    case ( kindToFieldName kind, kindToDefault kind ) of
        ( Just fieldName, Just default ) ->
            Just (fieldName ++ " = " ++ default)

        _ ->
            Nothing


gatherCustomTypes : List ( String, Kind ) -> Set String
gatherCustomTypes kinds =
    kinds
        |> List.filterMap
            (\( _, kind ) ->
                case kind of
                    CustomKind { logicName } ->
                        Just logicName

                    _ ->
                        Nothing
            )
        |> Set.fromList


logicKindType : List ( String, Kind ) -> String
logicKindType kinds =
    let
        defaultKinds =
            [ ( "Int", "Int" )
            , ( "Float", "Float" )
            , ( "String", "String" )
            , ( "Color", "Color" )
            , ( "Bool", "Bool" )
            , ( "Section", "()" )
            ]
    in
    "type LogicKind config\n"
        ++ "    = "
        ++ (kinds
                |> gatherCustomTypes
                |> Set.toList
                |> List.map (\k -> ( k, customTypeName k ))
                |> (++) defaultKinds
                |> List.map (\( kind, type_ ) -> kind ++ "Logic " ++ "(Lens config " ++ type_ ++ ")")
                |> String.join "\n    | "
           )
        ++ "\n"


customTypeName : String -> String
customTypeName k =
    "ConfigForm.Custom." ++ k


logics : List ( String, Kind ) -> String
logics data =
    let
        pre =
            """
logics : List (Logic Config)
logics =
"""
                |> String.trim

        middle =
            data
                |> List.indexedMap
                    (\i ( label, kind ) ->
                        let
                            pre_ =
                                if i == 0 then
                                    "    [ " ++ kindToLogic kind

                                else
                                    "    , " ++ kindToLogic kind

                            args =
                                kindToLogicArgs ( label, kind )
                                    |> List.map (\str -> "        " ++ str)
                        in
                        (pre_ :: args)
                            |> String.join "\n"
                    )
                |> String.join "\n"

        post =
            "    ]"
    in
    [ pre
    , middle
    , post
    ]
        |> String.join "\n"


kindToType : Kind -> Maybe String
kindToType kind =
    case kind of
        IntKind _ ->
            Just "Int"

        FloatKind _ ->
            Just "Float"

        StringKind _ ->
            Just "String"

        BoolKind _ ->
            Just "Bool"

        ColorKind _ ->
            Just "Color"

        SectionKind ->
            Nothing

        CustomKind { logicName } ->
            Just <| "ConfigForm.Custom." ++ logicName


kindToDefault : Kind -> Maybe String
kindToDefault kind =
    case kind of
        IntKind _ ->
            Just "defaults.int"

        FloatKind _ ->
            Just "defaults.float"

        StringKind _ ->
            Just "defaults.string"

        BoolKind _ ->
            Just "defaults.bool"

        ColorKind _ ->
            Just "defaults.color"

        SectionKind ->
            Nothing

        CustomKind { logicName } ->
            Just <| "defaults." ++ uncapitalize logicName


kindToLogic : Kind -> String
kindToLogic kind =
    case kind of
        IntKind _ ->
            "toLogic IntLogic <| ConfigForm.BuiltInTypes.int"

        FloatKind _ ->
            "toLogic FloatLogic <| ConfigForm.BuiltInTypes.float"

        StringKind _ ->
            "toLogic StringLogic <| ConfigForm.BuiltInTypes.string"

        BoolKind _ ->
            "toLogic BoolLogic <| ConfigForm.BuiltInTypes.bool"

        ColorKind _ ->
            "toLogic ColorLogic <| ConfigForm.BuiltInTypes.color"

        SectionKind ->
            "toLogic SectionLogic <| ConfigForm.BuiltInTypes.section"

        CustomKind { logicName } ->
            uncapitalize logicName


uncapitalize : String -> String
uncapitalize str =
    String.toLower (String.left 1 str) ++ String.dropLeft 1 str


kindToLogicArgs : ( String, Kind ) -> List String
kindToLogicArgs ( label, kind ) =
    case kindToFieldName kind of
        Just fieldName ->
            -- need all args
            let
                fieldLine =
                    "\"" ++ fieldName ++ "\""

                labelLine =
                    "\"" ++ label ++ "\""

                getter =
                    "." ++ fieldName

                setter =
                    "(\\a c -> { c | " ++ fieldName ++ " = a })"
            in
            [ fieldLine
            , labelLine
            , getter
            , setter
            ]

        Nothing ->
            [ "\"" ++ label ++ "\""
            ]


kindToFieldName : Kind -> Maybe String
kindToFieldName kind =
    case kind of
        IntKind str ->
            Just str

        FloatKind str ->
            Just str

        StringKind str ->
            Just str

        BoolKind str ->
            Just str

        ColorKind str ->
            Just str

        SectionKind ->
            Nothing

        CustomKind { fieldName } ->
            Just fieldName


customLogics : List ( String, Kind ) -> String
customLogics kinds =
    gatherCustomTypes kinds
        |> Set.map
            (\kind ->
                let
                    funcName =
                        uncapitalize kind

                    typeName =
                        customTypeName kind

                    logicConstructorName =
                        kind ++ "Logic"

                    templateStr =
                        """$funcName : String -> String -> (config -> $typeName) -> ($typeName -> config -> config) -> Logic config
$funcName fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = $logicConstructorName { getter = getter, setter = setter }
    }"""
                in
                templateStr
                    |> String.replace "$funcName" funcName
                    |> String.replace "$typeName" typeName
                    |> String.replace "$logicConstructorName" logicConstructorName
            )
        |> Set.toList
        |> String.join "\n\n"


fieldTypes : List ( String, Kind ) -> String
fieldTypes data =
    let
        customKinds =
            gatherCustomTypes data
    in
    [ """type Field
    = IntField ConfigForm.BuiltInTypes.IntFieldData
    | FloatField ConfigForm.BuiltInTypes.FloatFieldData
    | StringField ConfigForm.BuiltInTypes.StringFieldData
    | BoolField ConfigForm.BuiltInTypes.BoolFieldData
    | ColorField ConfigForm.BuiltInTypes.ColorFieldData
    | SectionField String"""
        ++ (customKinds
                |> Set.map (\kind -> "\n    | " ++ kind ++ "Field " ++ "(" ++ "ConfigForm.Custom." ++ kind ++ "Field" ++ ")")
                |> Set.toList
                |> String.join ""
           )
    ]
        |> String.join "\n\n\n"


encodeField : Set String -> String
encodeField customKinds =
    let
        customKindCases =
            customKinds
                |> Set.map (\kind -> "        " ++ kind ++ "Field data ->\n" ++ "            " ++ "ConfigForm.Custom.encode" ++ kind ++ " data")
                |> Set.toList
                |> String.join "\n\n"
    in
    """encodeField : Field -> Maybe Value
encodeField field =
    case field of
        IntField data ->
            ( data.val, data.power )
                |> ConfigForm.BuiltInTypes.tuple2Encoder Encode.int Encode.int
                |> Just

        FloatField data ->
            ( data.val, data.power )
                |> ConfigForm.BuiltInTypes.tuple2Encoder Encode.float Encode.int
                |> Just

        StringField data ->
            Encode.string data.val
                |> Just

        BoolField data ->
            Encode.bool data.val
                |> Just

        ColorField data ->
            ConfigForm.BuiltInTypes.encodeColor data.val
                |> Just

        SectionField _ ->
            Nothing

""" ++ customKindCases


defaults : Set String -> String
defaults customKinds =
    """type alias Defaults =
    { int : Int
    , float : Float
    , string : String
    , bool : Bool
    , color : Color
"""
        ++ (customKinds
                |> Set.map (\kind -> "    , " ++ uncapitalize kind ++ " : " ++ customTypeName kind ++ "\n")
                |> Set.toList
                |> String.join ""
           )
        ++ """    }"""


emptyField : Set String -> String
emptyField customKinds =
    let
        base =
            """emptyField : Logic config -> config -> Field
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

"""

        customKindCases =
            customKinds
                |> Set.map (\kind -> "        " ++ kind ++ "Logic lens ->\n" ++ "            " ++ kind ++ "Field <| ConfigForm.Custom.empty" ++ kind ++ " { fieldName = logic.fieldName, label = logic.label, getter = lens.getter } emptyConfig")
                |> Set.toList
                |> String.join "\n\n"
    in
    base ++ customKindCases


configFromFields : Set String -> String
configFromFields customKinds =
    let
        spacing =
            "                    "

        customKindCases =
            customKinds
                |> Set.map
                    (\kind ->
                        spacing
                            ++ "( "
                            ++ "Just ("
                            ++ (kind ++ "Field data")
                            ++ ")"
                            ++ ", "
                            ++ (kind ++ "Logic { setter }")
                            ++ " )"
                            ++ " ->\n"
                            ++ spacing
                            ++ "    "
                            ++ "setter data.val newConfig"
                    )
                |> Set.toList
                |> String.join "\n\n"

        base =
            """configFromFields : List (Logic config) -> OrderedDict String Field -> config -> config
configFromFields logics_ configForm config =
    logics_
        |> List.foldl
            (\\logic newConfig ->
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

"""
                ++ customKindCases
                ++ """

                    _ ->
                        newConfig
            )
            config

"""
    in
    base


decodeField : Set String -> String
decodeField customKinds =
    let
        customKindCases =
            customKinds
                |> Set.map
                    (\kind ->
                        """        $logicNameLogic _ ->
            let
                decoder =
                    Decode.at [ "fields", logic.fieldName ] ConfigForm.Custom.decode$logicNameField
            in
            case Decode.decodeValue decoder json of
                Ok field ->
                    Just <| $logicNameField field

                Err _ ->
                    Nothing"""
                            |> String.replace "$logicName" kind
                    )
                |> Set.toList
                |> String.join "\n\n"
    in
    """decodeField : Logic config -> Decode.Value -> Maybe Field
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
                    Decode.at [ "fields", logic.fieldName ] ConfigForm.BuiltInTypes.colorValDecoder
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

""" ++ customKindCases


viewField : Set String -> String
viewField customKinds =
    let
        spacing =
            "        "

        args =
            """
                { hoveredLabel = hoveredLabel logic.fieldName
                , changedConfigForm = \\f -> changedConfigForm logic.fieldName (Vec2Field f)
                , label = logic.label
                , fieldName = logic.fieldName
                , options = options
                , field = field_
                , index = i
                , isActive = isActive
                }
"""

        customKindCases =
            customKinds
                |> Set.map (\kind -> spacing ++ kind ++ "Field field_ ->\n" ++ spacing ++ "    " ++ "ConfigForm.Custom.view" ++ kind ++ "Field" ++ args)
                |> Set.toList
                |> String.join "\n\n"
    in
    """viewField :
    { hoveredLabel : String -> Bool -> msg
    , changedConfigForm : String -> Field -> msg
    }
    -> ViewOptions
    -> Field
    -> Int
    -> ConfigForm.Types.Logic config
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
                , changedConfigForm = \\f -> changedConfigForm logic.fieldName (IntField f)
                , fieldName = logic.fieldName
                , label = logic.label
                , intField = intField
                , isActive = isActive
                , options = options
                }

        FloatField floatField ->
            viewFloatField
                { hoveredLabel = hoveredLabel
                , changedConfigForm = \\f -> changedConfigForm logic.fieldName (FloatField f)
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

"""
        ++ customKindCases


toLogic : String
toLogic =
    """toLogic : (Lens config value -> LogicKind config) -> BuiltInLogic config value -> Logic config
toLogic constructor { fieldName, label, lens } =
    { fieldName = fieldName
    , label = label
    , kind = constructor lens
    }
"""
