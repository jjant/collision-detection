-- GENERATED CODE, DO NOT EDIT BY HAND!


module Config exposing (Config, empty, logics)

import Color exposing (Color)
import ConfigForm
import ConfigForm.Custom
import ConfigFormGeneric


type alias Config =
    { showSupportPoints : Bool
    , showPointProjections : Bool
    , showContactPoints : Bool
    , backgroundColor : Color
    , myKind : ConfigForm.Custom.Vec2
    }


empty : ConfigForm.Defaults -> Config
empty defaults =
    { showSupportPoints = defaults.bool
    , showPointProjections = defaults.bool
    , showContactPoints = defaults.bool
    , backgroundColor = defaults.color
    , myKind = defaults.vec2
    }


type alias Logic config =
    { fieldName : String
    , label : String
    , kind : LogicKind config
    }


type LogicKind config
    = IntLogic (ConfigFormGeneric.Lens config Int)
    | FloatLogic (ConfigFormGeneric.Lens config Float)
    | StringLogic (ConfigFormGeneric.Lens config String)
    | ColorLogic (ConfigFormGeneric.Lens config Color)
    | BoolLogic (ConfigFormGeneric.Lens config Bool)
    | SectionLogic (ConfigFormGeneric.Lens config ())
    | Vec2Logic (ConfigFormGeneric.Lens config ConfigForm.Custom.Vec2)


logics : List (ConfigForm.Logic Config)
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


vec2 : String -> String -> (config -> ConfigForm.Custom.Vec2) -> (ConfigForm.Custom.Vec2 -> config -> config) -> Logic config
vec2 fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , kind = Vec2Logic { getter = getter, setter = setter }
    }



--: ""
