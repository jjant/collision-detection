-- GENERATED CODE, DO NOT EDIT BY HAND!


module Config exposing (Config, empty, logics)

import Color exposing (Color)
import ConfigForm
import ConfigForm.Custom
import ConfigFormGeneric
import ConfigTypes exposing (Logic, LogicKind(..))


type alias Config =
    { showSupportPoints : Bool
    , showPointProjections : Bool
    , showContactPoints : Bool
    , backgroundColor : Color
    }


empty : ConfigForm.Defaults -> Config
empty defaults =
    { showSupportPoints = defaults.bool
    , showPointProjections = defaults.bool
    , showContactPoints = defaults.bool
    , backgroundColor = defaults.color
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
    ]


