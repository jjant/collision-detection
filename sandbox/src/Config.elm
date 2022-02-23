-- GENERATED CODE, DO NOT EDIT BY HAND!


module Config exposing (Config, empty, logics)

import Color exposing (Color)
import ConfigForm as ConfigForm


type alias Config =
    { x : Float
    , y : Float
    , showSupportPoints : Bool
    , showPointProjections : Bool
    }


empty : ConfigForm.Defaults -> Config
empty defaults =
    { x = defaults.float
    , y = defaults.float
    , showSupportPoints = defaults.bool
    , showPointProjections = defaults.bool
    }


--logics : List (ConfigForm.Logic Config)
logics =
    [ ConfigForm.section
        "Translate"
    , ConfigForm.float
        "x"
        "Translation X"
        .x
        (\a c -> { c | x = a })
    , ConfigForm.float
        "y"
        "Translation Y"
        .y
        (\a c -> { c | y = a })
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
    ]


--: ""
