-- GENERATED CODE, DO NOT EDIT BY HAND!


module Config exposing (Config, empty, logics)

import Color exposing (Color)
import ConfigForm as ConfigForm


type alias Config =
    { x : Float
    , y : Float
    , z : Float
    , sx : Float
    , sy : Float
    , sz : Float
    , thickness : Float
    , fade : Float
    }


empty : ConfigForm.Defaults -> Config
empty defaults =
    { x = defaults.float
    , y = defaults.float
    , z = defaults.float
    , sx = defaults.float
    , sy = defaults.float
    , sz = defaults.float
    , thickness = defaults.float
    , fade = defaults.float
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
    , ConfigForm.float
        "z"
        "Translation Z"
        .z
        (\a c -> { c | z = a })
    , ConfigForm.section
        "Scale"
    , ConfigForm.float
        "sx"
        "Scale X"
        .sx
        (\a c -> { c | sx = a })
    , ConfigForm.float
        "sy"
        "Scale Y"
        .sy
        (\a c -> { c | sy = a })
    , ConfigForm.float
        "sz"
        "Scale Z"
        .sz
        (\a c -> { c | sz = a })
    , ConfigForm.section
        "Render params"
    , ConfigForm.float
        "thickness"
        "Thickness"
        .thickness
        (\a c -> { c | thickness = a })
    , ConfigForm.float
        "fade"
        "Fade"
        .fade
        (\a c -> { c | fade = a })
    ]


--: ""
