-- GENERATED CODE, DO NOT EDIT BY HAND!


module ConfigTypes exposing (BoolFieldData, ColorFieldData, IntFieldData, StringFieldData, FloatFieldData, ColorFieldMeta(..), Lens, Logic, LogicKind(..), Field(..))

import Color exposing (Color)
import ColorPicker
import ConfigForm.Custom



type alias Logic config =
    { fieldName : String
    , label : String
    , kind : LogicKind config
    }



type LogicKind config
    = IntLogic (Lens config Int)
    | FloatLogic (Lens config Float)
    | StringLogic (Lens config String)
    | ColorLogic (Lens config Color)
    | BoolLogic (Lens config Bool)
    | SectionLogic (Lens config ())


type alias Lens big small =
    { getter : big -> small
    , setter : small -> big -> big
    }



type Field
    = IntField IntFieldData
    | FloatField FloatFieldData
    | StringField StringFieldData
    | BoolField BoolFieldData
    | ColorField ColorFieldData
    | SectionField String


type alias IntFieldData =
    { val : Int
    , str : String
    , power : Int
    }


type alias FloatFieldData =
    { val : Float
    , str : String
    , power : Int
    }


type alias StringFieldData =
    { val : String
    }


type alias BoolFieldData =
    { val : Bool
    }


type alias ColorFieldData =
    { val : Color
    , meta : ColorFieldMeta
    }


type ColorFieldMeta
    = ColorFieldMeta
        { state : ColorPicker.State
        , isOpen : Bool
        }
