-- GENERATED CODE, DO NOT EDIT BY HAND!


module ConfigForm.Types exposing (BoolFieldData, ColorFieldData, IntFieldData, StringFieldData, ColorFieldMeta(..), Lens, Logic, LogicKind(..), Field(..))

import Color exposing (Color)
import ColorPicker
import ConfigForm.BuiltInTypes
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
    | Vec2Logic (Lens config ConfigForm.Custom.Vec2)


type alias Lens big small =
    { getter : big -> small
    , setter : small -> big -> big
    }



type Field
    = IntField IntFieldData
    | FloatField ConfigForm.BuiltInTypes.FloatFieldData
    | StringField StringFieldData
    | BoolField BoolFieldData
    | ColorField ColorFieldData
    | SectionField String
    | Vec2Field (ConfigForm.Custom.Vec2Field)


type alias IntFieldData =
    { val : Int
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
