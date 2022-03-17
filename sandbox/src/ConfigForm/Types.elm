-- GENERATED CODE, DO NOT EDIT BY HAND!


module ConfigForm.Types exposing (Field(..), Lens, Logic, LogicKind(..))

import Color exposing (Color)
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
    = IntField ConfigForm.BuiltInTypes.IntFieldData
    | FloatField ConfigForm.BuiltInTypes.FloatFieldData
    | StringField ConfigForm.BuiltInTypes.StringFieldData
    | BoolField ConfigForm.BuiltInTypes.BoolFieldData
    | ColorField ConfigForm.BuiltInTypes.ColorFieldData
    | SectionField ConfigForm.BuiltInTypes.SectionFieldData
    | Vec2Field (ConfigForm.Custom.Vec2Field)
