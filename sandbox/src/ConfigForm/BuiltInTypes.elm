module ConfigForm.BuiltInTypes exposing
    ( BoolFieldData
    , ColorFieldData
    , ColorFieldMeta(..)
    , FloatFieldData
    , IntFieldData
    , StringFieldData
    )

import Color exposing (Color)
import ColorPicker


type alias IntFieldData =
    { val : Int
    , power : Int
    }


type alias FloatFieldData =
    { val : Float
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
