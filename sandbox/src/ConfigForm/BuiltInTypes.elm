module ConfigForm.BuiltInTypes exposing
    ( BoolFieldData, ColorFieldData, ColorFieldMeta(..), FloatFieldData, IntFieldData, StringFieldData
    , BuiltInLogic
    , Lens
    , bool, color, float, int, string, section
    )

{-| Module defining field types supported out of the box.


# Fields

@docs BoolFieldData, ColorFieldData, ColorFieldMeta, FloatFieldData, IntFieldData, StringFieldData


# Logic helpers

@docs BuiltInLogic
@docs Lens

@docs bool, color, float, int, string, section

-}

import Color exposing (Color)
import ColorPicker


type alias Lens big small =
    { getter : big -> small
    , setter : small -> big -> big
    }


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


type alias BuiltInLogic config value =
    { fieldName : String
    , label : String
    , lens : Lens config value
    }


{-| Creates the logic for Int values
-}
int : String -> String -> (config -> Int) -> (Int -> config -> config) -> BuiltInLogic config Int
int fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , lens = { getter = getter, setter = setter }
    }


{-| Creates the logic for Float values
-}
float : String -> String -> (config -> Float) -> (Float -> config -> config) -> BuiltInLogic config Float
float fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , lens = { getter = getter, setter = setter }
    }


{-| Creates the logic for String values
-}
string : String -> String -> (config -> String) -> (String -> config -> config) -> BuiltInLogic config String
string fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , lens = { getter = getter, setter = setter }
    }


{-| Creates the logic for Bool values
-}
bool : String -> String -> (config -> Bool) -> (Bool -> config -> config) -> BuiltInLogic config Bool
bool fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , lens = { getter = getter, setter = setter }
    }


{-| Creates the logic for Color values
-}
color : String -> String -> (config -> Color) -> (Color -> config -> config) -> BuiltInLogic config Color
color fieldName label getter setter =
    { fieldName = fieldName
    , label = label
    , lens = { getter = getter, setter = setter }
    }


{-| Creates the logic for Section values
-}
section : String -> BuiltInLogic config ()
section sectionStr =
    { fieldName = ""
    , label = sectionStr
    , lens =
        { getter = \_ -> ()
        , setter = \_ config -> config
        }
    }
