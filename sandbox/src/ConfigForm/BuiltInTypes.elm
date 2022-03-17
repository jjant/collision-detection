module ConfigForm.BuiltInTypes exposing
    ( BoolFieldData, ColorFieldData, ColorFieldMeta(..), FloatFieldData, IntFieldData, StringFieldData
    , BuiltInLogic
    , Lens
    , bool, color, float, int, string, section
    , tuple2Encoder, colorValDecoder, encodeColor
    )

{-| Module defining field types supported out of the box.


# Fields

@docs BoolFieldData, ColorFieldData, ColorFieldMeta, FloatFieldData, IntFieldData, StringFieldData


# Logic helpers

@docs BuiltInLogic
@docs Lens

@docs bool, color, float, int, string, section


# Misc

@docs tuple2Encoder, colorValDecoder, encodeColor

-}

import Color exposing (Color)
import ColorPicker
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


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



---- MISC ----


tuple2Encoder : (a -> Encode.Value) -> (b -> Encode.Value) -> ( a, b ) -> Encode.Value
tuple2Encoder enc1 enc2 ( val1, val2 ) =
    Encode.list identity [ enc1 val1, enc2 val2 ]


encodeColor : Color -> Encode.Value
encodeColor col =
    col
        |> Color.toRgba
        |> (\{ red, green, blue, alpha } ->
                Encode.object
                    [ ( "r", Encode.float red )
                    , ( "g", Encode.float green )
                    , ( "b", Encode.float blue )
                    , ( "a", Encode.float alpha )
                    ]
           )


colorValDecoder : Decoder Color
colorValDecoder =
    Decode.map4 Color.rgba
        (Decode.field "r" Decode.float)
        (Decode.field "g" Decode.float)
        (Decode.field "b" Decode.float)
        (Decode.field "a" Decode.float)
