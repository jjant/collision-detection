module Msg exposing (Msg(..))

import Config exposing (Config)
import ConfigForm exposing (ConfigForm)
import Vec2 exposing (Vec2)


type Msg
    = Increment
    | Decrement
    | ConfigFormMsg (ConfigForm.Msg Config)
    | MouseMove Vec2
