module Msg exposing (Msg(..))

import Body exposing (Body)
import Config exposing (Config)
import ConfigForm
import Fps
import Keys
import Vec2 exposing (Vec2)


type Msg
    = ConfigFormMsg (ConfigForm.Msg Config)
    | MouseMove Vec2
    | KeysMsg Keys.Msg
    | Tick Float
    | FpsMsg Fps.Msg
    | ChangeBody Body
