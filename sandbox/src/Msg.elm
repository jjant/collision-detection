module Msg exposing (Msg(..))

import Body exposing (Body)
import Config exposing (Config)
import ConfigFormGeneric
import Draggable
import Fps
import Keys
import Vec2 exposing (Vec2)


type Msg
    = ConfigFormMsg (ConfigFormGeneric.Msg Config)
    | MouseMove Vec2
    | MouseClick Vec2
    | KeysMsg Keys.Msg
    | Tick Float
    | FpsMsg Fps.Msg
    | ChangeBody Body
    | SelectBody Int
    | DragMsg (Draggable.Msg ())
    | OnDragBy Vec2
    | UpdatePolytope
