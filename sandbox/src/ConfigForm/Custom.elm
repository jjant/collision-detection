module ConfigForm.Custom exposing (Vec2)

import ConfigForm exposing (Logic)
import ConfigFormGeneric exposing (Lens)
import Vec2


type alias Vec2 =
    Vec2.Vec2


type alias Logic logicKind =
    { fieldName : String
    , label : String
    , kind : logicKind
    }
