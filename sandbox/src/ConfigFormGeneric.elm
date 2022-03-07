module ConfigFormGeneric exposing (..)


type alias Lens big small =
    { getter : big -> small
    , setter : small -> big -> big
    }
