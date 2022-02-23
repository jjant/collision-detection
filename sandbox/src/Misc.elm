module Misc exposing (listIf)


listIf : Bool -> List a -> List a
listIf b l =
    if b then
        l

    else
        []
