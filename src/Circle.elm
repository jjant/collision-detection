module Circle exposing
    ( Circle
    , PointProjection
    , localSupportPoint
    , projectLocalPoint
    )

import Vec2 exposing (Vec2)


type alias Circle =
    { radius : Float
    }


type alias PointProjection =
    { point : Vec2
    , isInside : Bool
    }


projectLocalPoint : Vec2 -> Circle -> PointProjection
projectLocalPoint localPoint { radius } =
    let
        dist2 =
            localPoint
                |> Vec2.lengthSquared
    in
    { point = Vec2.scale (radius / sqrt dist2) localPoint
    , isInside = dist2 < radius * radius
    }


localSupportPoint : Circle -> Vec2 -> Vec2
localSupportPoint { radius } dir =
    dir
        |> Vec2.normalize
        |> Vec2.scale radius
