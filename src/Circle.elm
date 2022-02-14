module Circle exposing
    ( Circle
    , PointProjection
    , projectLocalPoint
    )

import Isometry exposing (Isometry)
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
