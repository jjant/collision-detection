module Circle exposing
    ( Circle
    , PointProjection
    , projectLocalPoint
    , projectPoint
    )

import Isometry exposing (Isometry)
import Mat4
import Vec2 exposing (Vec2)
import Vec4


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


projectPoint : Vec2 -> Isometry -> Circle -> PointProjection
projectPoint point iso circle =
    let
        localPoint =
            point
                |> Isometry.applyInverse iso

        localProjection =
            projectLocalPoint localPoint circle
    in
    { localProjection
        | point =
            localProjection.point
                |> Isometry.apply iso
    }
