module Rectangle exposing (Rectangle, projectLocalPoint)

import AABB
import Circle exposing (PointProjection)
import Vec2 exposing (Vec2)


type alias Rectangle =
    { halfExtents : Vec2
    }


projectLocalPoint : Vec2 -> Rectangle -> PointProjection
projectLocalPoint localPoint { halfExtents } =
    { min = Vec2.scale -1 halfExtents
    , max = halfExtents
    }
        |> AABB.projectLocalPoint localPoint
