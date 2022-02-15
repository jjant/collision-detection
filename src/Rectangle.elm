module Rectangle exposing (Rectangle, localSupportPoint, projectLocalPoint)

import AABB
import Circle exposing (PointProjection)
import Util
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


localSupportPoint : Vec2 -> Rectangle -> Vec2
localSupportPoint dir { halfExtents } =
    Util.copySign2 { from = dir, to = halfExtents }
