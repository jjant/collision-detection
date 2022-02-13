module Body exposing (Body, projectPoint)

import Circle exposing (Circle, PointProjection)
import Isometry exposing (Isometry)
import Vec2 exposing (Vec2)


type alias Body =
    { transform : Isometry
    , shape : Circle
    }


projectPoint : Vec2 -> Body -> PointProjection
projectPoint point { transform, shape } =
    Circle.projectPoint point transform shape
