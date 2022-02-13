module Body exposing (Body, Shape(..), contact, projectPoint)

import Circle exposing (Circle, PointProjection)
import Contact exposing (Contact)
import Isometry exposing (Isometry)
import Vec2 exposing (Vec2)


type alias Body =
    { transform : Isometry
    , shape : Shape
    }


type Shape
    = Circle Circle


projectPoint : Vec2 -> Body -> PointProjection
projectPoint point { transform, shape } =
    case shape of
        Circle circle ->
            Circle.projectPoint point transform circle


contact : Body -> Body -> Maybe Contact
contact b1 b2 =
    case ( b1.shape, b2.shape ) of
        ( Circle circle1, Circle circle2 ) ->
            Contact.contactCircleCircle b1.transform.translation circle1 b2.transform.translation circle2
