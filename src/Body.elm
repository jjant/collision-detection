module Body exposing (Body, Shape(..), contact, projectPoint)

import Circle exposing (Circle, PointProjection)
import Contact exposing (Contact)
import Isometry exposing (Isometry)
import Rectangle exposing (Rectangle)
import Vec2 exposing (Vec2)


type alias Body =
    { transform : Isometry
    , shape : Shape
    }


type Shape
    = Circle Circle
    | Rectangle Rectangle


projectLocalPoint : Vec2 -> Shape -> PointProjection
projectLocalPoint point shape =
    case shape of
        Circle circle ->
            Circle.projectLocalPoint point circle

        Rectangle rectangle ->
            Rectangle.projectLocalPoint point rectangle


projectPoint : Vec2 -> Body -> PointProjection
projectPoint point { transform, shape } =
    let
        localPoint =
            point
                |> Isometry.applyInverse transform

        localProjection =
            projectLocalPoint localPoint shape
    in
    { localProjection
        | point =
            localProjection.point
                |> Isometry.apply transform
    }


contact : Body -> Body -> Maybe Contact
contact b1 b2 =
    case ( b1.shape, b2.shape ) of
        ( Circle circle1, Circle circle2 ) ->
            Contact.contactCircleCircle b1.transform.translation circle1 b2.transform.translation circle2

        _ ->
            Debug.todo ""
