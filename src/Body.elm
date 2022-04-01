module Body exposing
    ( Body
    , Shape(..)
    , contact
    , localSupportPoint
    , projectPoint
    , supportPoint
    )

import Circle exposing (Circle, PointProjection)
import Contact exposing (Contact)
import Gjk exposing (LocalSupportMap)
import Isometry exposing (Isometry)
import Rectangle exposing (Rectangle)
import Vec2 exposing (Vec2, point)
import VoronoiSimplex exposing (IncompleteSimplex(..), Simplex(..), VoronoiSimplex)


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


localSupportPoint : Shape -> LocalSupportMap
localSupportPoint shape =
    case shape of
        Circle circle ->
            Circle.localSupportPoint circle

        Rectangle rectangle ->
            Rectangle.localSupportPoint rectangle


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


supportPoint : Vec2 -> Body -> Vec2
supportPoint dir { transform, shape } =
    dir
        |> Vec2.normalize
        |> Isometry.vectorApplyInverse transform
        |> localSupportPoint shape
        |> Isometry.apply transform


contact : Body -> Body -> Maybe Contact
contact b1 b2 =
    case ( b1.shape, b2.shape ) of
        ( Circle circle1, Circle circle2 ) ->
            Contact.contactCircleCircle
                (Isometry.compose
                    (Isometry.invert b1.transform)
                    b2.transform
                )
                circle1
                circle2
                0

        ( Rectangle _, Circle _ ) ->
            -- TODO: Implement
            Nothing

        ( Rectangle _, Rectangle _ ) ->
            Nothing

        _ ->
            -- TODO: Implement
            Nothing


type alias SupportMap =
    Isometry -> Vec2 -> Vec2


type GJKResult
    = Intersection
    | ClosestPoints { point1 : Vec2, point2_1 : Vec2, n1 : Vec2 }
    | Proximity Vec2
    | NoIntersection Vec2


contactSupportMapSupportMap : Isometry -> SupportMap -> SupportMap -> Float -> Maybe Contact
contactSupportMapSupportMap pos12 g1 g2 prediction =
    let
        simplex =
            VoronoiSimplex.new
    in
    case contactSupportMapSupportMapWithParams pos12 g1 g2 prediction simplex Nothing of
        Proximity _ ->
            Debug.todo "Proximity: can't happen"

        Intersection ->
            Debug.todo "Intersection: can't happen"

        NoIntersection _ ->
            Nothing

        ClosestPoints { point1, point2_1, n1 } ->
            let
                dist =
                    Vec2.dot (Vec2.sub point2_1 point1) n1

                point2 =
                    Isometry.applyInverse pos12 point2_1

                normal2 =
                    Isometry.vectorApplyInverse pos12 (Vec2.negate n1)
            in
            Just
                { point1 = point1
                , point2 = point2
                , normal1 = n1
                , normal2 = normal2
                , dist = dist
                }


contactSupportMapSupportMapWithParams : Isometry -> SupportMap -> SupportMap -> Float -> VoronoiSimplex -> Maybe Vec2 -> GJKResult
contactSupportMapSupportMapWithParams pos12 g1 g2 prediction simplex initDirection =
    let
        dir =
            initDirection
                |> Maybe.withDefault (Vec2.normalize pos12.translation)
    in
    Debug.todo "todo"


closestPoints : Isometry -> SupportMap -> Isometry -> SupportMap -> Float -> Bool -> VoronoiSimplex -> GJKResult
closestPoints m1 g1 m2 g2 maxDistance exactDistance simplex =
    Debug.todo ""
