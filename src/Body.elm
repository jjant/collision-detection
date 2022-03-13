module Body exposing (Body, Shape(..), contact, gjkIntersection, localSupportPoint, projectPoint, supportPoint)

import Array exposing (Array)
import Circle exposing (Circle, PointProjection)
import Contact exposing (Contact)
import Isometry exposing (Isometry)
import Rectangle exposing (Rectangle)
import Unwrap
import Vec2 exposing (Vec2, point, vec2)
import Vec3
import VoronoiSimplex exposing (IncompleteSimplex(..), Simplex(..), VoronoiSimplex, append)


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

        ( Rectangle rect, Circle circ ) ->
            -- TODO: Implement
            Nothing

        ( Rectangle rectangle1, Rectangle rectangle2 ) ->
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


type alias LocalSupportMap =
    Vec2 -> Vec2


gjkIntersection : Isometry -> LocalSupportMap -> LocalSupportMap -> Result String Simplex
gjkIntersection pos12 g1 g2 =
    let
        initialAxis =
            pos12.translation
                |> Vec2.normalize

        a =
            support pos12 g1 g2 initialAxis
    in
    gjkIntersectionHelp pos12 g1 g2 (One { a = a }) (Vec2.negate a)


type Polytope a
    = Polytope a a a (Array a)


get : Int -> Polytope a -> Maybe a
get index (Polytope a0 a1 a2 rest) =
    case index of
        0 ->
            Just a0

        1 ->
            Just a1

        2 ->
            Just a2

        _ ->
            Array.get (index - 2) rest


length : Polytope a -> Int
length (Polytope _ _ _ rest) =
    3 + Array.length rest


{-| WIP. This should probably return the collision normal, penetration depth, and maybe also the closest points between the two bodies.

I _think_ these points are the ones that effectively generated the normal, but not sure yet.

-}
epa : Polytope Vec2 -> Isometry -> LocalSupportMap -> LocalSupportMap -> Result String Simplex
epa gjkSimplex pos12 g1 g2 =
    let
        len =
            length gjkSimplex

        minIndex =
            0
    in
    -- TODO: Can I assume polytope is counter-clockwise order??
    --
    -- I don't think so, I think the triangle from GJK may be arbitrarily winded.
    -- I'm not sure, so might as well cover both cases in doFace.
    List.range 0 (len - 1)
        |> List.map
            (\i ->
                ( i
                , get i gjkSimplex
                    |> Unwrap.maybe
                , get (modBy len i) gjkSimplex
                    |> Unwrap.maybe
                )
            )
        |> List.map (\( index, vertexI, vertexJ ) -> ( index, doFace vertexI vertexJ ))
        |> (\_ -> Debug.todo "")


{-| Takes an edge (two contiguous vertices) from the polytope (inside the Minkowski difference),
and returns the out-facing normal.
-}
doFace : Vec2 -> Vec2 -> { normal : Vec2, distance : Float }
doFace vertexI vertexJ =
    let
        iToJ =
            Vec2.sub vertexJ vertexI

        tentativeNormal =
            vec2 iToJ.y -iToJ.x
                |> Vec2.normalize

        tentativeDistance =
            Vec2.dot tentativeNormal vertexI
    in
    if tentativeDistance < 0 then
        { normal = Vec2.scale -1 tentativeNormal
        , distance = -tentativeDistance
        }

    else
        { normal = tentativeNormal
        , distance = tentativeDistance
        }


gjkIntersectionHelp : Isometry -> (Vec2 -> Vec2) -> (Vec2 -> Vec2) -> IncompleteSimplex -> Vec2 -> Result String Simplex
gjkIntersectionHelp pos12 g1 g2 incompleteSimplex dir =
    let
        a =
            support pos12 g1 g2 dir
    in
    if Vec2.dot a dir < 0 then
        Err "not found"

    else
        let
            s =
                append a incompleteSimplex

            res =
                doSimplex s
        in
        case res of
            Ok endSimplex ->
                Ok endSimplex

            Err ( newSimplex, newDir ) ->
                gjkIntersectionHelp pos12 g1 g2 (Simplex newSimplex) newDir


support : Isometry -> (Vec2 -> Vec2) -> (Vec2 -> Vec2) -> Vec2 -> Vec2
support pos12 g1 g2 dir =
    let
        support1 =
            g1 dir

        support2 =
            Isometry.apply pos12
                (g2 (Vec2.negate (Isometry.vectorApplyInverse pos12 dir)))
    in
    Vec2.sub support1 support2



-- |> (\_ -> Debug.todo "crash")


doSimplex : Simplex -> Result ( Simplex, Vec2 ) Simplex
doSimplex simplex =
    case simplex of
        Two { a, b } ->
            Err (lineCase a b)

        Three { a, b, c } ->
            triangleCase a b c


lineCase : Vec2 -> Vec2 -> ( Simplex, Vec2 )
lineCase a b =
    let
        ab =
            Vec2.sub b a

        ao =
            Vec2.negate a

        perp =
            tripleProduct ab ao ab
    in
    ( Two { a = a, b = b }, perp )


triangleCase : Vec2 -> Vec2 -> Vec2 -> Result ( Simplex, Vec2 ) Simplex
triangleCase a b c =
    let
        ab =
            Vec2.sub b a

        ac =
            Vec2.sub c a

        ao =
            Vec2.negate a

        abPerp =
            tripleProduct ac ab ab

        acPerp =
            tripleProduct ab ac ac
    in
    if Vec2.dot abPerp ao > 0 then
        -- Err = Keep looking
        Err ( Two { a = a, b = b }, abPerp )

    else if Vec2.dot acPerp ao > 0 then
        -- Err = Keep looking
        Err ( Two { a = a, b = c }, acPerp )

    else
        -- Ok = Done
        Ok (Three { a = a, b = b, c = c })


tripleProduct : Vec2 -> Vec2 -> Vec2 -> Vec2
tripleProduct a b c =
    let
        a3 =
            Vec3.vec3 a.x a.y 0

        b3 =
            Vec3.vec3 b.x b.y 0

        c3 =
            Vec3.vec3 c.x c.y 0

        prod =
            Vec3.cross (Vec3.cross a3 b3) c3
    in
    vec2 prod.x prod.y
