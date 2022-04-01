module Gjk exposing
    ( LocalSupportMap
    , intersection
    , support
    )

import CSOPoint exposing (CSOPoint)
import Isometry exposing (Isometry)
import Vec2 exposing (Vec2, vec2)
import Vec3
import VoronoiSimplex as Simplex
    exposing
        ( IncompleteSimplex(..)
        , Simplex(..)
        )


type alias LocalSupportMap =
    Vec2 -> Vec2


{-| Computes if the bodies are colliding and returns the ending simplex.
-}
intersection : Isometry -> LocalSupportMap -> LocalSupportMap -> { colliding : Bool, simplex : Simplex }
intersection pos12 g1 g2 =
    let
        initialAxis =
            pos12.translation
                |> Vec2.normalize

        a =
            support pos12 g1 g2 initialAxis
    in
    gjkIntersectionHelp pos12 g1 g2 (One { a = a }) (Vec2.negate a.point)


gjkIntersectionHelp : Isometry -> (Vec2 -> Vec2) -> (Vec2 -> Vec2) -> IncompleteSimplex -> Vec2 -> { colliding : Bool, simplex : Simplex }
gjkIntersectionHelp pos12 g1 g2 incompleteSimplex dir =
    let
        a =
            support pos12 g1 g2 dir
    in
    if Vec2.dot a.point dir < 0 then
        { colliding = False

        -- Which should I return here?
        -- I think it might not matter because adding this point to the simplex still returns
        -- something that basically looks like a line here.
        --
        -- , simplex = incompleteSimplex
        , simplex = Simplex.append a incompleteSimplex
        }

    else
        let
            s =
                Simplex.append a incompleteSimplex
        in
        case doSimplex s of
            Done endSimplex ->
                { colliding = True, simplex = endSimplex }

            KeepLooking newSimplex newDir ->
                gjkIntersectionHelp pos12 g1 g2 (Simplex newSimplex) newDir


{-| Computes the support point on the minkowski difference.

Exposed mostly for visualization (i.e, debugging) purposes.

-}
support : Isometry -> (Vec2 -> Vec2) -> (Vec2 -> Vec2) -> Vec2 -> CSOPoint
support pos12 g1 g2 dir =
    let
        support1 =
            g1 dir

        support2 =
            Isometry.apply pos12
                (g2 (Vec2.negate (Isometry.vectorApplyInverse pos12 dir)))
    in
    { point = Vec2.sub support1 support2
    , orig1 = support1
    , orig2 = support2
    }


type DoSimplexResult
    = KeepLooking Simplex Vec2
    | Done Simplex


doSimplex : Simplex -> DoSimplexResult
doSimplex simplex =
    case simplex of
        Two line ->
            let
                ( newSimplex, newSearchDirection ) =
                    lineCase line
            in
            KeepLooking newSimplex newSearchDirection

        Three triangle ->
            triangleCase triangle


lineCase : { a : CSOPoint, b : CSOPoint } -> ( Simplex, Vec2 )
lineCase { a, b } =
    let
        ab =
            Vec2.sub b.point a.point

        ao =
            Vec2.negate a.point

        perp =
            tripleProduct ab ao ab
    in
    ( Two { a = a, b = b }, perp )


triangleCase : { a : CSOPoint, b : CSOPoint, c : CSOPoint } -> DoSimplexResult
triangleCase { a, b, c } =
    let
        ab =
            Vec2.sub b.point a.point

        ac =
            Vec2.sub c.point a.point

        ao =
            Vec2.negate a.point

        abPerp =
            tripleProduct ac ab ab

        acPerp =
            tripleProduct ab ac ac
    in
    if Vec2.dot abPerp ao > 0 then
        KeepLooking (Two { a = a, b = b }) abPerp

    else if Vec2.dot acPerp ao > 0 then
        KeepLooking (Two { a = a, b = c }) acPerp

    else
        Done (Three { a = a, b = b, c = c })


{-| Computes the "triple cross product" for 2D vectors:

    tripleProduct a b c =
        toVec3 a x toVec3 b x toVec3 c
            |> toVec2

-}
tripleProduct : Vec2 -> Vec2 -> Vec2 -> Vec2
tripleProduct a b c =
    let
        toVec3 { x, y } =
            Vec3.vec3 x y 0

        toVec2 { x, y } =
            vec2 x y
    in
    Vec3.cross (Vec3.cross (toVec3 a) (toVec3 b)) (toVec3 c)
        |> toVec2
