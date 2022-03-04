module VoronoiSimplex exposing
    ( IncompleteSimplex(..)
    , Simplex(..)
    , VoronoiSimplex
    , append
    , doSimplex
    , new
    )

import CSOPoint exposing (CSOPoint)
import Vec2 exposing (Vec2)


type alias VoronoiSimplex =
    { prevVertices : List Int
    , prevDim : Int
    , prevProj : List Float
    , vertices : List CSOPoint
    , proj : List Float
    , dim : Int
    }


new : VoronoiSimplex
new =
    { prevVertices = [ 0, 1, 2 ]
    , prevProj = [ 0, 0 ]
    , prevDim = 0
    , vertices = [ CSOPoint.origin, CSOPoint.origin, CSOPoint.origin ]
    , proj = [ 0, 0 ]
    , dim = 0
    }


type IncompleteSimplex
    = One { a : Vec2 }
    | Simplex Simplex


append : Vec2 -> IncompleteSimplex -> Simplex
append p s =
    case s of
        One { a } ->
            Two { a = p, b = a }

        Simplex (Two { a, b }) ->
            Three { a = p, b = a, c = b }

        _ ->
            Debug.todo "Unreachable"


type Simplex
    = Two { a : Vec2, b : Vec2 }
    | Three { a : Vec2, b : Vec2, c : Vec2 }


doSimplex : Simplex -> ( Simplex, Vec2 )
doSimplex simplex =
    case simplex of
        Two { a, b } ->
            Debug.todo ""

        Three { a, b, c } ->
            Debug.todo "branch 'Three _' not implemented"
