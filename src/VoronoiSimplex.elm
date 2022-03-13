module VoronoiSimplex exposing
    ( IncompleteSimplex(..)
    , Simplex(..)
    , VoronoiSimplex
    , append
    , new
    )

import CSOPoint exposing (CSOPoint)


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
    = One { a : CSOPoint }
    | Simplex Simplex


append : CSOPoint -> IncompleteSimplex -> Simplex
append p s =
    case s of
        One { a } ->
            Two { a = p, b = a }

        Simplex (Two { a, b }) ->
            Three { a = p, b = a, c = b }

        _ ->
            Debug.todo "Unreachable"


type Simplex
    = Two { a : CSOPoint, b : CSOPoint }
    | Three { a : CSOPoint, b : CSOPoint, c : CSOPoint }
