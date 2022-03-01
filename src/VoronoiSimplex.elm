module VoronoiSimplex exposing (VoronoiSimplex, new)

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
