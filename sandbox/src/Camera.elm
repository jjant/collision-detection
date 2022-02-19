module Camera exposing
    ( Camera
    , inverseMatrix
    , matrix
    , new
    , tick
    )

import Keys exposing (Keys)
import Mat3 exposing (Mat3)
import Vec2 exposing (Vec2)


type alias Camera =
    { position : Vec2
    , velocity : Vec2
    , rotation : Float
    , viewportSize : Vec2 -- screen resolution?
    }


tick : Float -> Keys -> Camera -> Camera
tick dt keys camera =
    { camera
        | position =
            Vec2.add camera.position (Vec2.scale dt (Keys.toDirection keys))
    }


new : { position : Vec2, viewportSize : Vec2 } -> Camera
new { position, viewportSize } =
    { position = position
    , velocity = Vec2.zero
    , viewportSize = viewportSize
    , rotation = 0
    }


matrix : Camera -> Mat3
matrix { position, rotation, viewportSize } =
    let
        size =
            { width = viewportSize.x
            , height = viewportSize.y
            }
    in
    Mat3.mul
        (Mat3.viewport size)
        (Mat3.mul (Mat3.orthographic size)
            (Mat3.lookAt
                { centerOfAttention = position
                , upDirection = Vec2.up
                }
            )
        )


inverseMatrix : Camera -> Mat3
inverseMatrix camera =
    case Mat3.invert (matrix camera) of
        Just m ->
            m

        Nothing ->
            Debug.todo "Camera.inverseMatrix"
