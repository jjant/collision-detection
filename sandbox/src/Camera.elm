module Camera exposing
    ( Camera
    , inverseMatrix
    , matrix
    , new
    , tick
    )

import Keys exposing (Keys)
import Mat3 exposing (Mat3)
import Vec2 exposing (Vec2, vec2)


type alias Camera =
    { position : Vec2
    , velocity : Vec2
    , rotation : Float
    , viewportSize : Vec2 -- screen resolution?
    }


tick : Float -> Keys -> Camera -> Camera
tick dt keys camera =
    let
        { x, y } =
            speed camera

        v =
            Keys.toDirection keys
                |> Vec2.scaleX x
                |> Vec2.scaleY y
                |> Vec2.scale 5
    in
    { camera
        | position =
            Vec2.add camera.position (Vec2.scale dt v)
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



-- 		float


speed : Camera -> Vec2
speed { viewportSize } =
    let
        x =
            min (viewportSize.x / 1000.0) 2.4

        xFactor =
            0.0366 * (x * x) - 0.1778 * x + 0.3021

        y =
            min (viewportSize.y / 1000.0) 2.4

        yFactor =
            0.0366 * (y * y) - 0.1778 * y + 0.3021
    in
    vec2 xFactor yFactor
