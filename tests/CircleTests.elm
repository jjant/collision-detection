module CircleTests exposing (..)

import Circle
import Expect
import Test exposing (Test, describe, test)
import Vec2 exposing (vec2)


suite : Test
suite =
    describe "Circle"
        [ describe "projectLocalPoint"
            [ test "Known point inside circle" <|
                \_ ->
                    Circle.projectLocalPoint (vec2 0 0) { radius = 5 }
                        |> .isInside
                        |> Expect.true "point outside circle"
            , test "Known point outside circle" <|
                \_ ->
                    Circle.projectLocalPoint (vec2 0 5.00001) { radius = 5 }
                        |> .isInside
                        |> Expect.false "point inside circle"
            ]
        ]
