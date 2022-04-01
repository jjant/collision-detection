module RectangleTests exposing (suite)

import Expect
import Rectangle
import Test exposing (Test, describe, test)
import Vec2 exposing (vec2)


suite : Test
suite =
    describe "Rectangle"
        [ projectLocalPointTests
        , localSupportPointTests
        ]


projectLocalPointTests : Test
projectLocalPointTests =
    describe "projectLocalPoint"
        [ test "Known point inside Rectangle" <|
            \_ ->
                Rectangle.projectLocalPoint (vec2 0 0) { halfExtents = vec2 2 5 }
                    |> .isInside
                    |> Expect.true "point outside Rectangle"
        , test "Known point outside Rectangle" <|
            \_ ->
                Rectangle.projectLocalPoint (vec2 0 5.00001) { halfExtents = vec2 3 5 }
                    |> .isInside
                    |> Expect.false "point inside Rectangle"
        ]


localSupportPointTests : Test
localSupportPointTests =
    describe "localSupportPoint"
        [ test "top right corner" <|
            \_ ->
                Rectangle.localSupportPoint { halfExtents = vec2 3 5 } (vec2 1 1)
                    |> Expect.equal (vec2 3 5)
        , test "bottom right corner" <|
            \_ ->
                Rectangle.localSupportPoint { halfExtents = vec2 3 5 } (vec2 1 -1)
                    |> Expect.equal (vec2 3 -5)
        , test "top left corner" <|
            \_ ->
                Rectangle.localSupportPoint { halfExtents = vec2 3 5 } (vec2 -1 1)
                    |> Expect.equal (vec2 -3 5)
        , test "bottom left corner" <|
            \_ ->
                Rectangle.localSupportPoint { halfExtents = vec2 3 5 } (vec2 -1 -1)
                    |> Expect.equal (vec2 -3 -5)
        ]
