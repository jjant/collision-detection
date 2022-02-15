module UtilTests exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util exposing (copySign, copySign2)
import Vec2 exposing (vec2)


suite : Test
suite =
    describe "Util"
        [ copySignTests
        , copySign2Tests
        ]


copySignTests : Test
copySignTests =
    describe "copySign"
        [ test "Positive to positive" <|
            \_ ->
                copySign { from = 5, to = 50 }
                    |> Expect.equal 50
        , test "Positive to negative" <|
            \_ ->
                copySign { from = 5, to = -50 }
                    |> Expect.equal 50
        , test "Negative to positive" <|
            \_ ->
                copySign { from = -5, to = 50 }
                    |> Expect.equal -50
        , test "Negative to negative" <|
            \_ ->
                copySign { from = -5, to = -123 }
                    |> Expect.equal -123
        ]


copySign2Tests : Test
copySign2Tests =
    describe "copySign2"
        [ test "Positive to positive" <|
            \_ ->
                copySign2 { from = vec2 -13 3, to = vec2 109 -2 }
                    |> Expect.equal (vec2 -109 2)
        ]
