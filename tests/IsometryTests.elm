module IsometryTests exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Isometry exposing (Isometry)
import Rectangle
import Test exposing (Test, describe, fuzz, test)
import Vec2 exposing (vec2)


suite : Test
suite =
    describe "Isometry"
        [ invertTests
        , composeTests
        ]


composeTests : Test
composeTests =
    describe "compose"
        [ fuzz fuzzer "identity on the right" <|
            \iso ->
                Isometry.compose iso Isometry.identity
                    |> Expect.equal iso
        , fuzz fuzzer "identity on the left" <|
            \iso ->
                Isometry.compose Isometry.identity iso
                    |> Expect.equal iso
        ]


invertTests : Test
invertTests =
    describe "invert"
        [ fuzz fuzzer "iso times inverse is identity" <|
            \iso ->
                Isometry.compose iso (Isometry.invert iso)
                    |> compare "iso times inverse is identity" Isometry.identity
        , fuzz fuzzer "inverse times iso is identity" <|
            \iso ->
                Isometry.compose (Isometry.invert iso) iso
                    |> compare "inverse times iso is identity" Isometry.identity
        ]


fuzzer : Fuzzer Isometry
fuzzer =
    Fuzz.map3 (\x y theta -> { translation = vec2 x y, rotation = theta })
        Fuzz.float
        Fuzz.float
        Fuzz.float


compare : String -> Isometry -> Isometry -> Expectation
compare testName a b =
    Expect.all
        [ \_ -> comparePrecision 0.1 a.translation.x b.translation.x
        , \_ -> comparePrecision 0.1 a.translation.y b.translation.y
        , \_ -> comparePrecision 0.1 a.rotation b.rotation
        ]
        ()
        |> Expect.onFail ("Test failed `" ++ testName ++ "`:\n" ++ "\t" ++ Debug.toString a ++ "\n\t" ++ Debug.toString b)


comparePrecision : Float -> Float -> Float -> Expectation
comparePrecision precision a b =
    if isNaN a && isNaN b then
        Expect.pass

    else
        Expect.within (Absolute precision) a b
