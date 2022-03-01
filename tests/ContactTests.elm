module ContactTests exposing (..)

import Contact
import Expect exposing (FloatingPointTolerance(..))
import Test exposing (Test, describe, test)
import Vec2 exposing (vec2)


suite : Test
suite =
    describe "Contact"
        [ describe "contactCircleCircle"
            [ test "overlapping circles" <|
                \_ ->
                    let
                        relativeIso =
                            { translation = vec2 2 0, rotation = 0 }
                    in
                    Contact.contactCircleCircle relativeIso { radius = 5 } { radius = 3 } 0
                        |> Maybe.map
                            (\{ normal1, dist } ->
                                Expect.all
                                    [ \_ ->
                                        normal1
                                            |> Expect.equal Vec2.right
                                    , \_ ->
                                        dist
                                            |> Expect.within (Absolute 0.00001) -6
                                    ]
                                    ()
                            )
                        |> Maybe.withDefault (Expect.fail "Circles not in contact")
            ]
        ]
