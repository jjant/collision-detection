module ContactTests exposing (..)

import Contact
import Expect
import Test exposing (Test, describe, test)
import Vec2 exposing (vec2)


suite : Test
suite =
    describe "Contact"
        [ describe "contactCircleCircle"
            [ test "overlapping circles" <|
                \_ ->
                    Contact.contactCircleCircle (vec2 0 0) { radius = 5 } (vec2 2 0) { radius = 3 }
                        |> Maybe.map
                            (\{ normal, depth } ->
                                Expect.all
                                    [ \_ ->
                                        normal
                                            |> Expect.equal Vec2.right
                                    , \_ ->
                                        depth
                                            |> Expect.greaterThan 0
                                    ]
                                    ()
                            )
                        |> Maybe.withDefault (Expect.fail "Circles not in contact")
            ]
        ]
