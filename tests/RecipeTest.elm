module RecipeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Recipe exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "parsing"
        [ test "no ingredients" <|
            \_ ->
                Recipe.parse "Order some pizza."
                    |> Expect.equal
                        (Just
                            [ Plain "Order some pizza."
                            ]
                        )
        ]
