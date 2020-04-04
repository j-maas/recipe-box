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
                        (Just <|
                            Recipe.from
                                [ PlainPart "Order some pizza."
                                ]
                        )
        , test "unquantified ingredient" <|
            \_ ->
                Recipe.parse "Cook an <egg>."
                    |> Expect.equal
                        (Just <|
                            Recipe.from
                                [ PlainPart "Cook an "
                                , IngredientPart "egg"
                                , PlainPart "."
                                ]
                        )
        ]
