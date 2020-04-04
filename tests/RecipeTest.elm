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
                        (Ok <|
                            Recipe.from
                                [ PlainPart "Order some pizza."
                                ]
                        )
        , test "unquantified ingredient" <|
            \_ ->
                Recipe.parse "Cook an <egg>."
                    |> Expect.equal
                        (Ok <|
                            Recipe.from
                                [ PlainPart "Cook an "
                                , IngredientPart { name = "egg", quantity = Nothing }
                                , PlainPart "."
                                ]
                        )
        , test "ingredient with amount" <|
            \_ ->
                Recipe.parse "Cook an <egg (1)>."
                    |> Expect.equal
                        (Ok <|
                            Recipe.from
                                [ PlainPart "Cook an "
                                , IngredientPart { name = "egg", quantity = Just 1 }
                                , PlainPart "."
                                ]
                        )
        ]
