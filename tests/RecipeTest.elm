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
                                , IngredientPart <| ingredient "egg" Nothing
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
                                , IngredientPart <| ingredient "egg" (Just (quantityAmount 1))
                                , PlainPart "."
                                ]
                        )
        , test "ingredient with unit" <|
            \_ ->
                Recipe.parse "Boil <water (200 ml)>."
                    |> Expect.equal
                        (Ok <|
                            Recipe.from
                                [ PlainPart "Boil "
                                , IngredientPart <| ingredient "water" (Just (quantity 200 "ml"))
                                , PlainPart "."
                                ]
                        )
        , test "ingredient with decimal quantity" <|
            \_ ->
                Recipe.parse "Boil <water (0.2 l)>."
                    |> Expect.equal
                        (Ok <|
                            Recipe.from
                                [ PlainPart "Boil "
                                , IngredientPart <| ingredient "water" (Just (quantity 0.2 "l"))
                                , PlainPart "."
                                ]
                        )
        , test "ingredient with amount and different list name" <|
            \_ ->
                Recipe.parse "Cut the <onion (1: large onion)>."
                    |> Expect.equal
                        (Ok <|
                            Recipe.from
                                [ PlainPart "Cut the "
                                , IngredientPart <| ingredientWithName "onion" (Just (quantityAmount 1)) "large onion"
                                , PlainPart "."
                                ]
                        )
        ]
