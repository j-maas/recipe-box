module RecipeTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Recipe exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "recipe"
        [ describe "parsing"
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
            , test "ingredient with descriptive quantity" <|
                \_ ->
                    Recipe.parse "Add <salt (a pinch)>."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from
                                    [ PlainPart "Add "
                                    , IngredientPart <| ingredient "salt" (Just (Description "a pinch"))
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
                                    , IngredientPart <| ingredient "egg" (Just (Amount 1))
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
                                    , IngredientPart <| ingredient "water" (Just (Measure 200 "ml"))
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
                                    , IngredientPart <| ingredient "water" (Just (Measure 0.2 "l"))
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
                                    , IngredientPart <| ingredientWithName "onion" (Just (Amount 1)) "large onion"
                                    , PlainPart "."
                                    ]
                            )
            ]
        , describe "ingredients"
            [ test "lists ingredients" <|
                \_ ->
                    Recipe.from
                        [ PlainPart "Cut the "
                        , IngredientPart <|
                            ingredient "onions" (Just (Amount 2))
                        , PlainPart " and the "
                        , IngredientPart <|
                            ingredient "bell pepper" Nothing
                        , PlainPart ". Put the whole "
                        , IngredientPart <|
                            ingredientWithName "onion" (Just (Amount 1)) "onions"
                        , PlainPart " into the oven."
                        ]
                        |> ingredients
                        |> Expect.equal
                            (ingredientMapFromDict
                                (Dict.fromList
                                    [ ( "onions", [ Amount 2, Amount 1 ] )
                                    , ( "bell pepper", [] )
                                    ]
                                )
                            )
            ]
        ]
