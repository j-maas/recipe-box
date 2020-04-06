module RecipeTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import IngredientMap
import Recipe exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "recipe"
        [ describe "parsing"
            [ test "title" <|
                \_ ->
                    Recipe.parse "# The most amazing dish in the world\n"
                        |> Result.map Recipe.title
                        |> Expect.equal (Ok "The most amazing dish in the world")
            , test "no ingredients" <|
                \_ ->
                    Recipe.parse "# Pizza\n\nOrder some pizza."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Pizza"
                                    [ [ PlainPart "Order some pizza."
                                      ]
                                    ]
                            )
            , test "parses paragraphs" <|
                \_ ->
                    Recipe.parse "# Pizza\n\nOrder some pizza.\n\nLay back.\n\nEnjoy."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Pizza"
                                    [ [ PlainPart "Order some pizza." ]
                                    , [ PlainPart "Lay back." ]
                                    , [ PlainPart "Enjoy." ]
                                    ]
                            )
            , test "replaces single newlines with space" <|
                \_ ->
                    Recipe.parse "# Pizza\n\nOrder some pizza.\nLay back.\nEnjoy."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Pizza"
                                    [ [ PlainPart "Order some pizza."
                                      , PlainPart " "
                                      , PlainPart "Lay back."
                                      , PlainPart " "
                                      , PlainPart "Enjoy."
                                      ]
                                    ]
                            )
            , test "unquantified ingredient" <|
                \_ ->
                    Recipe.parse "# Egg\n\nCook an <egg>."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Egg"
                                    [ [ PlainPart "Cook an "
                                      , IngredientPart <| ingredient "egg" Nothing
                                      , PlainPart "."
                                      ]
                                    ]
                            )
            , test "ingredient with descriptive quantity" <|
                \_ ->
                    Recipe.parse "# Salt\n\nAdd <salt: a pinch>."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Salt"
                                    [ [ PlainPart "Add "
                                      , IngredientPart <| ingredient "salt" (Just (Description "a pinch"))
                                      , PlainPart "."
                                      ]
                                    ]
                            )

            -- Issue with `Parser.float`. See https://github.com/elm/parser/issues/28
            , test "does not mistake leading e for float" <|
                \_ ->
                    Recipe.parse "# Float\n<not a float: e as in exponent>"
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Float"
                                    [ [ IngredientPart <| ingredient "not a float" (Just (Description "e as in exponent"))
                                      ]
                                    ]
                            )
            , test "allows parentheses in quantities" <|
                \_ ->
                    Recipe.parse "# Butter\n\nCook in <butter: some (for frying)>."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Butter"
                                    [ [ PlainPart "Cook in "
                                      , IngredientPart <| ingredient "butter" (Just (Description "some (for frying)"))
                                      , PlainPart "."
                                      ]
                                    ]
                            )
            , test "ingredient with amount" <|
                \_ ->
                    Recipe.parse "# Egg\n\nCook an <egg: 1>."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Egg"
                                    [ [ PlainPart "Cook an "
                                      , IngredientPart <| ingredient "egg" (Just (Amount 1))
                                      , PlainPart "."
                                      ]
                                    ]
                            )
            , test "ingredient with unit" <|
                \_ ->
                    Recipe.parse "# Water\n\nBoil <water: 200 ml>."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Water"
                                    [ [ PlainPart "Boil "
                                      , IngredientPart <| ingredient "water" (Just (Measure 200 "ml"))
                                      , PlainPart "."
                                      ]
                                    ]
                            )
            , test "ingredient with decimal quantity" <|
                \_ ->
                    Recipe.parse "# Water\n\nBoil <water: 0.2 l>."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Water"
                                    [ [ PlainPart "Boil "
                                      , IngredientPart <| ingredient "water" (Just (Measure 0.2 "l"))
                                      , PlainPart "."
                                      ]
                                    ]
                            )
            , test "ingredient with amount and different list name" <|
                \_ ->
                    Recipe.parse "# Onion\n\nCut the <onion: 1; large onion>."
                        |> Expect.equal
                            (Ok <|
                                Recipe.from "Onion"
                                    [ [ PlainPart "Cut the "
                                      , IngredientPart <| ingredientWithName "onion" (Just (Amount 1)) "large onion"
                                      , PlainPart "."
                                      ]
                                    ]
                            )
            ]
        , describe "ingredients"
            [ test "lists ingredients" <|
                \_ ->
                    [ [ PlainPart "Cut the "
                      , IngredientPart <|
                            ingredient "onions" (Just (Amount 2))
                      , PlainPart " and the "
                      , IngredientPart <|
                            ingredient "bell pepper" Nothing
                      , PlainPart "."
                      ]
                    ]
                        |> IngredientMap.fromDescription
                        |> Expect.equalDicts
                            (Dict.fromList
                                [ ( "onions"
                                  , { descriptions = []
                                    , amount = Just 2
                                    , measures = Dict.empty
                                    }
                                  )
                                , ( "bell pepper"
                                  , { descriptions = []
                                    , amount = Nothing
                                    , measures = Dict.empty
                                    }
                                  )
                                ]
                            )
            , test "adds up ingredients" <|
                \_ ->
                    [ [ PlainPart "Boil an "
                      , IngredientPart <|
                            ingredientWithName "egg" (Just (Amount 1)) "eggs"
                      , PlainPart " in the "
                      , IngredientPart <|
                            ingredient "water" Nothing
                      , PlainPart ". Fry the other "
                      , IngredientPart <|
                            ingredientWithName "egg" (Just (Amount 1)) "eggs"
                      , PlainPart " in a pan."
                      ]
                    ]
                        |> IngredientMap.fromDescription
                        |> Expect.equalDicts
                            (Dict.fromList
                                [ ( "eggs"
                                  , { descriptions = []
                                    , amount = Just 2
                                    , measures = Dict.empty
                                    }
                                  )
                                , ( "water"
                                  , { descriptions = []
                                    , amount = Nothing
                                    , measures = Dict.empty
                                    }
                                  )
                                ]
                            )
            ]
        ]
