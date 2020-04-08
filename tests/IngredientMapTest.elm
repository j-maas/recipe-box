module IngredientMapTest exposing (suite)

import Dict
import Expect
import Ingredient exposing (Ingredient, Quantity(..))
import IngredientMap
import Test exposing (..)


suite : Test
suite =
    describe "ingredient map"
        [ test "lists ingredients" <|
            \_ ->
                [ ingredient "onions" (Just (Amount 2))
                , ingredient "bell pepper" Nothing
                ]
                    |> IngredientMap.fromIngredients
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
                [ Ingredient.from "egg" (Just (Amount 1)) (Just "eggs")
                , ingredient "water" Nothing
                , Ingredient.from "egg" (Just (Amount 1)) (Just "eggs")
                ]
                    |> IngredientMap.fromIngredients
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


ingredient : String -> Maybe Quantity -> Ingredient
ingredient text quantity =
    Ingredient.from text quantity Nothing
