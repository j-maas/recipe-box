module ParserTest exposing (suite)

import Expect
import Ingredient exposing (Ingredient, Quantity(..))
import Recipe exposing (Part(..))
import RecipeParser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "parser"
        [ test "title" <|
            \_ ->
                RecipeParser.parse "# The most amazing dish in the world"
                    |> Result.map Recipe.title
                    |> Expect.equal (Ok "The most amazing dish in the world")
        , test "reports empty title" <|
            \_ ->
                RecipeParser.parse "#"
                    |> Result.map Recipe.title
                    |> simplifyFailure
                    |> Expect.equal
                        (Err
                            [ SimpleDeadEnd EmptyText [ TitleContext ]
                            ]
                        )
        , test "no ingredients" <|
            \_ ->
                RecipeParser.parse "# Pizza\n\nOrder some pizza."
                    |> Expect.equal
                        (Ok <|
                            Recipe.from "Pizza"
                                [ [ PlainPart "Order some pizza."
                                  ]
                                ]
                        )
        , test "parses paragraphs" <|
            \_ ->
                RecipeParser.parse "# Pizza\n\nOrder some pizza.\n\nLay back.\n\nEnjoy."
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
                RecipeParser.parse "# Pizza\n\nOrder some pizza.\nLay back.\nEnjoy."
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
                RecipeParser.parse "# Egg\n\nCook an <egg>."
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
                RecipeParser.parse "# Salt\n\nAdd <salt: a pinch>."
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
                RecipeParser.parse "# Float\n<not a float: e as in exponent>"
                    |> Expect.equal
                        (Ok <|
                            Recipe.from "Float"
                                [ [ IngredientPart <| ingredient "not a float" (Just (Description "e as in exponent"))
                                  ]
                                ]
                        )
        , test "allows parentheses in quantities" <|
            \_ ->
                RecipeParser.parse "# Butter\n\nCook in <butter: some (for frying)>."
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
                RecipeParser.parse "# Egg\n\nCook an <egg: 1>."
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
                RecipeParser.parse "# Water\n\nBoil <water: 200 ml>."
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
                RecipeParser.parse "# Water\n\nBoil <water: 0.2 l>."
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
                RecipeParser.parse "# Onion\n\nCut the <onion: 1; large onion>."
                    |> Expect.equal
                        (Ok <|
                            Recipe.from "Onion"
                                [ [ PlainPart "Cut the "
                                  , IngredientPart <| Ingredient.from "onion" (Just (Amount 1)) (Just "large onion")
                                  , PlainPart "."
                                  ]
                                ]
                        )
        , test "reports ingredient without closing bracket" <|
            \_ ->
                RecipeParser.parse "# Unclosed\nI am a <runaway ingredient"
                    |> Result.map Recipe.title
                    |> simplifyFailure
                    |> Expect.equal
                        (Err
                            [ SimpleDeadEnd (Expecting ">") [ IngredientNameContext "runaway ingredient", IngredientContext ]
                            ]
                        )
        ]


simplifyFailure : Result RecipeParser.Failure a -> Result (List SimpleDeadEnd) a
simplifyFailure result =
    Result.mapError simplifyDeadEnds result


type alias SimpleDeadEnd =
    { problem : RecipeParser.Problem
    , contextStack : List RecipeParser.Context
    }


simplifyDeadEnds : RecipeParser.Failure -> List SimpleDeadEnd
simplifyDeadEnds failure =
    failure
        |> List.map
            (\deadEnd ->
                let
                    simpleContextStack =
                        deadEnd.contextStack |> List.map .context
                in
                { problem = deadEnd.problem, contextStack = simpleContextStack }
            )


ingredient : String -> Maybe Quantity -> Ingredient
ingredient text quantity =
    Ingredient.from text quantity Nothing
