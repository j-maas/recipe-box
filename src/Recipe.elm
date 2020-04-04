module Recipe exposing (Recipe, RecipePart(..), from, parse)

import Parser exposing ((|.), (|=), Parser)


type Recipe
    = Recipe RecipeParts


type alias RecipeParts =
    List RecipePart


type RecipePart
    = PlainPart String
    | IngredientPart Ingredient


type alias Ingredient =
    String


from : RecipeParts -> Recipe
from parts =
    Recipe parts


parse : String -> Maybe Recipe
parse input =
    Parser.run parseRecipe input
        |> Result.toMaybe


ingredients : Recipe -> List Ingredient
ingredients (Recipe recipe) =
    List.filterMap
        (\part ->
            case part of
                IngredientPart ingredient ->
                    Just ingredient

                _ ->
                    Nothing
        )
        recipe



-- Parsing


parseRecipe : Parser Recipe
parseRecipe =
    Parser.loop [] parseRecursion


parseRecursion : RecipeParts -> Parser (Parser.Step RecipeParts Recipe)
parseRecursion state =
    Parser.oneOf
        [ Parser.succeed (\ingredient -> Parser.Loop (ingredient :: state))
            |= parseIngredient
        , Parser.succeed (\plain -> Parser.Loop (plain :: state))
            |= parsePlain
        , Parser.end |> Parser.map (\_ -> Parser.Done (Recipe <| List.reverse state))
        ]


parsePlain : Parser RecipePart
parsePlain =
    Parser.getChompedString (Parser.chompWhile (\c -> c /= '<'))
        |> Parser.andThen
            (\text ->
                if String.isEmpty text then
                    Parser.problem "Text cannot be empty"

                else
                    Parser.succeed (PlainPart text)
            )


parseIngredient : Parser RecipePart
parseIngredient =
    Parser.succeed IngredientPart
        |. Parser.symbol "<"
        |= parseIngredientDescription
        |. Parser.symbol ">"


parseIngredientDescription : Parser String
parseIngredientDescription =
    Parser.getChompedString (Parser.chompWhile (\c -> c /= '>'))
