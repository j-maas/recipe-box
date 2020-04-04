module Recipe exposing (Recipe, RecipePart(..), from, ingredients, map, parse)

import Parser exposing ((|.), (|=), Parser)


type Recipe
    = Recipe RecipeParts


type alias RecipeParts =
    List RecipePart


type RecipePart
    = PlainPart String
    | IngredientPart Ingredient


type alias Ingredient =
    { name : String
    , quantity : Maybe Quantity
    }


type alias Quantity =
    Int


from : RecipeParts -> Recipe
from parts =
    Recipe parts


parse : String -> Result ParsingError Recipe
parse input =
    Parser.run parseRecipe input
        |> Result.mapError deadEndsToString


type alias ParsingError =
    String


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


map : (RecipePart -> a) -> Recipe -> List a
map f (Recipe recipe) =
    List.map f recipe



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
    Parser.succeed
        (\name maybeQuantity ->
            IngredientPart
                { name = name
                , quantity = maybeQuantity
                }
        )
        |. Parser.symbol "<"
        |= (Parser.getChompedString
                (Parser.chompWhile
                    (\c ->
                        not (c == '(' || c == '>')
                    )
                )
                |> Parser.map String.trimRight
           )
        |= parseOptional
            (Parser.succeed identity
                |. chompWhitespace
                |. Parser.symbol "("
                |= Parser.int
                |. Parser.symbol ")"
            )
        |. Parser.symbol ">"


parseOptional : Parser a -> Parser (Maybe a)
parseOptional parser =
    Parser.oneOf
        [ parser |> Parser.map Just
        , Parser.succeed Nothing
        ]


chompWhitespace : Parser ()
chompWhitespace =
    Parser.chompWhile isWhitespace


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t' || c == '\u{000D}' || c == '\n'



-- Miscellaneous


{-|

    The official method is currently a placeholder.
    See https://github.com/elm/parser/issues/9

-}
deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Parser.Problem -> String
problemToString p =
    case p of
        Parser.Expecting s ->
            "expecting '" ++ s ++ "'"

        Parser.ExpectingInt ->
            "expecting int"

        Parser.ExpectingHex ->
            "expecting hex"

        Parser.ExpectingOctal ->
            "expecting octal"

        Parser.ExpectingBinary ->
            "expecting binary"

        Parser.ExpectingFloat ->
            "expecting float"

        Parser.ExpectingNumber ->
            "expecting number"

        Parser.ExpectingVariable ->
            "expecting variable"

        Parser.ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        Parser.ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        Parser.ExpectingEnd ->
            "expecting end"

        Parser.UnexpectedChar ->
            "unexpected char"

        Parser.Problem s ->
            "problem " ++ s

        Parser.BadRepeat ->
            "bad repeat"
