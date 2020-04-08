module RecipeParser exposing (parse)

import Ingredient exposing (Quantity(..))
import Parser exposing ((|.), (|=), Parser)
import Recipe exposing (Part(..), Recipe)
import Set exposing (Set)


parse : String -> Result ParsingError Recipe
parse input =
    Parser.run parseRecipe input
        |> Result.mapError deadEndsToString


type alias ParsingError =
    String


parseRecipe : Parser Recipe
parseRecipe =
    Parser.succeed
        (\title method ->
            Recipe.from title method
        )
        |. Parser.symbol "# "
        |= Parser.getChompedString (Parser.chompUntil "\n")
        |. Parser.chompWhile (\c -> c == '\n')
        |= Parser.loop ( [], [] ) parseRecursion


parseRecursion : ( List Recipe.Part, Recipe.Parts ) -> Parser (Parser.Step ( List Recipe.Part, Recipe.Parts ) Recipe.Parts)
parseRecursion ( next, paragraphs ) =
    Parser.oneOf
        [ Parser.symbol "\n\n"
            |> Parser.map (\_ -> Parser.Loop ( [], List.reverse next :: paragraphs ))
        , Parser.symbol "\n"
            |> Parser.map (\_ -> Parser.Loop ( PlainPart " " :: next, paragraphs ))
        , Parser.succeed (\ingred -> Parser.Loop ( ingred :: next, paragraphs ))
            |= parseIngredient
        , Parser.succeed (\plain -> Parser.Loop ( plain :: next, paragraphs ))
            |= parsePlain (Set.fromList [ '<', '\n' ])
        , Parser.end |> Parser.map (\_ -> Parser.Done (List.reverse next :: paragraphs |> List.reverse))
        ]


parsePlain : Set Char -> Parser Recipe.Part
parsePlain endChars =
    Parser.getChompedString (Parser.chompWhile (\c -> not (Set.member c endChars)))
        |> Parser.andThen
            (\text ->
                if String.isEmpty text then
                    Parser.problem "Text cannot be empty"

                else
                    Parser.succeed (PlainPart text)
            )


parseIngredient : Parser Recipe.Part
parseIngredient =
    Parser.succeed
        (\text maybeQuantity maybeListName ->
            IngredientPart
                (Ingredient.from
                    (String.trim text)
                    maybeQuantity
                    maybeListName
                )
        )
        |. Parser.symbol "<"
        |= parseUntil (Set.fromList [ ':', ';', '>' ])
        |= parseOptional
            (Parser.succeed identity
                |. Parser.symbol ":"
                |. parseWhitespace
                |= parseQuantity (Set.fromList [ ';', '>' ])
            )
        |= parseOptional
            (Parser.succeed String.trim
                |. Parser.symbol ";"
                |= parseUntil (Set.fromList [ '>' ])
            )
        |. Parser.symbol ">"


parseQuantity : Set Char -> Parser Quantity
parseQuantity endChars =
    Parser.oneOf
        [ Parser.succeed
            (\number maybeUnit ->
                case maybeUnit of
                    Just unit ->
                        Measure number unit

                    Nothing ->
                        Amount number
            )
            |= parseFloat
            |= parseOptional
                (Parser.succeed identity
                    |. Parser.symbol " "
                    |= parseUntil endChars
                )
        , parseDescription endChars
        ]


parseDescription : Set Char -> Parser Quantity
parseDescription endChars =
    Parser.succeed
        (\inside ->
            String.trim inside |> Description
        )
        |= parseUntil endChars


parseUntil : Set Char -> Parser String
parseUntil endChars =
    Parser.getChompedString (Parser.chompWhile (\c -> not (Set.member c endChars)))
        |> Parser.andThen
            (\text ->
                if String.isEmpty text then
                    Parser.problem "Inside text cannot be empty"

                else
                    Parser.succeed text
            )


parseOptional : Parser a -> Parser (Maybe a)
parseOptional parser =
    Parser.oneOf
        [ parser |> Parser.map Just
        , Parser.succeed Nothing
        ]


parseWhitespace : Parser ()
parseWhitespace =
    Parser.chompWhile (\c -> c == ' ' || c == '\t')



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


{-| The built-in float parser has a bug with leading 'e's.
See <https://github.com/elm/parser/issues/28>
-}
parseFloat : Parser Float
parseFloat =
    Parser.backtrackable
        (Parser.oneOf
            [ Parser.symbol "e"
                |> Parser.andThen (\_ -> Parser.problem "A float cannot begin with e")
            , Parser.float
            ]
        )
