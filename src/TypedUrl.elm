module TypedUrl exposing (TypedUrl, parse)

import Parser exposing ((|.), (|=), Parser)


type alias TypedUrl =
    { scheme : Maybe String
    , authority : List String
    , port_ : Maybe Int
    , path : List String
    , query : List ( String, String )
    , fragment : Maybe String
    }


parse : String -> Maybe TypedUrl
parse raw =
    Parser.run parseTypedUrl raw
        |> Result.toMaybe


parseTypedUrl : Parser TypedUrl
parseTypedUrl =
    Parser.succeed
        (\maybeScheme authority maybePort maybePath maybeQuery maybeFragment ->
            { maybeScheme = maybeScheme
            , authority = authority
            , maybePort = maybePort
            , maybePath = maybePath
            , maybeQuery = maybeQuery
            , maybeFragment = maybeFragment
            }
        )
        |= parseOptional (Parser.getChompedString (Parser.chompUntil "://") |. Parser.symbol "://")
        |= Parser.getChompedString (Parser.chompWhile (\c -> not (List.member c [ ':', '/', '#', '?' ])))
        |= parseOptional
            (Parser.succeed identity
                |. Parser.symbol ":"
                |= Parser.int
            )
        |= parseOptional
            (Parser.succeed identity
                |. Parser.symbol "/"
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> not (List.member c [ '#', '?' ]))
                    )
            )
        |= parseOptional
            (Parser.succeed identity
                |. Parser.symbol "?"
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> not (List.member c [ '#' ]))
                    )
            )
        |= parseOptional
            (Parser.succeed identity
                |. Parser.symbol "?"
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> not (List.member c [ '#' ]))
                    )
            )
        |. Parser.end
        |> Parser.map
            (\parsed ->
                { scheme = parsed.maybeScheme
                , authority = parsed.authority |> String.split "."
                , port_ = parsed.maybePort
                , path = parsed.maybePath |> Maybe.withDefault "" |> String.split "/"
                , query =
                    parsed.maybeQuery
                        |> Maybe.withDefault ""
                        |> String.split "&"
                        |> List.filterMap
                            (\query ->
                                case String.split "=" query of
                                    [ key, value ] ->
                                        Just ( key, value )

                                    _ ->
                                        Nothing
                            )
                , fragment = parsed.maybeFragment
                }
            )


parseOptional : Parser a -> Parser (Maybe a)
parseOptional p =
    Parser.oneOf
        [ p |> Parser.map Just
        , Parser.succeed Nothing
        ]
