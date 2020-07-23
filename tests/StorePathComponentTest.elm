module StorePathComponentTest exposing (suite)

import Dict
import Expect
import Fuzz
import Store.PathComponent as PathComponent
import Test exposing (..)


charsetFuzzer : List Char -> Fuzz.Fuzzer String
charsetFuzzer chars =
    Fuzz.list (Fuzz.oneOf (List.map Fuzz.constant chars))
        |> Fuzz.map String.fromList


validChars : List Char
validChars =
    [ 'A', 'B', 'C', 'X', 'Y', 'Z', 'a', 'b', 'c', 'x', 'y', 'z', ' ', '-', '(', ')', '_' ]


suite : Test
suite =
    describe "path component"
        [ fuzz (charsetFuzzer validChars) "allows valid characters" <|
            \name ->
                PathComponent.autorename name (always False)
                    |> PathComponent.toString
                    |> Expect.equal name
        , fuzz (Fuzz.intRange 1 10) "counts on conflict" <|
            \conflicts ->
                let
                    db =
                        (( "conflict", () )
                            :: (List.range 1 conflicts
                                    |> List.map (\num -> ( "conflict" ++ String.fromInt num, () ))
                               )
                        )
                            |> Dict.fromList
                in
                PathComponent.autorename "conflict" (\name -> Dict.member (PathComponent.toString name) db)
                    |> PathComponent.toString
                    |> Expect.equal ("conflict" ++ String.fromInt (conflicts + 1))
        , test "removes special characters" <|
            \_ ->
                PathComponent.autorename "remove. these/" (always False)
                    |> PathComponent.toString
                    |> Expect.equal "remove these"
        , test "replaces Umlaute" <|
            \_ ->
                PathComponent.autorename "ÄäÖöÜüß" (always False)
                    |> PathComponent.toString
                    |> Expect.equal "AeaeOeoeUeuess"
        , test "removes accents" <|
            \_ ->
                PathComponent.autorename "éèàçã" (always False)
                    |> PathComponent.toString
                    |> Expect.equal "eeaca"
        ]
