module StorePathComponentTest exposing (suite)

import Dict
import Expect
import Fuzz
import Store.PathComponent as PathComponent exposing (NameCollision(..), nameCollisionFromMember)
import Test exposing (..)
import TestUtils exposing (charsetFuzzer, pathComponentFuzzer, safePathChars)


suite : Test
suite =
    describe "path component"
        [ fuzz pathComponentFuzzer "converts to string and from string without change" <|
            \name ->
                PathComponent.toString name
                    |> PathComponent.fromString
                    |> Expect.equal (Just name)
        , fuzz (charsetFuzzer safePathChars) "allows valid characters" <|
            \name ->
                PathComponent.autorename name (always NewName)
                    |> PathComponent.toString
                    |> Expect.equal name
        , test "disallows empty strings" <|
            \_ ->
                PathComponent.fromString ""
                    |> Expect.equal Nothing
        , fuzz (Fuzz.intRange 1 10) "counts on conflict" <|
            \conflicts ->
                let
                    db =
                        (( "conflict", () )
                            :: (List.range 1 conflicts
                                    |> List.map (\num -> ( "conflict-" ++ String.fromInt num, () ))
                               )
                        )
                            |> Dict.fromList
                in
                PathComponent.autorename "conflict" (\name -> Dict.member (PathComponent.toString name) db |> nameCollisionFromMember)
                    |> PathComponent.toString
                    |> Expect.equal ("conflict-" ++ String.fromInt (conflicts + 1))
        , test "removes special characters" <|
            \_ ->
                PathComponent.autorename "remove/ these!" (always NewName)
                    |> PathComponent.toString
                    |> Expect.equal "remove these"
        , test "replaces Umlaute" <|
            \_ ->
                PathComponent.autorename "ÄäÖöÜüß" (always NewName)
                    |> PathComponent.toString
                    |> Expect.equal "AeaeOeoeUeuess"
        , test "removes accents" <|
            \_ ->
                PathComponent.autorename "éèàçã" (always NewName)
                    |> PathComponent.toString
                    |> Expect.equal "eeaca"
        ]
