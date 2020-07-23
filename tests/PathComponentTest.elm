module PathComponentTest exposing (suite)

import Dict
import Expect
import Store.PathComponent as PathComponent
import Test exposing (..)


suite : Test
suite =
    describe "unique name"
        [ test "allows valid characters" <|
            \_ ->
                PathComponent.autorename "this_is a (valid) item-name" (always False)
                    |> PathComponent.toString
                    |> Expect.equal "this_is a (valid) item-name"
        , test "counts on conflict" <|
            \_ ->
                let
                    db =
                        Dict.empty
                            |> Dict.insert "conflict" ()
                            |> Dict.insert "conflict1" ()
                in
                PathComponent.autorename "conflict" (\name -> Dict.member (PathComponent.toString name) db)
                    |> PathComponent.toString
                    |> Expect.equal "conflict2"
        , test "removes special characters" <|
            \_ ->
                PathComponent.autorename "./" (always False)
                    |> PathComponent.toString
                    |> Expect.equal ""
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
