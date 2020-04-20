module FileNameTest exposing (suite)

import Db
import Expect
import FileName
import Id
import Test exposing (..)


suite : Test
suite =
    describe "unique id"
        [ test "allows valid characters" <|
            \_ ->
                FileName.autorename "this_is a (valid) file-name" Db.empty
                    |> Id.toString
                    |> Expect.equal "this_is a (valid) file-name"
        , test "counts on conflict" <|
            \_ ->
                let
                    db =
                        Db.empty
                            |> Db.insert ( Id.fromString "conflict", () )
                            |> Db.insert ( Id.fromString "conflict1", () )
                in
                FileName.autorename "conflict" db
                    |> Id.toString
                    |> Expect.equal "conflict2"
        , test "removes special characters" <|
            \_ ->
                FileName.autorename "./" Db.empty
                    |> Id.toString
                    |> Expect.equal ""
        , test "replaces Umlaute" <|
            \_ ->
                FileName.autorename "ÄäÖöÜüß" Db.empty
                    |> Id.toString
                    |> Expect.equal "AeaeOeoeUeuess"
        , test "removes accents" <|
            \_ ->
                FileName.autorename "éèàçã" Db.empty
                    |> Id.toString
                    |> Expect.equal "eeaca"
        ]
