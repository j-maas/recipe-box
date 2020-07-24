module StoreFolderPathTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Store.FolderPath as Path exposing (FolderPath)
import Store.PathComponent as PathComponent
import Test exposing (..)
import TestUtils exposing (pathComponentFuzzer)


suite : Test
suite =
    describe "path"
        [ test "converts to folder-inspired string representation" <|
            \_ ->
                Path.fromList
                    [ "i", "am", "a", "folder" ]
                    |> Maybe.map Path.toString
                    |> Expect.equal (Just "i/am/a/folder/")
        , test "allows empty (root) folder" <|
            \_ ->
                Path.fromList []
                    |> Maybe.map Path.toString
                    |> Expect.equal (Just "/")
        , test "fromList interprets [\"\"] as the root folder (happens when String.split is called on \"/\")" <|
            \_ ->
                Path.fromList [ "" ]
                    |> Maybe.map Path.toString
                    |> Expect.equal (Just "/")
        , fuzz pathFuzzer "converts to and from string without change" <|
            \path ->
                Path.toString path
                    |> Path.fromString
                    |> Expect.equal (Just path)
        , fuzz (Fuzz.list pathComponentFuzzer) "fromString handles missing trailing slash" <|
            \components ->
                let
                    stringComponents =
                        List.map PathComponent.toString components
                in
                String.join "/" stringComponents
                    |> Path.fromString
                    |> Maybe.map Path.toString
                    |> Expect.equal (Just <| String.join "/" stringComponents ++ "/")
        ]


pathFuzzer : Fuzzer FolderPath
pathFuzzer =
    Fuzz.list pathComponentFuzzer
