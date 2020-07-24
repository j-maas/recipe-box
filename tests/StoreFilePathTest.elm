module StoreFilePathTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Store.FilePath as Path exposing (FilePath)
import Test exposing (..)
import TestUtils exposing (pathComponentFuzzer)


suite : Test
suite =
    describe "path"
        [ test "converts to file-inspired string representation" <|
            \_ ->
                Path.from
                    { folder = [ "i", "am", "a" ]
                    , name = "file"
                    , extension = ( "with", [ "extension" ] )
                    }
                    |> Maybe.map Path.toString
                    |> Expect.equal (Just "i/am/a/file.with.extension")
        , fuzz pathFuzzer "converts to and from string without change" <|
            \path ->
                Path.toString path
                    |> Path.fromString
                    |> Expect.equal (Just path)
        ]


pathFuzzer : Fuzzer FilePath
pathFuzzer =
    Fuzz.map4
        (\folder name extension extensionRest ->
            { folder = folder
            , name = name
            , extension = ( extension, extensionRest )
            }
        )
        (Fuzz.list pathComponentFuzzer)
        pathComponentFuzzer
        pathComponentFuzzer
        (Fuzz.list pathComponentFuzzer)
