module StoreFilePathTest exposing (suite)

import Expect
import Store.FilePath as Path
import Test exposing (..)
import TestUtils exposing (filePathFuzzer)


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
        , fuzz filePathFuzzer "converts to and from string without change" <|
            \path ->
                Path.toString path
                    |> Path.fromString
                    |> Expect.equal (Just path)
        ]
