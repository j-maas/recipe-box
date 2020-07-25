module StoreTest exposing (suite)

import Expect
import Fuzz
import Store.FilePath as FilePath
import Store.PathComponent as PathComponent
import Store.Store as Store
import Test exposing (..)
import TestUtils exposing (filePathFuzzer)


suite : Test
suite =
    describe "store"
        [ fuzz filePathFuzzer "inserts and reads item" <|
            \filePath ->
                let
                    item =
                        1
                in
                Store.empty
                    |> Store.insert filePath item
                    |> Store.read filePath
                    |> Expect.equal (Just item)
        ]
