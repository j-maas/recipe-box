module StoreTest exposing (suite)

import Expect
import Fuzz
import Store.FilePath as FilePath
import Store.PathComponent as PathComponent
import Store.Store as Store
import Test exposing (..)
import TestUtils exposing (filePathFuzzer, pathComponentFuzzer)


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
        , fuzz2 (Fuzz.list pathComponentFuzzer) (Fuzz.list Fuzz.int) "inserts multiple and reads them all" <|
            \folder items ->
                List.indexedMap (\index item -> ( PathComponent.unsafe <| String.fromInt index, item )) items
                    |> List.foldl
                        (\( name, item ) store ->
                            let
                                path =
                                    { folder = folder, name = name }
                            in
                            Store.insert path item store
                        )
                        Store.empty
                    |> Store.list folder
                    |> List.sort
                    |> Expect.equal (List.sort items)
        ]
