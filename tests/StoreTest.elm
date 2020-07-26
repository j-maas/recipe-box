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
        , fuzz2 (Fuzz.list pathComponentFuzzer) (Fuzz.list Fuzz.int) "inserts all from list and reads them all" <|
            \folder items ->
                let
                    entries : List ( FilePath.FilePath, Int )
                    entries =
                        List.indexedMap
                            (\index item ->
                                let
                                    path =
                                        { folder = folder
                                        , name =
                                            case PathComponent.fromString (String.fromInt index) of
                                                Just n ->
                                                    n

                                                Nothing ->
                                                    Debug.todo "Invalid path component"
                                        }
                                in
                                ( path, item )
                            )
                            items

                    sort =
                        List.sortBy (\( path, _ ) -> FilePath.toString path)
                in
                entries
                    |> Store.insertList Store.empty
                    |> Store.list folder
                    |> sort
                    |> Expect.equalLists (sort entries)
        , fuzz3 filePathFuzzer Fuzz.int Fuzz.int "updates existing item" <|
            \path firstItem secondItem ->
                Store.empty
                    |> Store.insert path firstItem
                    |> Store.update path (Maybe.map ((+) secondItem))
                    |> Store.read path
                    |> Expect.equal (Just <| firstItem + secondItem)
        ]
