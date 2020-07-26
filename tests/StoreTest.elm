module StoreTest exposing (suite)

import Expect
import Fuzz
import Store.FilePath as FilePath
import Store.Store as Store
import Test exposing (..)
import TestUtils exposing (buildFolderPath, buildPathComponent, filePathFuzzer, pathComponentFuzzer)


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
                                        , name = buildPathComponent (String.fromInt index)
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
        , test "subfolders returns all subfolder with full path" <|
            \_ ->
                let
                    parent =
                        buildFolderPath [ "parent" ]

                    sub1 =
                        buildFolderPath [ "parent", "sub1" ]

                    sub2 =
                        buildFolderPath [ "parent", "sub2" ]

                    sub11 =
                        buildFolderPath [ "parent", "sub1", "sub11" ]

                    sub12 =
                        buildFolderPath [ "parent", "sub1", "sub12" ]

                    folders =
                        [ parent, sub1, sub2, sub11, sub12 ]
                in
                List.map (\folder -> ( { folder = folder, name = buildPathComponent "file" }, 1 )) folders
                    |> Store.insertList Store.empty
                    |> Expect.all
                        [ \store ->
                            Store.subfolders parent store
                                |> Expect.equalLists [ sub1, sub2 ]
                        , \store ->
                            Store.subfolders sub1 store
                                |> Expect.equalLists [ sub11, sub12 ]
                        , \store ->
                            Store.subfolders sub2 store
                                |> Expect.equalLists []
                        , \store ->
                            Store.subfolders sub11 store
                                |> Expect.equalLists []
                        , \store ->
                            Store.subfolders sub12 store
                                |> Expect.equalLists []
                        ]
        ]
