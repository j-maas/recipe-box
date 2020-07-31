module SyncedStoreTest exposing (suite)

import Expect
import Fuzz
import Store.FilePath exposing (FilePath)
import Store.FolderPath exposing (FolderPath)
import Store.Store as Store exposing (Store)
import Store.SyncedStore as SyncedStore exposing (StoreAccess, SyncedStore)
import Test exposing (..)
import TestUtils exposing (entriesFuzzer, filePathFuzzer, sortEntries)


suite : Test
suite =
    describe "synced store"
        [ fuzz2 filePathFuzzer Fuzz.int "inserts into both stores" <|
            \filePath item ->
                emptySyncedStore
                    |> SyncedStore.insert filePath item
                    |> expectEqualInAllStores filePath (Just item)
        , fuzz3 filePathFuzzer Fuzz.int Fuzz.int "updates existing item in both stores" <|
            \filePath firstItem secondItem ->
                emptySyncedStore
                    |> SyncedStore.insert filePath firstItem
                    |> SyncedStore.update filePath (Maybe.map ((+) secondItem))
                    |> expectEqualInAllStores filePath (Just <| firstItem + secondItem)
        , skip <|
            fuzz2 entriesFuzzer entriesFuzzer "sync results in same items in both stores" <|
                \localEntries remoteEntries ->
                    let
                        localStore =
                            Store.insertList Store.empty localEntries

                        remoteStore =
                            Store.insertList Store.empty remoteEntries
                    in
                    SyncedStore.with { local = ( localStore, storeAccess ), remote = ( remoteStore, storeAccess ) }
                        |> SyncedStore.sync []
                        |> (\syncedStore ->
                                let
                                    locals =
                                        SyncedStore.local syncedStore
                                            |> listAll
                                            |> sortEntries

                                    remotes =
                                        SyncedStore.remote syncedStore
                                            |> listAll
                                            |> sortEntries
                                in
                                locals
                                    |> Expect.equalLists remotes
                           )
        ]


emptySyncedStore : SyncedStore (Store item) (Store item) item
emptySyncedStore =
    SyncedStore.with { local = ( Store.empty, storeAccess ), remote = ( Store.empty, storeAccess ) }


storeAccess : StoreAccess (Store item) item
storeAccess =
    { insert = Store.insert
    , insertList = Store.insertList
    , read = Store.read
    , update = Store.update
    , delete = Store.delete
    , listAll = Store.listAll
    }


listAll : Store item -> List ( FilePath, item )
listAll store =
    listAllRec store []


listAllRec : Store item -> FolderPath -> List ( FilePath, item )
listAllRec store path =
    List.concat [ Store.list path store, List.concatMap (listAllRec store) (Store.subfolders path store) ]



-- Expectation helpers


expectEqualInAllStores : FilePath -> Maybe item -> SyncedStore (Store item) (Store item) item -> Expect.Expectation
expectEqualInAllStores path expected syncedStore =
    expectBothStores
        (\store ->
            let
                actual =
                    Store.read path store
            in
            if actual == expected then
                Nothing

            else
                Just (reportInequality actual expected)
        )
        syncedStore


expectBothStores : (Store item -> Maybe String) -> SyncedStore (Store item) (Store item) item -> Expect.Expectation
expectBothStores storeExpectation syncedStore =
    let
        local =
            storeExpectation (SyncedStore.local syncedStore)
                |> Maybe.map (\failure -> "Local store failed:\n\n" ++ failure)

        remote =
            storeExpectation (SyncedStore.remote syncedStore)
                |> Maybe.map (\failure -> "Remote store failed:\n\n" ++ failure)

        combined =
            String.join "\n\n\n" (List.filterMap identity [ local, remote ])
    in
    if String.isEmpty combined then
        Expect.pass

    else
        Expect.fail combined


reportInequality : item -> item -> String
reportInequality top bottom =
    String.join "\n"
        [ "    " ++ Debug.toString top
        , "    ╷"
        , "    │ Expect.equal"
        , "    ╵"
        , "    " ++ Debug.toString bottom
        ]
