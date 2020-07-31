module SyncedStoreTest exposing (suite)

import Expect
import Fuzz
import Store.FilePath exposing (FilePath)
import Store.Store as Store exposing (Store)
import Store.SyncedStore as SyncedStore exposing (StoreAccess, SyncedStore)
import Test exposing (..)
import TestUtils exposing (filePathFuzzer)


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
        ]


emptySyncedStore : SyncedStore (Store item) (Store item) item
emptySyncedStore =
    SyncedStore.with { local = ( Store.empty, storeAccess ), remote = ( Store.empty, storeAccess ) }


storeAccess : StoreAccess (Store item) item
storeAccess =
    { insert = Store.insert
    , read = Store.read
    , update = Store.update
    , delete = Store.delete
    , list = Store.list
    }



-- Expectation helpers


expectEqualInAllStores : FilePath -> Maybe item -> SyncedStore (Store item) (Store item) item -> Expect.Expectation
expectEqualInAllStores path expected syncedStore =
    let
        synced =
            let
                actual =
                    SyncedStore.read path syncedStore
            in
            if actual == expected then
                Nothing

            else
                Just (reportStoreInequality "SyncedStore" actual expected)

        local =
            let
                actual =
                    SyncedStore.local syncedStore
                        |> Store.read path
            in
            if actual == expected then
                Nothing

            else
                Just (reportStoreInequality "Local store" actual expected)

        remote =
            let
                actual =
                    SyncedStore.remote syncedStore
                        |> Store.read path
            in
            if actual == expected then
                Nothing

            else
                Just (reportStoreInequality "Remote store" actual expected)

        combined =
            String.join "\n\n" (List.filterMap identity [ synced, local, remote ])
    in
    if String.isEmpty combined then
        Expect.pass

    else
        Expect.fail combined


reportStoreInequality : String -> item -> item -> String
reportStoreInequality store top bottom =
    String.join "\n"
        [ store ++ " returns different item than expected:"
        , reportInequality top bottom
        ]


reportInequality : item -> item -> String
reportInequality top bottom =
    String.join "\n"
        [ "    " ++ Debug.toString top
        , "    ╷"
        , "    │ Expect.equal"
        , "    ╵"
        , "    " ++ Debug.toString bottom
        ]
