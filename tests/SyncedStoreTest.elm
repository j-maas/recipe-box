module SyncedStoreTest exposing (suite)

import Expect
import Fuzz
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
                    |> Expect.all
                        [ \syncedStore ->
                            SyncedStore.read filePath syncedStore
                                |> Expect.equal (Store.read filePath (SyncedStore.local syncedStore))
                        , \syncedStore ->
                            SyncedStore.read filePath syncedStore
                                |> Expect.equal (Store.read filePath (SyncedStore.remote syncedStore))
                        ]
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
