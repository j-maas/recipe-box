module SyncedStoreTest exposing (suite)

import Expect
import Fuzz
import Store.FilePath exposing (FilePath)
import Store.FolderPath exposing (FolderPath)
import Store.Store as Store exposing (Store)
import Store.SyncedStore as SyncedStore exposing (LocalStoreAccess, RemoteStoreAccess, SyncStateAccess, SyncedStore)
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

        {- , fuzz3 filePathFuzzer Fuzz.int Fuzz.int "updates existing item in both stores" <|
           \filePath firstItem secondItem ->
               emptySyncedStore
                   |> SyncedStore.insert filePath firstItem
                   |> SyncedStore.update filePath (Maybe.map ((+) secondItem))
                   |> expectEqualInAllStores filePath (Just <| firstItem + secondItem)
        -}
        , fuzz2 entriesFuzzer entriesFuzzer "sync results in same items in both stores" <|
            \localEntries remoteEntries ->
                let
                    localStore =
                        List.map (\( path, item ) -> ( path, ( item, 0 ) )) localEntries
                            |> Store.insertList Store.empty

                    remoteStore =
                        ( List.length remoteEntries
                        , List.indexedMap (\index ( path, item ) -> ( path, ( item, String.fromInt index ) )) remoteEntries
                            |> Store.insertList Store.empty
                        )
                in
                SyncedStore.with
                    { local = ( localStore, localStoreAccess )
                    , sync = ( Store.empty, syncStoreAccess )
                    , remote = ( remoteStore, remoteStoreAccess )
                    , nextVersion = 0
                    }
                    |> SyncedStore.sync []
                    |> (\syncedStore ->
                            let
                                locals =
                                    SyncedStore.local syncedStore
                                        |> Store.listAll []
                                        |> List.map (\( path, ( item, _ ) ) -> ( path, item ))
                                        |> sortEntries

                                remotes =
                                    SyncedStore.remote syncedStore
                                        |> Tuple.second
                                        |> Store.listAll []
                                        |> List.map (\( path, ( item, _ ) ) -> ( path, item ))
                                        |> sortEntries
                            in
                            locals
                                |> Expect.equalLists remotes
                       )
        ]


emptySyncedStore : SyncedStore LocalStore SyncStore RemoteStore Int
emptySyncedStore =
    SyncedStore.with
        { local = ( Store.empty, localStoreAccess )
        , sync = ( Store.empty, syncStoreAccess )
        , nextVersion = 0
        , remote = ( ( 0, Store.empty ), remoteStoreAccess )
        }


type alias LocalStore =
    Store ( Int, SyncedStore.LocalVersion )


localStoreAccess : LocalStoreAccess LocalStore Int
localStoreAccess =
    { set = Store.insert
    , insertWithRename = Store.insertWithRename
    , read = Store.read
    , delete = Store.delete
    , listAll = Store.listAll
    }


type alias SyncStore =
    Store SyncedStore.SyncState


syncStoreAccess : SyncStateAccess SyncStore
syncStoreAccess =
    { set = Store.insert
    , read = Store.read
    , delete = Store.delete
    , listAll = Store.listAll
    }


type alias RemoteStore =
    ( Int, Store ( Int, SyncedStore.RemoteVersion ) )


remoteStoreAccess : RemoteStoreAccess RemoteStore Int
remoteStoreAccess =
    { upload =
        \path item version ( count, store ) ->
            let
                maybeExistingVersion =
                    Store.read path store
                        |> Maybe.map Tuple.second
            in
            if maybeExistingVersion == version then
                let
                    newVersion =
                        String.fromInt count

                    newStore =
                        ( count + 1, Store.insert path ( item, newVersion ) store )
                in
                ( Just newVersion, newStore )

            else
                ( Nothing, ( count, store ) )
    , download = \path ( _, store ) -> Store.read path store
    , delete =
        \path version ( count, store ) ->
            let
                success =
                    (Store.read path store
                        |> Maybe.map Tuple.second
                    )
                        == version

                newStore =
                    if success then
                        Store.delete path store

                    else
                        store
            in
            ( success, ( count, newStore ) )
    , listAll =
        \folder ( _, store ) ->
            Store.listAll folder store
    }



-- Expectation helpers


expectEqualInAllStores : FilePath -> Maybe Int -> SyncedStore LocalStore SyncStore RemoteStore Int -> Expect.Expectation
expectEqualInAllStores path expected syncedStore =
    let
        local =
            let
                actual =
                    Store.read path (SyncedStore.local syncedStore)
                        |> Maybe.map (\( item, _ ) -> item)
            in
            if actual == expected then
                Nothing

            else
                Just ("Local store failed:\n\n" ++ reportInequality actual expected)

        remote =
            let
                actual =
                    SyncedStore.remote syncedStore
                        |> Tuple.second
                        |> Store.read path
                        |> Maybe.map (\( item, _ ) -> item)
            in
            if actual == expected then
                Nothing

            else
                Just ("Remote store failed:\n\n" ++ reportInequality actual expected)

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
