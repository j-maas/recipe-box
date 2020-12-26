module SyncedStoreTest exposing (suite)

import Expect
import Fuzz
import Store.FilePath exposing (FilePath)
import Store.Store as Store exposing (Store)
import Store.SyncedStore as SyncedStore exposing (LocalStoreAccess, RemoteStoreAccess, SyncStateAccess, SyncedStore)
import Store.VersionStore as VersionStore exposing (VersionStore)
import Test exposing (..)
import TestUtils exposing (buildFolderPath, buildPathComponent, entriesFuzzer, filePathFuzzer, sortEntries)


suite : Test
suite =
    describe "synced store"
        [ fuzz2 filePathFuzzer Fuzz.int "inserts into both stores" <|
            \filePath item ->
                emptySyncedStore
                    |> SyncedStore.insert filePath item
                    |> Tuple.first
                    |> expectEqualInAllStores filePath (Just item)
        , test "syncs locally item added to remote" <|
            \_ ->
                let
                    filePath =
                        { folder = buildFolderPath [], name = buildPathComponent "added" }

                    item =
                        1

                    localStore =
                        VersionStore.insertList
                            VersionStore.empty
                            [ ( filePath, item ) ]

                    remoteStore =
                        VersionStore.empty
                in
                SyncedStore.with
                    { local = ( localStore, localStoreAccess )
                    , sync = ( Store.empty, syncStoreAccess )
                    , remote = ( remoteStore, remoteStoreAccess )
                    }
                    |> SyncedStore.sync []
                    |> Tuple.first
                    |> expectEqualInAllStores filePath (Just item)
        , test "resolves conflict" <|
            \_ ->
                let
                    folder =
                        buildFolderPath []

                    firstFilePath =
                        { folder = folder
                        , name = buildPathComponent "added"
                        }

                    secondFilePath =
                        { folder = folder
                        , name = buildPathComponent "added-1"
                        }

                    localItem =
                        1

                    remoteItem =
                        2

                    localStore =
                        VersionStore.insertList VersionStore.empty
                            [ ( firstFilePath, localItem ) ]

                    remoteStore =
                        VersionStore.insertList VersionStore.empty
                            [ ( firstFilePath, remoteItem ) ]
                in
                SyncedStore.with
                    { local = ( localStore, localStoreAccess )
                    , sync = ( Store.empty, syncStoreAccess )
                    , remote = ( remoteStore, remoteStoreAccess )
                    }
                    |> SyncedStore.sync []
                    |> Tuple.first
                    |> Expect.all
                        [ expectEqualInAllStores firstFilePath (Just remoteItem)
                        , expectEqualInAllStores secondFilePath (Just localItem)
                        ]
        , fuzz2 entriesFuzzer entriesFuzzer "sync results in same items in both stores" <|
            \localEntries remoteEntries ->
                let
                    localStore =
                        VersionStore.insertList VersionStore.empty localEntries

                    remoteStore =
                        VersionStore.insertList VersionStore.empty remoteEntries
                in
                SyncedStore.with
                    { local = ( localStore, localStoreAccess )
                    , sync = ( Store.empty, syncStoreAccess )
                    , remote = ( remoteStore, remoteStoreAccess )
                    }
                    |> SyncedStore.sync []
                    |> Tuple.first
                    |> (\syncedStore ->
                            let
                                locals =
                                    SyncedStore.local syncedStore
                                        |> VersionStore.listAll []
                                        |> List.map (\( path, ( item, _ ) ) -> ( path, item ))
                                        |> sortEntries

                                remotes =
                                    SyncedStore.remote syncedStore
                                        |> VersionStore.listAll []
                                        |> List.map (\( path, ( item, _ ) ) -> ( path, item ))
                                        |> sortEntries
                            in
                            locals
                                |> Expect.equalLists remotes
                       )
        ]


emptySyncedStore : SyncedStore LocalStore SyncStore RemoteStore Int ()
emptySyncedStore =
    SyncedStore.with
        { local = ( VersionStore.empty, localStoreAccess )
        , sync = ( Store.empty, syncStoreAccess )
        , remote = ( VersionStore.empty, remoteStoreAccess )
        }


type alias LocalStore =
    VersionStore Int


localStoreAccess : LocalStoreAccess LocalStore Int
localStoreAccess =
    { set = VersionStore.insert
    , insertWithRename = VersionStore.insertWithRename
    , read = VersionStore.read
    , delete = VersionStore.delete
    , listAll = VersionStore.listAll
    }


type alias SyncStore =
    Store SyncedStore.SyncState


syncStoreAccess : SyncStateAccess SyncStore
syncStoreAccess =
    { set = Store.insert
    , read = Store.read
    , update = Store.update
    , delete = Store.delete
    , listAll = Store.listAll
    }


type alias RemoteStore =
    VersionStore Int


remoteStoreAccess : RemoteStoreAccess RemoteStore Int ()
remoteStoreAccess =
    { upload =
        \path item maybeSyncedVersion store ->
            let
                maybeExistingVersion =
                    VersionStore.read path store
                        |> Maybe.map Tuple.second

                uploaded =
                    let
                        ( _, newStore ) =
                            VersionStore.insert path item store
                    in
                    ( newStore, Cmd.none )
            in
            case ( maybeExistingVersion, maybeSyncedVersion ) of
                ( Just existingVersion, Just syncedVersion ) ->
                    if existingVersion /= syncedVersion then
                        uploaded

                    else
                        ( store, Cmd.none )

                _ ->
                    uploaded
    , download = \path store -> VersionStore.read path store
    , delete =
        \path version store ->
            let
                success =
                    (VersionStore.read path store
                        |> Maybe.map Tuple.second
                    )
                        == version

                newStore =
                    if success then
                        VersionStore.delete path store

                    else
                        store
            in
            ( success, newStore )
    , listAll =
        \folder store ->
            VersionStore.listAll folder store
    }



-- Expectation helpers


expectEqualInAllStores : FilePath -> Maybe Int -> SyncedStore LocalStore SyncStore RemoteStore Int () -> Expect.Expectation
expectEqualInAllStores path expected syncedStore =
    let
        local =
            let
                actual =
                    VersionStore.read path (SyncedStore.local syncedStore)
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
                        |> VersionStore.read path
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
