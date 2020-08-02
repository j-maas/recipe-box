module Store.SyncedStore exposing (LocalStoreAccess, LocalVersion, RemoteStoreAccess, RemoteVersion, SyncState, SyncStateAccess, SyncedStore, delete, insert, local, read, remote, sync, with)

import Dict
import Store.FilePath as FilePath exposing (FilePath)
import Store.FolderPath exposing (FolderPath)


type SyncedStore local syncState remote item
    = SyncedStore
        { local : local
        , localAccess : LocalStoreAccess local item
        , nextVersion : LocalVersion
        , sync : syncState
        , syncAccess : SyncStateAccess syncState
        , remote : remote
        , remoteAccess : RemoteStoreAccess remote item
        }


type alias LocalStoreAccess store item =
    { set : FilePath -> ( item, LocalVersion ) -> store -> store
    , insertWithRename : FilePath -> ( item, LocalVersion ) -> store -> ( FilePath, store )
    , read : FilePath -> store -> Maybe ( item, LocalVersion )
    , delete : FilePath -> store -> store
    , listAll : FolderPath -> store -> List ( FilePath, ( item, LocalVersion ) )
    }


type alias SyncStateAccess store =
    { set : FilePath -> SyncState -> store -> store
    , read : FilePath -> store -> Maybe SyncState
    , delete : FilePath -> store -> store
    , listAll : FolderPath -> store -> List ( FilePath, SyncState )
    }


type alias RemoteStoreAccess store item =
    { upload : FilePath -> item -> Maybe RemoteVersion -> store -> ( Maybe RemoteVersion, store )
    , download : FilePath -> store -> Maybe ( item, RemoteVersion )
    , delete : FilePath -> Maybe RemoteVersion -> store -> ( Bool, store )
    , listAll : FolderPath -> store -> List ( FilePath, ( item, RemoteVersion ) )
    }


type alias SyncState =
    { localVersion : LocalVersion
    , remoteVersion : RemoteVersion
    }


type alias LocalVersion =
    Int


type alias RemoteVersion =
    String


with :
    { local : ( local, LocalStoreAccess local item )
    , nextVersion : LocalVersion
    , sync : ( syncState, SyncStateAccess syncState )
    , remote : ( remote, RemoteStoreAccess remote item )
    }
    -> SyncedStore local syncState remote item
with stores =
    let
        ( localStore, localAccess ) =
            stores.local

        ( syncStore, syncAccess ) =
            stores.sync

        ( remoteStore, remoteAccess ) =
            stores.remote
    in
    SyncedStore
        { local = localStore
        , localAccess = localAccess
        , nextVersion = stores.nextVersion
        , sync = syncStore
        , syncAccess = syncAccess
        , remote = remoteStore
        , remoteAccess = remoteAccess
        }


local : SyncedStore local syncState remote item -> local
local (SyncedStore stores) =
    stores.local


syncState : SyncedStore local syncState remote item -> syncState
syncState (SyncedStore stores) =
    stores.sync


remote : SyncedStore local syncState remote item -> remote
remote (SyncedStore stores) =
    stores.remote


insert : FilePath -> item -> SyncedStore local syncState remote item -> SyncedStore local syncState remote item
insert path item (SyncedStore stores) =
    let
        localVersion =
            stores.nextVersion

        ( maybeRemoteVersion, newRemote ) =
            stores.remoteAccess.upload path item Nothing stores.remote

        newStore =
            case maybeRemoteVersion of
                Just remoteVersion ->
                    SyncedStore
                        { stores
                            | local = stores.localAccess.set path ( item, localVersion ) stores.local
                            , sync =
                                stores.syncAccess.set
                                    path
                                    { localVersion = localVersion, remoteVersion = remoteVersion }
                                    stores.sync
                            , remote = newRemote
                            , nextVersion = stores.nextVersion + 1
                        }

                Nothing ->
                    syncFile path (SyncedStore stores)
    in
    newStore


read : FilePath -> SyncedStore local syncState remote item -> Maybe item
read path (SyncedStore stores) =
    stores.localAccess.read path stores.local
        |> Maybe.map Tuple.first


delete : FilePath -> SyncedStore local syncState remote item -> SyncedStore local syncState remote item
delete path (SyncedStore stores) =
    let
        remoteVersion =
            stores.syncAccess.read path stores.sync
                |> Maybe.map .remoteVersion

        ( success, newRemote ) =
            stores.remoteAccess.delete path remoteVersion stores.remote

        newStore =
            if success then
                SyncedStore
                    { stores
                        | local = stores.localAccess.delete path stores.local
                        , sync =
                            stores.syncAccess.delete
                                path
                                stores.sync
                        , remote = newRemote
                    }

            else
                syncFile path (SyncedStore stores)
    in
    newStore


syncFile : FilePath -> SyncedStore local syncState remote item -> SyncedStore local syncState remote item
syncFile path (SyncedStore stores) =
    let

        maybeRemoteEntry =
            stores.remoteAccess.download path stores.remote

        maybeLocalEntry =
            stores.localAccess.read path stores.local

        maybeSyncEntry =
            stores.syncAccess.read path stores.sync
    in
    case ( maybeRemoteEntry, maybeLocalEntry, maybeSyncEntry ) of
        ( Nothing, Nothing, maybeSyncState ) ->
            case maybeSyncState of
                Just _ ->
                    resolveConflict path
                        { local = Nothing
                        , remote =
                            Nothing
                        }
                        (SyncedStore stores)

                Nothing ->
                    SyncedStore stores

        ( Just ( remoteItem, remoteVersion ), Nothing, maybeSyncState ) ->
            case maybeSyncState of
                Just synced ->
                    if remoteVersion == synced.remoteVersion then
                        deleteRemotely path remoteVersion (SyncedStore stores)

                    else
                        resolveConflict path
                            { local = Nothing
                            , remote =
                                Just ( remoteItem, remoteVersion )
                            }
                            (SyncedStore stores)

                Nothing ->
                    insertLocally path remoteItem remoteVersion (SyncedStore stores)

        ( Nothing, Just ( localItem, localVersion ), maybeSyncState ) ->
            case maybeSyncState of
                Just synced ->
                    if localVersion == synced.localVersion then
                        deleteLocally path (SyncedStore stores)

                    else
                        resolveConflict path
                            { local =
                                Just
                                    ( localItem
                                    , localVersion
                                    )
                            , remote = Nothing
                            }
                            (SyncedStore stores)

                Nothing ->
                    insertRemotely path localItem Nothing localVersion (SyncedStore stores)

        ( Just ( remoteItem, remoteVersion ), Just ( localItem, localVersion ), maybeSyncState ) ->
            case maybeSyncState of
                Just synced ->
                    if remoteVersion == synced.remoteVersion && localVersion == synced.localVersion then
                        SyncedStore
                            { stores
                                | sync =
                                    stores.syncAccess.set
                                        path
                                        { localVersion = localVersion
                                        , remoteVersion =
                                            remoteVersion
                                        }
                                        stores.sync
                            }

                    else if localVersion /= synced.localVersion && remoteVersion == synced.remoteVersion then
                        insertRemotely path localItem (Just remoteVersion) localVersion (SyncedStore stores)

                    else if remoteVersion /= synced.remoteVersion && localVersion == synced.localVersion then
                        insertLocally path remoteItem remoteVersion (SyncedStore stores)

                    else
                        resolveConflict path
                            { local =
                                Just
                                    ( localItem
                                    , localVersion
                                    )
                            , remote = Just ( remoteItem, remoteVersion )
                            }
                            (SyncedStore stores)

                Nothing ->
                    if remoteItem == localItem then
                        SyncedStore
                            { stores
                                | sync =
                                    stores.syncAccess.set
                                        path
                                        { localVersion = localVersion
                                        , remoteVersion = remoteVersion
                                        }
                                        stores.sync
                            }

                    else
                        resolveConflict path
                            { local =
                                Just
                                    ( localItem
                                    , localVersion
                                    )
                            , remote = Just ( remoteItem, remoteVersion )
                            }
                            (SyncedStore stores)


insertLocally : FilePath -> item -> RemoteVersion -> SyncedStore local syncState remote item -> SyncedStore local syncState remote item
insertLocally path item remoteVersion (SyncedStore stores) =
    let
        localVersion =
            stores.nextVersion
    in
    SyncedStore
        { stores
            | local = stores.localAccess.set path ( item, localVersion ) stores.local
            , sync =
                stores.syncAccess.set
                    path
                    { localVersion =
                        localVersion
                    , remoteVersion = remoteVersion
                    }
                    stores.sync
            , nextVersion = stores.nextVersion + 1
        }


deleteLocally : FilePath -> SyncedStore local syncState remote item -> SyncedStore local syncState remote item
deleteLocally path (SyncedStore stores) =
    SyncedStore
        { stores
            | local = stores.localAccess.delete path stores.local
            , sync = stores.syncAccess.delete path stores.sync
        }


insertRemotely : FilePath -> item -> Maybe RemoteVersion -> LocalVersion -> SyncedStore local syncState remote item -> SyncedStore local syncState remote item
insertRemotely path item existingRemoteVersion localVersion (SyncedStore stores) =
    let
        ( maybeRemoteVersion, newRemote ) =
            stores.remoteAccess.upload path item existingRemoteVersion stores.remote
    in
    case maybeRemoteVersion of
        Just remoteVersion ->
            SyncedStore
                { stores
                    | remote = newRemote
                    , sync =
                        stores.syncAccess.set
                            path
                            { localVersion =
                                localVersion
                            , remoteVersion = remoteVersion
                            }
                            stores.sync
                }

        Nothing ->
            syncFile path (SyncedStore stores)


deleteRemotely : FilePath -> RemoteVersion -> SyncedStore local syncState remote item -> SyncedStore local syncState remote item
deleteRemotely path remoteVersion (SyncedStore stores) =
    let
        ( success, newRemote ) =
            stores.remoteAccess.delete path (Just remoteVersion) stores.remote
    in
    if success then
        SyncedStore
            { stores
                | remote = newRemote
                , sync = stores.syncAccess.delete path stores.sync
            }

    else
        syncFile path (SyncedStore stores)


resolveConflict : FilePath -> { local : Maybe ( item, LocalVersion ), remote : Maybe ( item, RemoteVersion ) } -> SyncedStore local syncState remote item -> SyncedStore local syncState remote item
resolveConflict path entries (SyncedStore stores) =
    case ( entries.local, entries.remote ) of
        ( Just ( localItem, localVersion ), Just ( remoteItem, remoteVersion ) ) ->
            let
                localVersionForRemoteItem =
                    stores.nextVersion

                ( secondPath, newLocal ) =
                    stores.localAccess.set path ( remoteItem, localVersionForRemoteItem ) stores.local
                        |> stores.localAccess.insertWithRename path ( localItem, localVersion )
            in
            SyncedStore
                { stores
                    | local = newLocal
                    , sync =
                        stores.syncAccess.set path
                            { localVersion = localVersionForRemoteItem
                            , remoteVersion =
                                remoteVersion
                            }
                            stores.sync
                    , nextVersion = stores.nextVersion + 1
                }
                |> insertRemotely secondPath localItem (Just remoteVersion) localVersion

        _ ->
            SyncedStore stores


sync : FolderPath -> SyncedStore local syncState remote item -> SyncedStore local syncState remote item
sync path (SyncedStore stores) =
    let
        { newStore1, localOnly, syncNotInRemote } =
            List.foldl
                (\( nextPath, _ ) current ->
                    let
                        stringPath =
                            FilePath.toString nextPath

                        newLocalOnly =
                            Dict.remove stringPath current.localOnly

                        newSyncNoteInRemote =
                            Dict.remove stringPath current.syncNotInRemote
                    in
                    { newStore1 = syncFile nextPath current.newStore1
                    , localOnly = newLocalOnly
                    , syncNotInRemote = newSyncNoteInRemote
                    }
                )
                { newStore1 = SyncedStore stores
                , localOnly =
                    stores.localAccess.listAll path stores.local
                        |> List.map (\( p, _ ) -> ( FilePath.toString p, p ))
                        |> Dict.fromList
                , syncNotInRemote =
                    stores.syncAccess.listAll path stores.sync
                        |> List.map (\( p, _ ) -> ( FilePath.toString p, p ))
                        |> Dict.fromList
                }
                (stores.remoteAccess.listAll path stores.remote)

        { newStore2, syncOnly } =
            List.foldl
                (\nextPath current ->
                    let
                        stringPath =
                            FilePath.toString nextPath

                        newSyncOnly =
                            Dict.remove stringPath current.syncOnly
                    in
                    { newStore2 = syncFile nextPath current.newStore2
                    , syncOnly = newSyncOnly
                    }
                )
                { newStore2 = newStore1
                , syncOnly = syncNotInRemote
                }
                (Dict.values localOnly)

        newStore3 =
            List.foldl
                (\nextPath currentStores ->
                    syncFile nextPath currentStores
                )
                newStore2
                (Dict.values syncOnly)
    in
    newStore3
