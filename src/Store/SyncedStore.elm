module Store.SyncedStore exposing (LocalStoreAccess, RemoteStoreAccess, SyncState, SyncStateAccess, SyncedStore, Version, delete, insert, local, read, remote, setLocalVersion, setRemoteVersion, sync, with)

import Dict
import Store.FilePath as FilePath exposing (FilePath)
import Store.FolderPath exposing (FolderPath)


type SyncedStore local syncState remote item msg
    = SyncedStore
        { local : local
        , localAccess : LocalStoreAccess local item
        , sync : syncState
        , syncAccess : SyncStateAccess syncState
        , remote : remote
        , remoteAccess : RemoteStoreAccess remote item msg
        }


type alias LocalStoreAccess store item =
    { set : FilePath -> item -> store -> ( Version, store )
    , insertWithRename : FilePath -> item -> store -> ( FilePath, Version, store )
    , read : FilePath -> store -> Maybe ( item, Version )
    , delete : FilePath -> store -> store
    , listAll : FolderPath -> store -> List ( FilePath, ( item, Version ) )
    }


type alias SyncStateAccess store =
    { set : FilePath -> SyncState -> store -> store
    , update : FilePath -> (Maybe SyncState -> Maybe SyncState) -> store -> store
    , read : FilePath -> store -> Maybe SyncState
    , delete : FilePath -> store -> store
    , listAll : FolderPath -> store -> List ( FilePath, SyncState )
    }


type alias RemoteStoreAccess store item msg =
    { upload : FilePath -> item -> Maybe Version -> store -> ( store, Cmd msg )
    , download : FilePath -> store -> Maybe ( item, Version )
    , delete : FilePath -> Maybe Version -> store -> ( Bool, store )
    , listAll : FolderPath -> store -> List ( FilePath, ( item, Version ) )
    }


type SyncState
    = Synced SyncedVersions
    | Local Version
    | Remote Version


setLocalSync : Version -> SyncState -> SyncState
setLocalSync localVersion state =
    case state of
        Synced version ->
            Synced { version | local = localVersion }

        Local _ ->
            Local localVersion

        Remote remoteVersion ->
            Synced { local = localVersion, remote = remoteVersion }


setRemoteSync : Version -> SyncState -> SyncState
setRemoteSync remoteVersion state =
    case state of
        Synced version ->
            Synced { version | remote = remoteVersion }

        Local localVersion ->
            Synced { local = localVersion, remote = remoteVersion }

        Remote _ ->
            Remote remoteVersion


getSynced : SyncState -> Maybe SyncedVersions
getSynced state =
    case state of
        Synced version ->
            Just version

        _ ->
            Nothing


type alias SyncedVersions =
    { local : Version
    , remote : Version
    }


type alias Version =
    String


with :
    { local : ( local, LocalStoreAccess local item )
    , sync : ( syncState, SyncStateAccess syncState )
    , remote : ( remote, RemoteStoreAccess remote item msg )
    }
    -> SyncedStore local syncState remote item msg
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
        , sync = syncStore
        , syncAccess = syncAccess
        , remote = remoteStore
        , remoteAccess = remoteAccess
        }


local : SyncedStore local syncState remote item msg -> local
local (SyncedStore stores) =
    stores.local


syncState : SyncedStore local syncState remote item msg -> syncState
syncState (SyncedStore stores) =
    stores.sync


remote : SyncedStore local syncState remote item msg -> remote
remote (SyncedStore stores) =
    stores.remote


insert : FilePath -> item -> SyncedStore local syncState remote item msg -> ( SyncedStore local syncState remote item msg, Cmd msg )
insert path item (SyncedStore stores) =
    let
        ( localVersion, newLocal ) =
            stores.localAccess.set path item stores.local

        ( newRemote, remoteCmd ) =
            stores.remoteAccess.upload path item Nothing stores.remote
    in
    ( SyncedStore
        { stores
            | local = newLocal
            , remote = newRemote
        }
        |> setLocalVersion path localVersion
    , Cmd.batch [ remoteCmd ]
    )


read : FilePath -> SyncedStore local syncState remote item msg -> Maybe item
read path (SyncedStore stores) =
    stores.localAccess.read path stores.local
        |> Maybe.map Tuple.first


delete : FilePath -> SyncedStore local syncState remote item msg -> ( SyncedStore local syncState remote item msg, Cmd msg )
delete path (SyncedStore stores) =
    let
        remoteVersion =
            stores.syncAccess.read path stores.sync
                |> Maybe.andThen getSynced
                |> Maybe.map .remote

        ( success, newRemote ) =
            stores.remoteAccess.delete path remoteVersion stores.remote
    in
    if success then
        ( SyncedStore
            { stores
                | local = stores.localAccess.delete path stores.local
                , sync =
                    stores.syncAccess.delete
                        path
                        stores.sync
                , remote = newRemote
            }
        , Cmd.none
        )

    else
        syncFile path (SyncedStore stores)


syncFile : FilePath -> SyncedStore local syncState remote item msg -> ( SyncedStore local syncState remote item msg, Cmd msg )
syncFile path (SyncedStore stores) =
    let
        maybeRemoteEntry =
            stores.remoteAccess.download path stores.remote

        maybeLocalEntry =
            stores.localAccess.read path stores.local

        maybeSyncEntry =
            stores.syncAccess.read path stores.sync
                |> Maybe.andThen getSynced
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
                    ( SyncedStore stores, Cmd.none )

        ( Just ( remoteItem, remoteVersion ), Nothing, maybeSyncState ) ->
            case maybeSyncState of
                Just synced ->
                    if remoteVersion == synced.remote then
                        deleteRemotely path remoteVersion (SyncedStore stores)

                    else
                        resolveConflict path
                            { local = Nothing
                            , remote =
                                Just ( remoteItem, remoteVersion )
                            }
                            (SyncedStore stores)

                Nothing ->
                    ( insertLocally path remoteItem remoteVersion (SyncedStore stores), Cmd.none )

        ( Nothing, Just ( localItem, localVersion ), maybeSyncState ) ->
            case maybeSyncState of
                Just synced ->
                    if localVersion == synced.local then
                        ( deleteLocally path (SyncedStore stores), Cmd.none )

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
                    if remoteVersion == synced.remote && localVersion == synced.local then
                        ( SyncedStore
                            { stores
                                | sync =
                                    stores.syncAccess.set
                                        path
                                        (Synced
                                            { local = localVersion
                                            , remote = remoteVersion
                                            }
                                        )
                                        stores.sync
                            }
                        , Cmd.none
                        )

                    else if localVersion /= synced.local && remoteVersion == synced.remote then
                        insertRemotely path localItem (Just remoteVersion) localVersion (SyncedStore stores)

                    else if remoteVersion /= synced.remote && localVersion == synced.local then
                        ( insertLocally path remoteItem remoteVersion (SyncedStore stores), Cmd.none )

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
                        ( SyncedStore
                            { stores
                                | sync =
                                    stores.syncAccess.set
                                        path
                                        (Synced
                                            { local = localVersion
                                            , remote = remoteVersion
                                            }
                                        )
                                        stores.sync
                            }
                        , Cmd.none
                        )

                    else
                        resolveConflict
                            path
                            { local =
                                Just
                                    ( localItem
                                    , localVersion
                                    )
                            , remote = Just ( remoteItem, remoteVersion )
                            }
                            (SyncedStore stores)


insertLocally : FilePath -> item -> Version -> SyncedStore local syncState remote item msg -> SyncedStore local syncState remote item msg
insertLocally path item remoteVersion (SyncedStore stores) =
    let
        ( localVersion, newLocal ) =
            stores.localAccess.set path item stores.local
    in
    SyncedStore
        { stores
            | local = newLocal
            , sync =
                stores.syncAccess.set
                    path
                    (Synced
                        { local = localVersion
                        , remote = remoteVersion
                        }
                    )
                    stores.sync
        }


deleteLocally : FilePath -> SyncedStore local syncState remote item msg -> SyncedStore local syncState remote item msg
deleteLocally path (SyncedStore stores) =
    SyncedStore
        { stores
            | local = stores.localAccess.delete path stores.local
            , sync = stores.syncAccess.delete path stores.sync
        }


insertRemotely : FilePath -> item -> Maybe Version -> Version -> SyncedStore local syncState remote item msg -> ( SyncedStore local syncState remote item msg, Cmd msg )
insertRemotely path item existingRemoteVersion localVersion (SyncedStore stores) =
    let
        ( newRemote, remoteCmd ) =
            stores.remoteAccess.upload path item existingRemoteVersion stores.remote
    in
    ( SyncedStore
        { stores
            | remote = newRemote
        }
    , remoteCmd
    )


deleteRemotely : FilePath -> Version -> SyncedStore local syncState remote item msg -> ( SyncedStore local syncState remote item msg, Cmd msg )
deleteRemotely path remoteVersion (SyncedStore stores) =
    let
        ( success, newRemote ) =
            stores.remoteAccess.delete path (Just remoteVersion) stores.remote
    in
    if success then
        ( SyncedStore
            { stores
                | remote = newRemote
                , sync = stores.syncAccess.delete path stores.sync
            }
        , Cmd.none
        )

    else
        syncFile path (SyncedStore stores)


resolveConflict : FilePath -> { local : Maybe ( item, Version ), remote : Maybe ( item, Version ) } -> SyncedStore local syncState remote item msg -> ( SyncedStore local syncState remote item msg, Cmd msg )
resolveConflict path entries (SyncedStore stores) =
    case ( entries.local, entries.remote ) of
        ( Just ( localItem, _ ), Just ( remoteItem, remoteVersion ) ) ->
            let
                ( localVersionForRemoteItem, newLocal1 ) =
                    stores.localAccess.set path remoteItem stores.local

                ( secondPath, localVersion, newLocal2 ) =
                    stores.localAccess.insertWithRename path localItem newLocal1
            in
            SyncedStore
                { stores
                    | local = newLocal2
                    , sync =
                        stores.syncAccess.set path
                            (Synced
                                { local = localVersionForRemoteItem
                                , remote = remoteVersion
                                }
                            )
                            stores.sync
                }
                |> insertRemotely secondPath localItem (Just remoteVersion) localVersion

        _ ->
            ( SyncedStore stores, Cmd.none )


sync : FolderPath -> SyncedStore local syncState remote item msg -> ( SyncedStore local syncState remote item msg, Cmd msg )
sync path (SyncedStore stores) =
    let
        { newStore1, localOnly, syncNotInRemote, cmds1 } =
            List.foldl
                (\( nextPath, _ ) current ->
                    let
                        stringPath =
                            FilePath.toString nextPath

                        newLocalOnly =
                            Dict.remove stringPath current.localOnly

                        newSyncNotInRemote =
                            Dict.remove stringPath current.syncNotInRemote

                        ( newStore, cmd ) =
                            syncFile nextPath current.newStore1
                    in
                    { newStore1 = newStore
                    , localOnly = newLocalOnly
                    , syncNotInRemote = newSyncNotInRemote
                    , cmds1 = cmd :: current.cmds1
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
                , cmds1 = []
                }
                (stores.remoteAccess.listAll path stores.remote)

        { newStore2, syncOnly, cmds2 } =
            List.foldl
                (\nextPath current ->
                    let
                        stringPath =
                            FilePath.toString nextPath

                        newSyncOnly =
                            Dict.remove stringPath current.syncOnly

                        ( newStore, cmd ) =
                            syncFile nextPath current.newStore2
                    in
                    { newStore2 = newStore
                    , syncOnly = newSyncOnly
                    , cmds2 = cmd :: current.cmds2
                    }
                )
                { newStore2 = newStore1
                , syncOnly = syncNotInRemote
                , cmds2 = []
                }
                (Dict.values localOnly)

        { newStore3, cmds3 } =
            List.foldl
                (\nextPath current ->
                    let
                        ( newStore, cmd ) =
                            syncFile nextPath current.newStore3
                    in
                    { newStore3 = newStore
                    , cmds3 = cmd :: current.cmds3
                    }
                )
                { newStore3 = newStore2
                , cmds3 = []
                }
                (Dict.values syncOnly)

        allCmds =
            Cmd.batch (cmds1 ++ cmds2 ++ cmds3)
    in
    ( newStore3, allCmds )


setLocalVersion : FilePath -> Version -> SyncedStore local syncState remote item msg -> SyncedStore local syncState remote item msg
setLocalVersion path version (SyncedStore stores) =
    SyncedStore
        { stores
            | sync =
                stores.syncAccess.update path
                    (Maybe.map (setLocalSync version))
                    stores.sync
        }


setRemoteVersion : FilePath -> Version -> SyncedStore local syncState remote item msg -> SyncedStore local syncState remote item msg
setRemoteVersion path version (SyncedStore stores) =
    SyncedStore
        { stores
            | sync =
                stores.syncAccess.update path
                    (Maybe.map (setRemoteSync version))
                    stores.sync
        }
