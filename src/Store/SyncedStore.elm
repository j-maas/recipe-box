module Store.SyncedStore exposing (LocalStoreAccess, RemoteStoreAccess, SyncTag, SyncedStore, delete, insert, local, read, remote, sync, update, with)

import Dict
import Store.FilePath as FilePath exposing (FilePath)
import Store.FolderPath exposing (FolderPath)
import Store.PathComponent as PathComponent exposing (PathComponent)


type SyncedStore local remote item
    = SyncedStore
        { local : local
        , localAccess : LocalStoreAccess local item
        , remote : remote
        , remoteAccess : RemoteStoreAccess remote item
        }


type alias LocalStoreAccess store item =
    { insert : FilePath -> item -> Maybe SyncTag -> store -> store
    , insertWithRename : FilePath -> item -> Maybe SyncTag -> store -> ( FilePath, store )
    , insertList : store -> List ( FilePath, item, Maybe SyncTag ) -> store
    , read : FilePath -> store -> Maybe ( item, Maybe SyncTag )
    , update : FilePath -> (Maybe ( item, Maybe SyncTag ) -> Maybe ( item, Maybe SyncTag )) -> store -> store
    , delete : FilePath -> store -> store
    , listAll : FolderPath -> store -> List ( FilePath, item, Maybe SyncTag )
    }


type alias RemoteStoreAccess store item =
    { insert : FilePath -> item -> store -> store
    , insertList : store -> List ( FilePath, item ) -> store
    , read : FilePath -> store -> Maybe ( item, Version )
    , update : FilePath -> (Maybe item -> Maybe item) -> store -> store
    , delete : FilePath -> store -> store
    , listAll : FolderPath -> store -> List ( FilePath, item, Version )
    }


type SyncTag
    = Synced Version
    | Changed Version


versionFromSyncTag : SyncTag -> Version
versionFromSyncTag syncTag =
    case syncTag of
        Synced version ->
            version

        Changed version ->
            version


touchSyncTag : Maybe SyncTag -> Maybe SyncTag
touchSyncTag syncTag =
    Maybe.map (versionFromSyncTag >> Changed) syncTag


type alias Version =
    String


with :
    { local : ( local, LocalStoreAccess local item )
    , remote : ( remote, RemoteStoreAccess remote item )
    }
    -> SyncedStore local remote item
with stores =
    let
        ( localStore, localAccess ) =
            stores.local

        ( remoteStore, remoteAccess ) =
            stores.remote
    in
    SyncedStore
        { local = localStore
        , localAccess = localAccess
        , remote = remoteStore
        , remoteAccess = remoteAccess
        }


local : SyncedStore local remote item -> local
local (SyncedStore stores) =
    stores.local


remote : SyncedStore local remote item -> remote
remote (SyncedStore stores) =
    stores.remote


insert : FilePath -> item -> SyncedStore local remote item -> SyncedStore local remote item
insert path item (SyncedStore stores) =
    SyncedStore
        { stores
          -- TODO: Check if we need to update and set SyncTag to Changed
            | local = stores.localAccess.insert path item Nothing stores.local
            , remote = stores.remoteAccess.insert path item stores.remote
        }


read : FilePath -> SyncedStore local remote item -> Maybe item
read path (SyncedStore stores) =
    stores.localAccess.read path stores.local
        |> Maybe.map (\( item, _ ) -> item)


update : FilePath -> (Maybe item -> Maybe item) -> SyncedStore local remote item -> SyncedStore local remote item
update path f (SyncedStore stores) =
    let
        updateLocal entry =
            case entry of
                Just ( item, syncTag ) ->
                    f (Just item) |> Maybe.map (\i -> ( i, touchSyncTag syncTag ))

                Nothing ->
                    f Nothing |> Maybe.map (\i -> ( i, Nothing ))
    in
    SyncedStore
        { stores
            | local = stores.localAccess.update path updateLocal stores.local
            , remote = stores.remoteAccess.update path f stores.remote
        }


delete : FilePath -> SyncedStore local remote item -> SyncedStore local remote item
delete path (SyncedStore stores) =
    SyncedStore
        { stores
            | local = stores.localAccess.delete path stores.local
            , remote = stores.remoteAccess.delete path stores.remote
        }


sync : FolderPath -> SyncedStore local remote item -> SyncedStore local remote item
sync path (SyncedStore stores) =
    let
        { newLocal, newRemote, localOnly, conflicts } =
            List.foldl
                (\( nextPath, nextItem, nextVersion ) accu ->
                    let
                        stringPath =
                            FilePath.toString nextPath
                    in
                    case Dict.get stringPath accu.localOnly of
                        Just ( _, item, syncTag ) ->
                            case syncTag of
                                Nothing ->
                                    { accu
                                        | localOnly = Dict.remove stringPath accu.localOnly
                                    }

                                Just (Synced localVersion) ->
                                    if localVersion == nextVersion then
                                        accu

                                    else
                                        { accu | newLocal = stores.localAccess.update nextPath (always <| Just ( nextItem, Just <| Synced nextVersion )) accu.newLocal }

                                Just (Changed localVersion) ->
                                    if nextVersion == localVersion then
                                        { accu | newRemote = stores.remoteAccess.update nextPath (always <| Just item) accu.newRemote }

                                    else
                                        { accu | conflicts = { conflictPath = nextPath, remoteItem = nextItem, remoteVersion = nextVersion, localItem = item } :: accu.conflicts }

                        Nothing ->
                            { accu | newLocal = stores.localAccess.insert nextPath nextItem (Just <| Synced nextVersion) accu.newLocal }
                )
                { localOnly =
                    stores.localAccess.listAll path stores.local
                        |> List.map (\( p, item, syncTag ) -> ( FilePath.toString p, ( p, item, syncTag ) ))
                        |> Dict.fromList
                , conflicts = []
                , newLocal = stores.local
                , newRemote = stores.remote
                }
                (stores.remoteAccess.listAll path stores.remote)

        ( updatedLocalOnly, updatedLocal ) =
            List.foldl
                (\{ conflictPath, remoteItem, remoteVersion, localItem } ( currentLocalOnly, currentLocal ) ->
                    let
                        ( newPath, newCurrentLocal ) =
                            stores.localAccess.update conflictPath (always <| Just ( remoteItem, Nothing )) currentLocal
                                |> stores.localAccess.insertWithRename conflictPath localItem Nothing
                    in
                    ( ( newPath, localItem, Nothing ) :: currentLocalOnly, newCurrentLocal )
                )
                ( Dict.values localOnly, newLocal )
                (Debug.log "conflicts" conflicts)

        updatedRemote =
            stores.remoteAccess.insertList newRemote (updatedLocalOnly |> List.map (\( p, entry, _ ) -> ( p, entry )))
    in
    SyncedStore { stores | local = updatedLocal, remote = updatedRemote }
