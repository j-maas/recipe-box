module Store.SyncedStore exposing (StoreAccess, SyncedStore, delete, insert, local, read, remote, sync, update, with)

import Dict
import Store.FilePath as FilePath exposing (FilePath)
import Store.FolderPath exposing (FolderPath)


type SyncedStore local remote item
    = SyncedStore
        { local : local
        , localAccess : StoreAccess local item
        , remote : remote
        , remoteAccess : StoreAccess remote item
        }


type alias StoreAccess store item =
    { insert : FilePath -> item -> store -> store
    , insertList : store -> List ( FilePath, item ) -> store
    , read : FilePath -> store -> Maybe item
    , update : FilePath -> (Maybe item -> Maybe item) -> store -> store
    , delete : FilePath -> store -> store
    , listAll : FolderPath -> store -> List ( FilePath, item )
    }


with :
    { local : ( local, StoreAccess local item )
    , remote : ( remote, StoreAccess remote item )
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
            | local = stores.localAccess.insert path item stores.local
            , remote = stores.remoteAccess.insert path item stores.remote
        }


read : FilePath -> SyncedStore local remote item -> Maybe item
read path (SyncedStore stores) =
    stores.localAccess.read path stores.local


update : FilePath -> (Maybe item -> Maybe item) -> SyncedStore local remote item -> SyncedStore local remote item
update path f (SyncedStore stores) =
    SyncedStore
        { stores
            | local = stores.localAccess.update path f stores.local
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
        { newLocal, newRemote, localOnly } =
            List.foldl
                (\( nextPath, nextItem ) accu ->
                    case stores.localAccess.read nextPath accu.newLocal of
                        Just _ ->
                            { accu
                                | localOnly = Dict.remove (FilePath.toString nextPath) accu.localOnly
                            }

                        Nothing ->
                            { accu | newLocal = stores.localAccess.insert nextPath nextItem accu.newLocal }
                )
                { localOnly =
                    stores.localAccess.listAll path stores.local
                        |> List.map (\( p, item ) -> ( FilePath.toString p, ( p, item ) ))
                        |> Dict.fromList
                , newLocal = stores.local
                , newRemote = stores.remote
                }
                (stores.remoteAccess.listAll path stores.remote)

        updatedRemote =
            stores.remoteAccess.insertList newRemote (Dict.values localOnly)
    in
    SyncedStore { stores | local = newLocal, remote = updatedRemote }
