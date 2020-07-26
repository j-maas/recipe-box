module Store.SyncedStore exposing (StoreAccess, SyncedStore, insert, local, read, remote, with)

import Store.FilePath exposing (FilePath)
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
    , read : FilePath -> store -> Maybe item
    , update : FilePath -> (Maybe item -> Maybe item) -> store -> store
    , delete : FilePath -> store -> store
    , list : FolderPath -> store -> List ( FilePath, item )
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
