module Store.VersionStore exposing (VersionStore, delete, empty, insert, insertList, insertWithRename, list, listAll, read, subfolders, update)

import Store.FilePath exposing (FilePath)
import Store.FolderPath exposing (FolderPath)
import Store.Store as Store exposing (Store)


type VersionStore item
    = VersionStore Int (Store ( item, Version ))


type alias Version =
    String


empty : VersionStore item
empty =
    VersionStore 0 Store.empty


insert : FilePath -> item -> VersionStore item -> ( Version, VersionStore item )
insert path item (VersionStore counter store) =
    let
        version =
            String.fromInt counter
    in
    ( version, VersionStore (counter + 1) (Store.insert path ( item, version ) store) )


insertWithRename : FilePath -> item -> VersionStore item -> ( FilePath, Version, VersionStore item )
insertWithRename path item (VersionStore counter store) =
    let
        version =
            String.fromInt counter

        ( newPath, newStore ) =
            Store.insertWithRename path ( item, version ) store
    in
    ( newPath, version, VersionStore (counter + 1) newStore )


insertList : VersionStore item -> List ( FilePath, item ) -> VersionStore item
insertList (VersionStore initialCounter initialStore) items =
    let
        newStore =
            List.indexedMap (\index ( path, item ) -> ( path, ( item, String.fromInt <| index + initialCounter ) ))
                items
                |> List.foldl
                    (\( path, entry ) store ->
                        Store.insert path entry store
                    )
                    initialStore
    in
    VersionStore (initialCounter + List.length items) newStore


read : FilePath -> VersionStore item -> Maybe ( item, Version )
read path (VersionStore _ store) =
    Store.read path store


list : FolderPath -> VersionStore item -> List ( FilePath, ( item, Version ) )
list path (VersionStore _ store) =
    Store.list path store


listAll : FolderPath -> VersionStore item -> List ( FilePath, ( item, Version ) )
listAll path (VersionStore _ store) =
    Store.listAll path store


subfolders : FolderPath -> VersionStore item -> List FolderPath
subfolders path (VersionStore _ store) =
    Store.subfolders path store


update : FilePath -> (Maybe item -> Maybe item) -> VersionStore item -> VersionStore item
update path f (VersionStore counter store) =
    VersionStore (counter + 1)
        (Store.update
            path
            (\maybeEntry ->
                case maybeEntry of
                    Nothing ->
                        f Nothing |> Maybe.map (\item -> ( item, String.fromInt counter ))

                    Just ( item, _ ) ->
                        f (Just item) |> Maybe.map (\i -> ( i, String.fromInt counter ))
            )
            store
        )


delete : FilePath -> VersionStore item -> VersionStore item
delete path (VersionStore counter store) =
    VersionStore counter (Store.delete path store)
