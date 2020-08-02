module Store.Store exposing (FilePath, FolderPath, Store, insertWithRename, delete, empty, insert, insertList, list, listAll, read, subfolders, update)

import Dict exposing (Dict)
import Store.FilePath as FilePath
import Store.FolderPath as FolderPath
import Store.PathComponent as PathComponent


type alias FilePath =
    FilePath.FilePath


type alias FolderPath =
    FolderPath.FolderPath


type Store item
    = Store (Folder item)



-- Cannot be a type alias, because it is recursive.


type Folder item
    = Folder
        { contents : Dict String item
        , children : Dict String (Folder item)
        }


empty : Store item
empty =
    Store emptyFolder


emptyFolder : Folder item
emptyFolder =
    Folder { contents = Dict.empty, children = Dict.empty }


insert : FilePath -> item -> Store item -> Store item
insert path item (Store (Folder folder)) =
    case path.folder of
        [] ->
            let
                key =
                    PathComponent.toString path.name
            in
            Store (Folder { folder | contents = Dict.insert key item folder.contents })

        child :: rest ->
            Store
                (Folder
                    { folder
                        | children =
                            Dict.update (PathComponent.toString child)
                                (\existing ->
                                    let
                                        subFolder =
                                            Maybe.withDefault emptyFolder existing
                                    in
                                    case insert { path | folder = rest } item (Store subFolder) of
                                        Store newFolder ->
                                            Just newFolder
                                )
                                folder.children
                    }
                )


insertWithRename : FilePath -> item -> Store item -> ( FilePath, Store item )
insertWithRename path item store =
    let
        newPath =
            { path
                | name =
                    PathComponent.autorename (PathComponent.toString path.name)
                        (\candidate ->
                            read { path | name = candidate } store
                                |> Maybe.map (always False)
                                |> Maybe.withDefault True
                        )
            }
    in
    ( newPath, insert newPath item store )


insertList : Store item -> List ( FilePath, item ) -> Store item
insertList initialStore items =
    List.foldl
        (\( path, item ) store ->
            insert path item store
        )
        initialStore
        items


read : FilePath -> Store item -> Maybe item
read path (Store (Folder folder)) =
    case path.folder of
        [] ->
            Dict.get (PathComponent.toString path.name) folder.contents

        child :: rest ->
            Dict.get (PathComponent.toString child) folder.children
                |> Maybe.andThen
                    (\subFolder ->
                        read { path | folder = rest } (Store subFolder)
                    )


list : FolderPath -> Store item -> List ( FilePath, item )
list path (Store f) =
    findFolder
        (folderEntries path)
        []
        path
        f


findFolder : (Folder item -> a) -> a -> FolderPath -> Folder item -> a
findFolder f default currentPath (Folder folder) =
    case currentPath of
        [] ->
            f (Folder folder)

        child :: rest ->
            Dict.get (PathComponent.toString child) folder.children
                |> Maybe.map
                    (\subFolder ->
                        findFolder f default rest subFolder
                    )
                |> Maybe.withDefault default


folderEntries : FolderPath -> Folder item -> List ( FilePath, item )
folderEntries path (Folder folder) =
    Dict.toList folder.contents
        |> List.map
            (\( name, content ) ->
                ( { folder = path

                  -- This is ok because only valid file paths are inserted into the dict.
                  , name = PathComponent.unsafe name
                  }
                , content
                )
            )


listAll : FolderPath -> Store item -> List ( FilePath, item )
listAll path (Store f) =
    findFolder
        (listAllRec path)
        []
        path
        f


listAllRec : FolderPath -> Folder item -> List ( FilePath, item )
listAllRec path (Folder folder) =
    folderEntries path (Folder folder)
        ++ (List.concatMap
                (\( subName, subFolder ) ->
                    listAllRec (path ++ [ PathComponent.unsafe subName ]) subFolder
                )
            <|
                Dict.toList folder.children
           )


subfolders : FolderPath -> Store item -> List FolderPath
subfolders path (Store (Folder folder)) =
    case path of
        [] ->
            Dict.keys folder.children
                -- This is ok because only valid folder paths are inserted into the dict.
                |> List.map PathComponent.unsafe
                |> List.map List.singleton

        child :: rest ->
            Dict.get (PathComponent.toString child) folder.children
                |> Maybe.map
                    (\subFolder ->
                        subfolders rest (Store subFolder)
                            |> List.map (\subpath -> child :: subpath)
                    )
                |> Maybe.withDefault []


update : FilePath -> (Maybe item -> Maybe item) -> Store item -> Store item
update path f (Store (Folder folder)) =
    case path.folder of
        [] ->
            let
                newContents =
                    Dict.update (PathComponent.toString path.name) f folder.contents
            in
            Store (Folder { folder | contents = newContents })

        child :: rest ->
            Store
                (Folder
                    { folder
                        | children =
                            Dict.update (PathComponent.toString child)
                                (Maybe.map
                                    (\subFolder ->
                                        case update { path | folder = rest } f (Store subFolder) of
                                            Store newFolder ->
                                                newFolder
                                    )
                                )
                                folder.children
                    }
                )


delete : FilePath -> Store item -> Store item
delete path store =
    update path (always Nothing) store
