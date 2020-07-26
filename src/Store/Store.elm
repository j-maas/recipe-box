module Store.Store exposing (FilePath, FolderPath, Store, delete, empty, insert, insertList, list, read, update)

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
list path store =
    listRec path path store


listRec : FolderPath -> FolderPath -> Store item -> List ( FilePath, item )
listRec fullFolderPath currentPath (Store (Folder folder)) =
    case currentPath of
        [] ->
            Dict.toList folder.contents
                |> List.map
                    (\( name, content ) ->
                        ( { folder = fullFolderPath

                          -- This is ok because only valid file paths are inserted into the dict.
                          , name = PathComponent.unsafe name
                          }
                        , content
                        )
                    )

        child :: rest ->
            Dict.get (PathComponent.toString child) folder.children
                |> Maybe.map
                    (\subFolder ->
                        listRec fullFolderPath rest (Store subFolder)
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
