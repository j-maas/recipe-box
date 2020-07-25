module Store.Store exposing (FilePath, FolderPath, Store, empty, insert, list, read)

import Dict exposing (Dict)
import Store.FilePath as FilePath
import Store.FolderPath as FolderPath
import Store.PathComponent as PathComponent exposing (PathComponent)


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


list : FolderPath -> Store item -> List item
list path (Store (Folder folder)) =
    case path of
        [] ->
            Dict.values folder.contents

        child :: rest ->
            Dict.get (PathComponent.toString child) folder.children
                |> Maybe.map
                    (\subFolder ->
                        list rest (Store subFolder)
                    )
                |> Maybe.withDefault []
