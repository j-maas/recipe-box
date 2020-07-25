module Store.FilePath exposing (FilePath, from, fromString, toString)

import List.Extra as List
import Store.FolderPath as FolderPath exposing (FolderPath)
import Store.PathComponent as PathComponent exposing (PathComponent)
import Utils


type alias FilePath =
    { folder : FolderPath
    , name : PathComponent
    }


fromString : String -> Maybe FilePath
fromString raw =
    String.split "/" raw
        |> List.unconsLast
        |> Maybe.andThen
            (\( last, folder ) ->
                from { folder = folder, name = last }
            )


from : { folder : List String, name : String } -> Maybe FilePath
from rawPath =
    FolderPath.fromList rawPath.folder
        |> Maybe.map (\folder -> { folder = folder })
        |> Maybe.andThen
            (\path ->
                PathComponent.fromString rawPath.name
                    |> Maybe.map (\name -> { folder = path.folder, name = name })
            )


toString : FilePath -> String
toString path =
    -- Ensure a trailing slash, if there are folder components.
    FolderPath.toString path.folder
        ++ PathComponent.toString path.name
