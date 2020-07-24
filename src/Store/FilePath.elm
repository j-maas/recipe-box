module Store.FilePath exposing (FilePath, from, fromString, toString)

import List.Extra as List
import Store.FolderPath as FolderPath exposing (FolderPath)
import Store.PathComponent as PathComponent exposing (PathComponent)
import Utils


type alias FilePath =
    { folder : FolderPath
    , name : PathComponent
    , extension : ( PathComponent, List PathComponent )
    }


fromString : String -> Maybe FilePath
fromString raw =
    String.split "/" raw
        |> List.unconsLast
        |> Maybe.andThen
            (\( last, folder ) ->
                case String.split "." last of
                    name :: extension :: extensionRest ->
                        from { folder = folder, name = name, extension = ( extension, extensionRest ) }

                    _ ->
                        Nothing
            )


from : { folder : List String, name : String, extension : ( String, List String ) } -> Maybe FilePath
from rawPath =
    FolderPath.fromList rawPath.folder
        |> Maybe.map (\folder -> { folder = folder })
        |> Maybe.andThen
            (\path ->
                PathComponent.fromString rawPath.name
                    |> Maybe.map (\name -> { folder = path.folder, name = name })
            )
        |> Maybe.andThen
            (\path ->
                let
                    ( rawExtension, rawExtensionRest ) =
                        rawPath.extension
                in
                Maybe.map2
                    (\extension extensionRest ->
                        { folder = path.folder, name = path.name, extension = ( extension, extensionRest ) }
                    )
                    (PathComponent.fromString rawExtension)
                    (Utils.mapMaybes PathComponent.fromString rawExtensionRest)
            )


toString : FilePath -> String
toString path =
    let
        ( extension, extensionRest ) =
            path.extension
    in
    -- Ensure a trailing slash, if there are folder components.
    FolderPath.toString path.folder
        ++ PathComponent.toString path.name
        -- Ensure a leading dot.
        ++ String.join "." ("" :: List.map PathComponent.toString (extension :: extensionRest))
