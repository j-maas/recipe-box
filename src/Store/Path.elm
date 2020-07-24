module Store.Path exposing (Path, from, fromString, toString)

import List.Extra as List
import Store.PathComponent as PathComponent exposing (PathComponent)


type alias Path =
    { folder : List PathComponent
    , name : PathComponent
    , extension : ( PathComponent, List PathComponent )
    }


fromString : String -> Maybe Path
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


from : { folder : List String, name : String, extension : ( String, List String ) } -> Maybe Path
from rawPath =
    mapMaybes PathComponent.fromString rawPath.folder
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
                    (mapMaybes PathComponent.fromString rawExtensionRest)
            )


mapMaybes : (a -> Maybe b) -> List a -> Maybe (List b)
mapMaybes mapping list =
    List.foldr
        (\a accu ->
            accu
                |> Maybe.andThen
                    (\previous ->
                        mapping a
                            |> Maybe.map
                                (\b ->
                                    b :: previous
                                )
                    )
        )
        (Just [])
        list


toString : Path -> String
toString path =
    let
        ( extension, extensionRest ) =
            path.extension
    in
    -- Ensure a trailing slash, if there are folder components.
    String.join "/" (List.map PathComponent.toString path.folder ++ [ "" ])
        ++ PathComponent.toString path.name
        -- Ensure a leading dot.
        ++ String.join "." ("" :: List.map PathComponent.toString (extension :: extensionRest))
