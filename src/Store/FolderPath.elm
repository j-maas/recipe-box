module Store.FolderPath exposing (FolderPath, fromList, fromString, toString)

import List.Extra as List
import Store.PathComponent as PathComponent exposing (PathComponent)
import Utils


type alias FolderPath =
    List PathComponent


fromString : String -> Maybe FolderPath
fromString raw =
    String.split "/" raw
        |> List.unconsLast
        |> Maybe.andThen
            (\( last, previous ) ->
                case last of
                    "" ->
                        fromList previous

                    -- In case the trailing slash is missing, we do not want to ignore the last component.
                    lastComponent ->
                        fromList (previous ++ [ lastComponent ])
            )


fromList : List String -> Maybe FolderPath
fromList raw =
    case raw of
        -- This happens when String.split is called on "/".
        -- We just handle it here because otherwise we need to clean it up in a lot of places.
        [ "" ] ->
            Just []

        components ->
            Utils.mapMaybes PathComponent.fromString components


toString : FolderPath -> String
toString path =
    (List.map PathComponent.toString path
        |> String.join "/"
    )
        ++ "/"
