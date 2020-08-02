module TestUtils exposing (buildFolderPath, buildPathComponent, charsetFuzzer, entriesFuzzer, filePathFuzzer, pathComponentFuzzer, safePathChars, sortEntries)

import Array
import Fuzz exposing (Fuzzer)
import Random
import Shrink
import Store.FilePath as FilePath exposing (FilePath)
import Store.FolderPath as FolderPath exposing (FolderPath)
import Store.PathComponent as PathComponent exposing (PathComponent)
import Store.Store as Store exposing (Store)


filePathFuzzer : Fuzzer FilePath
filePathFuzzer =
    Fuzz.map2
        (\folder name ->
            { folder = folder
            , name = name
            }
        )
        (Fuzz.list pathComponentFuzzer)
        pathComponentFuzzer


pathComponentFuzzer : Fuzzer PathComponent
pathComponentFuzzer =
    let
        validChars =
            Array.fromList safePathChars
    in
    Fuzz.map2
        (\first rest ->
            PathComponent.unsafe (String.cons first rest)
        )
        (Fuzz.intRange 0 (Array.length validChars - 1)
            |> Fuzz.map
                (\index ->
                    case Array.get index validChars of
                        Just c ->
                            c

                        Nothing ->
                            Debug.todo "Invalid index"
                )
        )
        (charsetFuzzer safePathChars)


charsetFuzzer : List Char -> Fuzz.Fuzzer String
charsetFuzzer chars =
    Fuzz.list (Fuzz.oneOf (List.map Fuzz.constant chars))
        |> Fuzz.map String.fromList


safePathChars : List Char
safePathChars =
    [ 'A', 'B', 'C', 'X', 'Y', 'Z', 'a', 'b', 'c', 'x', 'y', 'z', ' ', '-', '(', ')', '_', '.' ]


buildPathComponent : String -> PathComponent
buildPathComponent raw =
    case PathComponent.fromString raw of
        Just p ->
            p

        Nothing ->
            Debug.todo "Invalid path component"


buildFolderPath : List String -> FolderPath
buildFolderPath raw =
    case FolderPath.fromList raw of
        Just f ->
            f

        Nothing ->
            Debug.todo "Invalid folder path"


sortEntries : List ( FilePath, item ) -> List ( FilePath, item )
sortEntries list =
    List.sortBy (\( path, _ ) -> FilePath.toString path) list


entriesFuzzer : Fuzz.Fuzzer (List ( FilePath, Int ))
entriesFuzzer =
    let
        filePathGenerator =
            Random.map2
                (\folder name ->
                    { folder = folder, name = name }
                )
                (Random.int 0 5
                    |> Random.andThen
                        (\length ->
                            Random.list length pathComponentGenerator
                        )
                )
                pathComponentGenerator

        pathComponentGenerator =
            Random.int 1 5
                |> Random.andThen
                    (\length ->
                        Random.list length (Random.uniform 'A' safePathChars)
                    )
                |> Random.map String.fromList
                |> Random.map PathComponent.unsafe

        shrinkFilePath =
            Shrink.convert (\( folder, name ) -> { folder = folder, name = name })
                (\filePath -> ( filePath.folder, filePath.name ))
                (Shrink.tuple ( Shrink.list shrinkPathComponent, shrinkPathComponent ))

        shrinkPathComponent =
            Shrink.convert
                PathComponent.unsafe
                PathComponent.toString
                (Shrink.string
                    |> Shrink.dropIf String.isEmpty
                )
    in
    Fuzz.custom
        (Random.int 0 50
            |> Random.andThen
                (\length ->
                    Random.list length
                        (Random.map2 Tuple.pair
                            filePathGenerator
                            (Random.int 0 100)
                        )
                )
        )
        (Shrink.list (Shrink.tuple ( shrinkFilePath, Shrink.int )))
