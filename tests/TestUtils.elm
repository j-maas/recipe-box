module TestUtils exposing (charsetFuzzer, filePathFuzzer, pathComponentFuzzer, safePathChars)

import Array
import Fuzz exposing (Fuzzer)
import Store.FilePath exposing (FilePath)
import Store.PathComponent as PathComponent exposing (PathComponent)


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
