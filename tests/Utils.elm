module Utils exposing (charsetFuzzer, pathComponentFuzzer, safePathChars)

import Fuzz exposing (Fuzzer)
import Store.PathComponent as PathComponent exposing (PathComponent)


pathComponentFuzzer : Fuzzer PathComponent
pathComponentFuzzer =
    charsetFuzzer safePathChars
        |> Fuzz.map PathComponent.unsafe


charsetFuzzer : List Char -> Fuzz.Fuzzer String
charsetFuzzer chars =
    Fuzz.list (Fuzz.oneOf (List.map Fuzz.constant chars))
        |> Fuzz.map String.fromList


safePathChars : List Char
safePathChars =
    [ 'A', 'B', 'C', 'X', 'Y', 'Z', 'a', 'b', 'c', 'x', 'y', 'z', ' ', '-', '(', ')', '_' ]
