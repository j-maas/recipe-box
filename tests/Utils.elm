module Utils exposing (charsetFuzzer, pathComponentFuzzer, safePathChars)

import Fuzz exposing (Fuzzer)
import Store.PathComponent as PathComponent exposing (PathComponent)


pathComponentFuzzer : Fuzzer PathComponent
pathComponentFuzzer =
    Fuzz.map2
        (\first rest ->
            PathComponent.unsafe (String.cons first rest)
        )
        (Fuzz.oneOf (List.map Fuzz.constant safePathChars))
        (charsetFuzzer safePathChars)


charsetFuzzer : List Char -> Fuzz.Fuzzer String
charsetFuzzer chars =
    Fuzz.list (Fuzz.oneOf (List.map Fuzz.constant chars))
        |> Fuzz.map String.fromList


safePathChars : List Char
safePathChars =
    [ 'A', 'B', 'C', 'X', 'Y', 'Z', 'a', 'b', 'c', 'x', 'y', 'z', ' ', '-', '(', ')', '_' ]
