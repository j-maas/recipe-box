module Utils exposing (mapMaybes)


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
