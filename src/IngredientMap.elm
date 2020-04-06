module IngredientMap exposing (IngredientMap, fromDescription, fromIngredients, fromList)

import Dict exposing (Dict)
import Recipe exposing (Ingredient, Quantity)


type alias IngredientMap =
    Dict String Quantities


type alias Quantities =
    { descriptions : List String
    , amount : Maybe Float
    , measures : Measures
    }


type alias Measures =
    Dict String Float


emptyQuantities : Quantities
emptyQuantities =
    { descriptions = []
    , amount = Nothing
    , measures = Dict.empty
    }


fromList : List ( String, Maybe Quantity ) -> IngredientMap
fromList ingredients =
    List.foldl
        (\( name, maybeQuantity ) accumulated ->
            Dict.update name
                (\maybeExisting ->
                    let
                        existing =
                            Maybe.withDefault emptyQuantities maybeExisting
                    in
                    Just <|
                        case maybeQuantity of
                            Just quantity ->
                                addQuantity quantity existing

                            Nothing ->
                                existing
                )
                accumulated
        )
        Dict.empty
        ingredients


fromIngredients : List Ingredient -> IngredientMap
fromIngredients ingredients =
    ingredients
        |> List.map
            (\ingredient ->
                ( Recipe.getListName ingredient, Recipe.getQuantity ingredient )
            )
        |> fromList


fromDescription : Recipe.RecipeParts -> IngredientMap
fromDescription parts =
    Recipe.ingredients parts
        |> fromIngredients


addQuantity : Quantity -> Quantities -> Quantities
addQuantity new existing =
    case new of
        Recipe.Description description ->
            { existing | descriptions = description :: existing.descriptions }

        Recipe.Amount amount ->
            { existing | amount = Just <| Maybe.withDefault 0 existing.amount + amount }

        Recipe.Measure amount unit ->
            { existing | measures = addMeasure amount unit existing.measures }


addMeasure : Float -> String -> Measures -> Measures
addMeasure amount unit measures =
    Dict.update unit
        (\maybeExisting ->
            Just
                (Maybe.withDefault 0 maybeExisting + amount)
        )
        measures
