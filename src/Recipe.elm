module Recipe exposing (Part(..), Parts, Recipe, from, ingredients, map, method, title)

import Ingredient exposing (Ingredient)


type Recipe
    = Recipe { title : String, steps : Parts }


type alias Parts =
    List (List Part)


type Part
    = PlainPart String
    | IngredientPart Ingredient


from : String -> Parts -> Recipe
from t parts =
    Recipe
        { title = t
        , steps = parts
        }


title : Recipe -> String
title (Recipe recipe) =
    recipe.title


method : Recipe -> Parts
method (Recipe recipe) =
    recipe.steps


ingredients : Parts -> List Ingredient
ingredients parts =
    List.concat parts
        |> List.filterMap
            (\part ->
                case part of
                    IngredientPart ingred ->
                        Just ingred

                    _ ->
                        Nothing
            )


map : (Part -> a) -> Recipe -> List (List a)
map f (Recipe recipe) =
    List.map (\paragraph -> List.map f paragraph) recipe.steps
