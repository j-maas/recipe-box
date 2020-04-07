module Language exposing (Language, available, fromString)

import Dict exposing (Dict)


type alias Language a =
    { title : String
    , overview :
        { goToShoppingList : String
        , newRecipe : String
        }
    , recipe :
        { edit : String
        , delete : String
        , ingredientsWithCount : Int -> String
        , noIngredientsRequired : String
        , steps : String
        }
    , editRecipe :
        { save : String
        }
    , shoppingList :
        { title : String
        , selectedRecipesWithCount : Int -> String
        , noRecipeSelected : String
        , remove : String
        , addRecipesWithCount : Int -> String
        , add : String
        , allRecipesSelected : String
        , emptyShoppingList : String
        }
    , noRecipes : (String -> a) -> (String -> a) -> List a
    , goToOverview : String
    }


fromString : String -> Language a
fromString code =
    if String.startsWith "de" code then
        deutsch

    else
        english


available : Dict String { name : String, content : Language a }
available =
    Dict.fromList
        [ ( "en", { name = "English", content = english } )
        , ( "de", { name = "Deutsch", content = deutsch } )
        ]


english : Language a
english =
    { title = "Recipe Box"
    , overview =
        { goToShoppingList = "Go to shopping list"
        , newRecipe = "New recipe"
        }
    , recipe =
        { edit = "Edit"
        , delete = "Delete"
        , ingredientsWithCount =
            \count ->
                case count of
                    0 ->
                        "Ingredients (none)"

                    n ->
                        "Ingredients (" ++ String.fromInt n ++ ")"
        , noIngredientsRequired = "No ingredients required."
        , steps = "Steps"
        }
    , editRecipe =
        { save = "Save"
        }
    , shoppingList =
        { title = "Shopping List"
        , selectedRecipesWithCount =
            \count ->
                case count of
                    0 ->
                        "No selected recipes"

                    1 ->
                        "1 selected recipe"

                    n ->
                        String.fromInt n ++ " selected recipes"
        , noRecipeSelected = "You have selected no recipe."
        , remove = "Remove"
        , addRecipesWithCount =
            \count ->
                "Add recipes (" ++ String.fromInt count ++ " available)"
        , add = "Add"
        , allRecipesSelected = "You have selected all recipes."
        , emptyShoppingList = "Your shopping list is empty. (Select some recipes by opening the list of selected recipes.)"
        }
    , noRecipes =
        \normalText linkToNew ->
            [ normalText "You do not have any recipes yet. Create a "
            , linkToNew "new recipe"
            , normalText "!"
            ]
    , goToOverview = "Go to recipe list"
    }


deutsch : Language a
deutsch =
    { title = "Rezeptkasten"
    , overview =
        { goToShoppingList = "Zur Einkaufsliste"
        , newRecipe = "Neues Rezept"
        }
    , recipe =
        { edit = "Bearbeiten"
        , delete = "Löschen"
        , ingredientsWithCount =
            \count ->
                case count of
                    0 ->
                        "Zutaten (keine)"

                    n ->
                        "Zutaten (" ++ String.fromInt n ++ ")"
        , noIngredientsRequired = "Keine Zutaten nötig."
        , steps = "Zubereitung"
        }
    , editRecipe =
        { save = "Speichern"
        }
    , shoppingList =
        { title = "Einkaufsliste"
        , selectedRecipesWithCount =
            \count ->
                case count of
                    0 ->
                        "Kein Rezept ausgewählt"

                    1 ->
                        "1 ausgewähltes Rezept"

                    n ->
                        String.fromInt n ++ " ausgewählte Rezepte"
        , noRecipeSelected = "Du hast kein Rezept ausgewählt."
        , remove = "Entfernen"
        , addRecipesWithCount = \count -> "Rezepte hinzufügen (" ++ String.fromInt count ++ " verfügbar)"
        , add = "Hinzufügen"
        , allRecipesSelected = "Du hast alle Rezepte ausgewählt."
        , emptyShoppingList = "Deine Einkaufsliste ist leer. (Füge Rezepte hinzu, indem du die Liste der ausgewählten Rezepte aufklappst.)"
        }
    , noRecipes =
        \normalText linkToNew ->
            [ normalText "Du hast noch keine Rezepte. Füge ein "
            , linkToNew "neues Rezept"
            , normalText " hinzu!"
            ]
    , goToOverview = "Zur Rezeptliste"
    }
