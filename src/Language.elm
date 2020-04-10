module Language exposing (Language, available, fromString)

import Dict exposing (Dict)
import RecipeParser exposing (Context(..), DeadEnd, Problem(..))


type alias Language a =
    { title : String
    , overview :
        { goToShoppingList : String
        , newRecipe : String
        }
    , recipe :
        { edit : String
        , delete : String
        , ingredients : String
        , noIngredientsRequired : String
        , method : String
        }
    , editRecipe :
        { problemCount : Int -> String
        , explainDeadEnd : DeadEnd -> String
        , save : String
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
    , clearChecks : String
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
        [ ( "en", { name = "🇺🇸 English", content = english } )
        , ( "de", { name = "🇩🇪 Deutsch", content = deutsch } )
        ]


deadEndHelper : DeadEnd -> LanguageDeadEnd
deadEndHelper deadEnd =
    let
        contextStack =
            deadEnd.contextStack |> List.map .context

        lastContext =
            List.head contextStack
    in
    case lastContext of
        Just TitleContext ->
            TitleDeadEnd deadEnd.problem

        Just IngredientContext ->
            IngredientDeadEnd Nothing deadEnd.problem

        Just (IngredientNameContext name) ->
            IngredientDeadEnd (Just name) deadEnd.problem

        _ ->
            ProblemDeadEnd deadEnd.problem


type LanguageDeadEnd
    = TitleDeadEnd Problem
    | IngredientDeadEnd (Maybe String) Problem
    | ProblemDeadEnd Problem


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
        , ingredients = "Ingredients"
        , noIngredientsRequired = "No ingredients required."
        , method = "Method"
        }
    , editRecipe =
        { problemCount =
            \count ->
                case count of
                    1 ->
                        "The recipe could not be saved because of a problem:"

                    n ->
                        "The recipe could not be saved because of " ++ String.fromInt n ++ " problems:"
        , explainDeadEnd = explainDeadEndInEnglish
        , save = "Save"
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
    , clearChecks = "Clear all checkmarks"
    , noRecipes =
        \normalText linkToNew ->
            [ normalText "You do not have any recipes yet. Create a "
            , linkToNew "new recipe"
            , normalText "!"
            ]
    , goToOverview = "Go to recipe list"
    }


explainDeadEndInEnglish : DeadEnd -> String
explainDeadEndInEnglish deadEnd =
    case deadEndHelper deadEnd of
        TitleDeadEnd problem ->
            "The recipe must start with a '# title', but I had a problem:\n" ++ explainProblemInEnglish problem

        IngredientDeadEnd maybeName problem ->
            "I found an '<ingredient>'"
                ++ (case maybeName of
                        Just name ->
                            " called '" ++ name ++ "'"

                        Nothing ->
                            ""
                                ++ ", but I ran into a problem:\n"
                                ++ explainProblemInEnglish problem
                   )

        ProblemDeadEnd problem ->
            explainProblemInEnglish problem


explainProblemInEnglish : Problem -> String
explainProblemInEnglish problem =
    case problem of
        Expecting char ->
            "A '" ++ char ++ "' is missing."

        ExpectingLineBreak ->
            "A line break is missing."

        ExpectingEnd ->
            "There is too much text."

        ExpectingFloat ->
            "A number is missing."

        InvalidNumber ->
            "The number is not valid."

        EmptyText ->
            "Some text is missing."


deutsch : Language a
deutsch =
    { title = "Rezeptekasten"
    , overview =
        { goToShoppingList = "Zur Einkaufsliste"
        , newRecipe = "Neues Rezept"
        }
    , recipe =
        { edit = "Bearbeiten"
        , delete = "Löschen"
        , ingredients = "Zutaten"
        , noIngredientsRequired = "Keine Zutaten nötig."
        , method = "Zubereitung"
        }
    , editRecipe =
        { problemCount =
            \count ->
                case count of
                    1 ->
                        "Das Rezept konnte aufgrund eines Problems nicht gespeichert werden:"

                    n ->
                        "Das Rezept konnte aufgrund von " ++ String.fromInt n ++ " Problemen nicht gespeichert werden:"
        , explainDeadEnd = explainDeadEndInDeutsch
        , save = "Speichern"
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
    , clearChecks = "Alle Häckchen entfernen"
    , noRecipes =
        \normalText linkToNew ->
            [ normalText "Du hast noch keine Rezepte. Füge ein "
            , linkToNew "neues Rezept"
            , normalText " hinzu!"
            ]
    , goToOverview = "Zur Rezeptliste"
    }


explainDeadEndInDeutsch : DeadEnd -> String
explainDeadEndInDeutsch deadEnd =
    case deadEndHelper deadEnd of
        TitleDeadEnd problem ->
            "Das Rezept muss mit einem '# Titel' beginnen, aber es gab ein Problem:\n" ++ explainProblemInDeutsch problem

        IngredientDeadEnd maybeName problem ->
            "Ich habe eine 'Zutat'"
                ++ (case maybeName of
                        Just name ->
                            " namens '" ++ name ++ "'"

                        Nothing ->
                            ""
                   )
                ++ " gefunden, aber es gab ein Problem:\n"
                ++ explainProblemInDeutsch problem

        ProblemDeadEnd problem ->
            explainProblemInDeutsch problem


explainProblemInDeutsch : Problem -> String
explainProblemInDeutsch problem =
    case problem of
        Expecting char ->
            "Ein '" ++ char ++ "' fehlt."

        ExpectingLineBreak ->
            "Es fehlt ein Zeilenumbruch."

        ExpectingEnd ->
            "Es gibt zu viel Text."

        ExpectingFloat ->
            "Eine Nummer fehlt."

        InvalidNumber ->
            "Eine Nummer ist ungültig."

        EmptyText ->
            "Es fehlt Text."
