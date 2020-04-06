port module Main exposing (main, p)

import Browser
import Browser.Navigation as Navigation
import Css exposing (auto, num, pct, rem, zero)
import Css.Global as Global
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import IngredientMap exposing (IngredientMap)
import Recipe exposing (Recipe)
import Set exposing (Set)
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Navigation.Key
    , state : State
    , screen : Screen
    }


type alias State =
    { recipes : RecipeStore
    , shoppingList : ShoppingList
    }


type alias ShoppingList =
    { selectedRecipes : Set String
    , extras : List Recipe.Ingredient
    }


type Screen
    = Overview
    | Recipe Recipe
    | Edit { code : String, error : Maybe String }
    | Shopping ShoppingState


type alias ShoppingState =
    { openSelection : Bool }


type alias RecipeStore =
    Dict String ( Recipe.RecipeParts, String )


type alias Flags =
    { recipes : List String
    , recipesOnShoppingList : List String
    }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        recipes =
            List.filterMap
                (\code ->
                    Recipe.parse code
                        |> Result.toMaybe
                        |> Maybe.map (\recipe -> ( recipe, code ))
                )
                flags.recipes
                |> List.map (\( recipe, code ) -> ( Recipe.title recipe, ( Recipe.description recipe, code ) ))
                |> Dict.fromList

        shoppingList =
            { selectedRecipes = flags.recipesOnShoppingList|> Set.fromList
            , extras = []
            }

        state =
            { recipes = recipes
            , shoppingList = shoppingList
            }
    in
    ( { key = key
      , state = state
      , screen = parseRoute url |> screenFromRoute state |> Maybe.withDefault Overview
      }
    , Cmd.none
    )


type Msg
    = DeleteRecipe String
    | Edited String
    | Save
    | AddRecipeToShoppingList String
    | RemoveRecipeFromShoppingList String
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateState func m =
            { m | state = func m.state }

        toScreen route =
            screenFromRoute model.state route |> Maybe.withDefault model.screen
    in
    case msg of
        DeleteRecipe title ->
            ( { model
                | screen = toScreen OverviewRoute
              }
                |> updateState
                    (\state ->
                        { state | recipes = Dict.remove title state.recipes }
                    )
            , removeRecipe title
            )

        Edited code ->
            let
                newScreen =
                    case model.screen of
                        Edit _ ->
                            Edit { code = code, error = Nothing }

                        _ ->
                            model.screen
            in
            ( { model | screen = newScreen }, Cmd.none )

        Save ->
            case model.screen of
                Edit { code } ->
                    case Recipe.parse code of
                        Ok recipe ->
                            let
                                title =
                                    Recipe.title recipe

                                parts =
                                    Recipe.description recipe
                            in
                            ( { model | screen = Recipe recipe }
                                |> updateState
                                    (\state ->
                                        { state
                                            | recipes =
                                                Dict.insert title
                                                    ( parts, code )
                                                    state.recipes
                                        }
                                    )
                            , saveRecipe { title = title, code = code }
                            )

                        Err error ->
                            ( { model | screen = Edit { code = code, error = Just error } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AddRecipeToShoppingList title ->
            let
                state =
                    model.state

                oldShoppingList =
                    state.shoppingList

                newShoppingList =
                    { oldShoppingList | selectedRecipes = Set.insert title oldShoppingList.selectedRecipes }
            in
            ( model
                |> updateState
                    (\s ->
                        { s | shoppingList = newShoppingList }
                    )
            , saveShoppingListCmd newShoppingList
            )

        RemoveRecipeFromShoppingList title ->
            let
                oldShoppingList =
                    model.state.shoppingList

                newShoppingList =
                    { oldShoppingList | selectedRecipes = Set.remove title oldShoppingList.selectedRecipes }

                newScreen =
                    case model.screen of
                        Shopping shoppingState ->
                            Shopping
                                { shoppingState
                                    | openSelection =
                                        -- Because Elm does not detect closing of details due to its virtual dom,
                                        -- this will not have the desired effect for now.
                                        Set.isEmpty newShoppingList.selectedRecipes
                                            || shoppingState.openSelection
                                }

                        _ ->
                            model.screen
            in
            ( { model | screen = newScreen }
                |> updateState
                    (\state ->
                        { state | shoppingList = newShoppingList }
                    )
            , saveShoppingListCmd newShoppingList
            )

        UrlChanged url ->
            ( { model | screen = toScreen (parseRoute url) }, Cmd.none )

        LinkClicked request ->
            case request of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )


type Route
    = OverviewRoute
    | RecipeRoute String
    | NewRoute
    | EditRoute String
    | ShoppingListRoute


screenFromRoute : State -> Route -> Maybe Screen
screenFromRoute state route =
    case route of
        OverviewRoute ->
            Just Overview

        RecipeRoute title ->
            Dict.get title state.recipes
                |> Maybe.map
                    (\( recipe, _ ) ->
                        Recipe (Recipe.from title recipe)
                    )

        NewRoute ->
            Just <| Edit { code = "", error = Nothing }

        EditRoute title ->
            Dict.get title state.recipes
                |> Maybe.map
                    (\( _, code ) ->
                        Edit { code = code, error = Nothing }
                    )

        ShoppingListRoute ->
            let
                openSelection =
                    Set.isEmpty state.shoppingList.selectedRecipes
            in
            Just <| Shopping { openSelection = openSelection }


stringFromRoute : Route -> String
stringFromRoute route =
    case route of
        OverviewRoute ->
            "#"

        RecipeRoute title ->
            "#recipe:" ++ Url.percentEncode title

        NewRoute ->
            "#new"

        EditRoute title ->
            "#edit:" ++ Url.percentEncode title

        ShoppingListRoute ->
            "#shopping"


parseRoute : Url -> Route
parseRoute url =
    url.fragment
        |> Maybe.andThen
            (\raw ->
                let
                    extractFrom index string =
                        String.slice index (String.length string) string
                            |> Url.percentDecode
                in
                if String.isEmpty raw then
                    Just OverviewRoute

                else if String.startsWith "recipe:" raw then
                    extractFrom 7 raw
                        |> Maybe.map RecipeRoute

                else if raw == "new" then
                    Just NewRoute

                else if String.startsWith "edit:" raw then
                    extractFrom 5 raw
                        |> Maybe.map EditRoute

                else if raw == "shopping" then
                    Just ShoppingListRoute

                else
                    Nothing
            )
        |> Maybe.withDefault OverviewRoute


port saveRecipe : { title : String, code : String } -> Cmd msg


port removeRecipe : String -> Cmd msg


saveShoppingListCmd : ShoppingList -> Cmd msg
saveShoppingListCmd shoppingList =
    shoppingList.selectedRecipes
        |> Set.toList
        |> saveShoppingList


port saveShoppingList : List String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Browser.Document Msg
view model =
    let
        state =
            model.state

        ( maybeSubtitle, body ) =
            case model.screen of
                Overview ->
                    viewOverview (Dict.keys state.recipes)

                Recipe recipe ->
                    viewRecipe recipe

                Edit { code, error } ->
                    viewEditRecipe code error

                Shopping shoppingState ->
                    viewShoppingList state.recipes state.shoppingList shoppingState
    in
    { title =
        "Recipe Box"
            ++ (maybeSubtitle
                    |> Maybe.map (\subtitle -> ": " ++ subtitle)
                    |> Maybe.withDefault ""
               )
    , body =
        List.map Html.toUnstyled
            [ Html.main_
                [ css
                    [ bodyFontStyle
                    , Css.lineHeight (num 1.4)
                    , Css.maxWidth (rem 48)
                    , Css.margin2 zero auto
                    , Css.marginTop (rem 2)
                    ]
                ]
                [ body ]
            ]
    }


viewOverview : List String -> ( Maybe String, Html Msg )
viewOverview recipeTitles =
    ( Nothing
    , Html.div []
        [ h1 [] [] [ Html.text "Recipe Box" ]
        , Html.nav [] [ navLink [] "Go to shopping list" ShoppingListRoute ]
        , toolbar [ linkButton "New recipe" NewRoute ]
        , viewRecipeList recipeTitles
        ]
    )


viewRecipeList : List String -> Html Msg
viewRecipeList recipeTitles =
    contentList
        (Html.div [] noRecipes)
        (ul
            [ recipeListStyle ]
            []
        )
        (\title ->
            Html.li [] [ viewRecipeLink title ]
        )
        recipeTitles


noRecipes : List (Html Msg)
noRecipes =
    [ Html.text "You do not have any recipes yet. Create a "
    , navLink [] "new recipe" NewRoute
    , Html.text "!"
    ]


recipeListStyle : Css.Style
recipeListStyle =
    Css.batch
        [ Css.property "list-style-type" "\">  \""
        , Css.listStylePosition Css.inside
        , Css.paddingLeft zero
        ]


viewRecipeLink : String -> Html Msg
viewRecipeLink title =
    Html.a
        [ css [ recipeLinkStyle ]
        , Attributes.href
            (RecipeRoute title |> stringFromRoute)
        ]
        [ Html.text title
        ]


recipeLinkStyle : Css.Style
recipeLinkStyle =
    Css.batch
        [ clickableStyle
        , linkUnstyle
        , Css.display Css.inlineBlock
        , Css.padding (rem 0.5)
        , onHover
            [ Css.backgroundColor (Css.hsla 0 0 0.5 0.1)
            ]
        ]


viewRecipe : Recipe -> ( Maybe String, Html Msg )
viewRecipe recipe =
    let
        title =
            Recipe.title recipe

        ingredientsView =
            viewIngredientsList (IngredientMap.fromDescription (Recipe.description recipe))

        descriptionView =
            Recipe.map
                (\recipePart ->
                    case recipePart of
                        Recipe.PlainPart text ->
                            Html.text text

                        Recipe.IngredientPart ingredient ->
                            Html.text (Recipe.getText ingredient)
                )
                recipe
                |> List.map (\paragraph -> p [] [] paragraph)
    in
    ( Just title
    , Html.div []
        [ Html.nav []
            [ backToOverview
            , toolbar
                [ linkButton "Edit" (EditRoute <| Recipe.title recipe)
                , button [] "Delete" (DeleteRecipe <| Recipe.title recipe)
                ]
            ]
        , Html.article
            []
            (h1 [] [] [ Html.text <| Recipe.title recipe ]
                :: details
                    []
                    [ h2
                        [ Css.display Css.inlineBlock
                        , Css.marginTop zero
                        ]
                        []
                        [ Html.text "Ingredients" ]
                    ]
                    [ Css.marginTop (rem 1) ]
                    [ Attributes.attribute "open" "" ]
                    [ ingredientsView
                    ]
                :: h2 [ headingStyle ] [] [ Html.text "Description" ]
                :: descriptionView
            )
        ]
    )


viewIngredientsList : IngredientMap -> Html Msg
viewIngredientsList ingredientsMap =
    let
        ingredients =
            ingredientsMap
                |> Dict.toList
                |> List.sortBy (\( name, _ ) -> name)
    in
    contentList
        (Html.text "No ingredients required.")
        (ul [] [])
        (\( name, quantities ) ->
            let
                descriptions =
                    quantities.descriptions

                amount =
                    Maybe.map (\a -> [ String.fromFloat a ]) quantities.amount
                        |> Maybe.withDefault []

                measures =
                    Dict.toList quantities.measures
                        |> List.map
                            (\( unit, unitAmount ) ->
                                String.fromFloat unitAmount ++ " " ++ unit
                            )

                quantitiesTexts =
                    measures ++ descriptions ++ amount

                quantitiesText =
                    if List.isEmpty quantitiesTexts then
                        ""

                    else
                        " (" ++ String.join " + " quantitiesTexts ++ ")"

                text =
                    name ++ quantitiesText
            in
            Html.li [] [ Html.text text ]
        )
        ingredients


viewEditRecipe : String -> Maybe String -> ( Maybe String, Html Msg )
viewEditRecipe code errors =
    ( Nothing
    , Html.div []
        [ Html.nav [] [ backToOverview ]
        , case errors of
            Just error ->
                Html.div [] [ Html.text error ]

            Nothing ->
                -- Do not remove this item. Otherwise, the textarea might lose focus while typing.
                Html.div [ css [ Css.display Css.none ] ] []
        , Html.textarea
            [ Events.onInput Edited
            , Attributes.value code
            , css
                [ Css.width (pct 100)
                , Css.maxWidth (pct 100)
                , Css.height (rem 20)
                ]
            ]
            []
        , button [] "Save" Save
        ]
    )


viewShoppingList : RecipeStore -> ShoppingList -> ShoppingState -> ( Maybe String, Html Msg )
viewShoppingList recipes shoppingList state =
    ( Nothing
    , let
        selectedRecipesView =
            [ let
                open =
                    if state.openSelection then
                        [ Attributes.attribute "open" "" ]

                    else
                        []

                selectedRecipes =
                    shoppingList.selectedRecipes |> Set.toList

                unselectedRecipes =
                    recipes |> Dict.keys |> List.filter (\title -> not <| Set.member title shoppingList.selectedRecipes)

                summaryStyles =
                    [ headingFontStyle ]
              in
              details
                summaryStyles
                [ Html.text "Selected recipes" ]
                [ Css.margin2 (rem 1) zero
                ]
                open
                [ details
                    summaryStyles
                    [ Html.text "Add recipes" ]
                    [ Css.marginLeft (rem 1)
                    , Css.marginTop (rem 1)
                    , Css.marginBottom (rem 1)
                    ]
                    open
                    [ contentList
                        (Html.div [] <|
                            if Dict.isEmpty recipes then
                                noRecipes

                            else
                                [ Html.text "You have selected all recipes." ]
                        )
                        (ul [ recipeListStyle ] [])
                        (\title ->
                            Html.li []
                                [ viewRecipeLink title
                                , smallButton [] "Add" (AddRecipeToShoppingList title)
                                ]
                        )
                        unselectedRecipes
                    ]
                , contentList
                    (Html.text "No recipes selected.")
                    (ul [ recipeListStyle ] [])
                    (\title ->
                        Html.li []
                            [ viewRecipeLink title
                            , smallButton [] "Remove" (RemoveRecipeFromShoppingList title)
                            ]
                    )
                    selectedRecipes
                ]
            ]

        allIngredients =
            (shoppingList.selectedRecipes
                |> Set.toList
                |> List.filterMap (\title -> Dict.get title recipes)
                |> List.concatMap (\( parts, _ ) -> Recipe.ingredients parts)
            )
                ++ shoppingList.extras

        ingredientsMap =
            IngredientMap.fromIngredients allIngredients

        ingredientsListView =
            if List.isEmpty allIngredients then
                [ Html.text "Your shopping list is empty. Add some recipes from the list."
                ]

            else
                [ viewIngredientsList ingredientsMap
                ]
      in
      Html.div []
        ([ Html.nav [] [ backToOverview ]
         , h1 [] [] [ Html.text "Shopping List" ]
         ]
            ++ selectedRecipesView
            ++ ingredientsListView
        )
    )


linkButton : String -> Route -> Html Msg
linkButton text route =
    styledNode
        Html.a
        [ buttonStyle, Css.display Css.inlineBlock ]
        [ Attributes.href (stringFromRoute route)
        ]
        [ Html.text text ]


button : List Css.Style -> String -> Msg -> Html Msg
button styles text msg =
    buttonWith
        (buttonStyle :: styles)
        [ Events.onClick msg
        ]
        [ Html.text text ]


smallButton : List Css.Style -> String -> Msg -> Html Msg
smallButton styles text msg =
    buttonWith
        (smallButtonStyle :: styles)
        [ Events.onClick msg
        ]
        [ Html.text text ]


onHover : List Css.Style -> Css.Style
onHover styles =
    Css.batch
        [ Css.hover styles
        , Css.focus styles
        ]


buttonWith : List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
buttonWith styles attributes children =
    styledNode
        Html.button
        styles
        attributes
        children


toolbar : List (Html Msg) -> Html Msg
toolbar items =
    Html.div
        [ css
            [ Css.margin2 (rem 1) zero
            , Css.displayFlex
            , Global.children
                [ Global.everything
                    [ Global.adjacentSiblings
                        [ Global.everything
                            [ Css.marginLeft (rem 0.5)
                            ]
                        ]
                    ]
                ]
            ]
        ]
        items


navLink : List Css.Style -> String -> Route -> Html Msg
navLink styles text route =
    styledNode
        Html.a
        ([ clickableStyle
         , Css.fontStyle Css.italic
         , linkUnstyle
         , Css.textDecoration Css.underline
         , Css.before [ Css.property "content" "\">> \"" ]
         ]
            ++ styles
        )
        [ Attributes.href (route |> stringFromRoute)
        ]
        [ Html.text text ]


backToOverview : Html Msg
backToOverview =
    navLink [ Css.before [ Css.property "content" "\"<< \"" ] ]
        "Go back to recipe list"
        OverviewRoute


contentList : Html Msg -> (List (Html Msg) -> Html Msg) -> (a -> Html Msg) -> List a -> Html Msg
contentList empty list itemFunc items =
    if List.isEmpty items then
        Html.span [ css [ Css.fontStyle Css.italic ] ] [ empty ]

    else
        list (List.map itemFunc items)


details : List Css.Style -> List (Html Msg) -> List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
details summaryStyles summary styles attributes children =
    styledNode
        Html.details
        (detailsStyle
            :: styles
        )
        attributes
        (Html.summary [ css <| [ clickableStyle ] ++ summaryStyles ] summary :: children)


ul : List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
ul styles attributes children =
    styledNode
        Html.ul
        ([ Css.margin zero
         , Css.paddingLeft (rem 1.5)
         ]
            ++ styles
        )
        attributes
        children


p : List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
p styles attributes children =
    styledNode
        Html.p
        ([ Css.margin zero
         , Global.adjacentSiblings
            [ Global.typeSelector "p"
                [ Css.marginTop (rem 0.6)
                ]
            ]
         ]
            ++ styles
        )
        attributes
        children


h1 : List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
h1 styles attributes children =
    styledNode
        Html.h1
        ([ headingStyle
         , Css.fontSize (rem 1.5)
         ]
            ++ styles
        )
        attributes
        children


h2 : List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
h2 styles attributes children =
    styledNode
        Html.h2
        ([ headingStyle
         , Css.fontSize (rem 1.3)
         ]
            ++ styles
        )
        attributes
        children


styledNode : (List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg) -> List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
styledNode tag styles attributes children =
    tag
        (attributes ++ [ css styles ])
        children


buttonStyle : Css.Style
buttonStyle =
    Css.batch
        [ borderStyle
        , clickableStyle
        , Css.boxShadow4 zero (rem 0.1) (rem 0.1) (Css.hsla 0 0 0 0.3)
        , onHover
            [ Css.backgroundColor (Css.hsla 0 0 0.5 0.1)
            , Css.boxShadow4 zero (rem 0.1) (rem 0.2) (Css.hsla 0 0 0 0.3)
            ]
        , headingFontStyle
        , Css.fontSize (rem 1)
        , linkUnstyle
        ]


smallButtonStyle : Css.Style
smallButtonStyle =
    Css.batch
        [ buttonStyle
        , Css.padding2 (rem 0.2) (rem 0.3)
        , bodyFontStyle
        ]


linkUnstyle : Css.Style
linkUnstyle =
    Css.batch
        [ Css.textDecoration Css.none
        , Css.color (Css.hsl 0 0 0)
        ]


borderStyle : Css.Style
borderStyle =
    Css.batch
        [ Css.padding2 (rem 0.5) (rem 1)
        , Css.borderRadius (rem 0.3)
        , Css.border3 (rem 0.1) Css.solid (Css.hsl 0 0 0)
        , Css.backgroundColor Css.transparent
        ]


headingStyle : Css.Style
headingStyle =
    Css.batch
        [ headingFontStyle
        , Css.lineHeight (num 1)
        , Css.margin zero
        , Css.marginTop (rem 0.6)
        , Css.marginBottom (rem 0.3)
        ]


headingFontStyle : Css.Style
headingFontStyle =
    Css.fontFamilies [ "Bree Serif", "Georgia", "serif" ]


bodyFontStyle : Css.Style
bodyFontStyle =
    Css.fontFamilies [ "Zilla Slab", "Palatino", "serif" ]


detailsStyle : Css.Style
detailsStyle =
    Css.batch
        [ Css.borderLeft3 (rem 0.1) Css.solid (Css.hsl 0 0 0)
        , Css.paddingLeft (rem 0.5)
        ]


clickableStyle : Css.Style
clickableStyle =
    Css.batch
        [ onHover
            [ Css.cursor Css.pointer
            ]
        ]
