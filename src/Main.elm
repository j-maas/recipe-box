port module Main exposing (main, p)

import Browser
import Browser.Navigation as Navigation
import Css exposing (auto, em, num, pct, rem, zero)
import Css.Global as Global
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import IngredientMap exposing (IngredientMap)
import Recipe exposing (Recipe)
import Set exposing (Set)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


main : Program (List String) Model Msg
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
    , recipes : RecipeStore
    , shoppingList : ShoppingList
    , state : State
    }


type alias ShoppingList =
    { selectedRecipes : Set String
    , extras : List Recipe.Ingredient
    }


type State
    = Overview
    | Recipe Recipe
    | Edit { code : String, error : Maybe String }
    | Shopping


type alias RecipeStore =
    Dict String ( Recipe.RecipeParts, String )


init : List String -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init rawRecipes url key =
    let
        recipes =
            List.filterMap
                (\code ->
                    Recipe.parse code
                        |> Result.toMaybe
                        |> Maybe.map (\recipe -> ( recipe, code ))
                )
                rawRecipes
                |> List.map (\( recipe, code ) -> ( Recipe.title recipe, ( Recipe.description recipe, code ) ))
                |> Dict.fromList
    in
    ( { key = key
      , recipes = recipes
      , shoppingList = { selectedRecipes = Set.empty, extras = [] }
      , state = parseRoute url |> stateFromRoute recipes |> Maybe.withDefault Overview
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
        toState route =
            stateFromRoute model.recipes route |> Maybe.withDefault model.state
    in
    case msg of
        DeleteRecipe title ->
            ( { model
                | recipes = Dict.remove title model.recipes
                , state = toState OverviewRoute
              }
            , delete title
            )

        Edited code ->
            let
                newState =
                    case model.state of
                        Edit _ ->
                            Edit { code = code, error = Nothing }

                        _ ->
                            model.state
            in
            ( { model | state = newState }, Cmd.none )

        Save ->
            case model.state of
                Edit { code } ->
                    case Recipe.parse code of
                        Ok recipe ->
                            let
                                title =
                                    Recipe.title recipe

                                parts =
                                    Recipe.description recipe
                            in
                            ( { model
                                | state = Recipe recipe
                                , recipes =
                                    Dict.insert title
                                        ( parts, code )
                                        model.recipes
                              }
                            , save { title = title, code = code }
                            )

                        Err error ->
                            ( { model | state = Edit { code = code, error = Just error } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AddRecipeToShoppingList title ->
            let
                oldShoppingList =
                    model.shoppingList

                newShoppingList =
                    { oldShoppingList | selectedRecipes = Set.insert title oldShoppingList.selectedRecipes }
            in
            ( { model | shoppingList = newShoppingList }, Cmd.none )

        RemoveRecipeFromShoppingList title ->
            let
                oldShoppingList =
                    model.shoppingList

                newShoppingList =
                    { oldShoppingList | selectedRecipes = Set.remove title oldShoppingList.selectedRecipes }
            in
            ( { model | shoppingList = newShoppingList }, Cmd.none )

        UrlChanged url ->
            ( { model | state = toState (parseRoute url) }, Cmd.none )

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


stateFromRoute : RecipeStore -> Route -> Maybe State
stateFromRoute recipes route =
    case route of
        OverviewRoute ->
            Just Overview

        RecipeRoute title ->
            Dict.get title recipes
                |> Maybe.map
                    (\( recipe, _ ) ->
                        Recipe (Recipe.from title recipe)
                    )

        NewRoute ->
            Just <| Edit { code = "", error = Nothing }

        EditRoute title ->
            Dict.get title recipes
                |> Maybe.map
                    (\( _, code ) ->
                        Edit { code = code, error = Nothing }
                    )

        ShoppingListRoute ->
            Just Shopping


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


port save : { title : String, code : String } -> Cmd msg


port delete : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Browser.Document Msg
view model =
    let
        ( maybeSubtitle, body ) =
            case model.state of
                Overview ->
                    viewOverview (Dict.keys model.recipes) model.shoppingList

                Recipe recipe ->
                    viewRecipe recipe

                Edit { code, error } ->
                    viewEditRecipe code error

                Shopping ->
                    viewShoppingList model.recipes model.shoppingList
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
                    , Css.maxWidth (em 48)
                    , Css.margin2 zero auto
                    , Css.marginTop (em 2)
                    ]
                ]
                [ body ]
            ]
    }


viewOverview : List String -> ShoppingList -> ( Maybe String, Html Msg )
viewOverview recipeTitles shoppingList =
    ( Nothing
    , Html.div []
        ([ h1 [] [] [ Html.text "Recipe Box" ]
         , Html.nav [] [ navLink [] "Go to shopping list" ShoppingListRoute ]
         , Html.div [ css [ Css.margin2 (em 1) zero ] ] [ linkButton "New recipe" NewRoute ]
         ]
            ++ (if List.isEmpty recipeTitles then
                    [ Html.text "You do not have any recipes yet. Create a "
                    , navLink [] "new recipe" NewRoute
                    , Html.text "!"
                    ]

                else
                    [ viewRecipeList recipeTitles shoppingList
                    ]
               )
        )
    )


viewRecipeList : List String -> ShoppingList -> Html Msg
viewRecipeList recipeTitles shoppingList =
    ul
        [ Css.property "list-style-type" "\">  \""
        , Css.listStylePosition Css.inside
        , Css.paddingLeft zero
        ]
        []
        (List.map
            (\recipeTitle ->
                Html.li
                    []
                    [ Html.a
                        [ css
                            [ clickableStyle
                            , linkUnstyle
                            , Css.display Css.inlineBlock
                            , Css.padding (em 0.5)
                            , Css.hover
                                [ Css.backgroundColor (Css.hsla 0 0 0.5 0.1)
                                ]
                            ]
                        , Attributes.href
                            (RecipeRoute recipeTitle |> stringFromRoute)
                        ]
                        [ Html.text recipeTitle
                        ]
                    , Html.div [ css [ Css.display Css.inlineBlock, Css.marginInlineStart (rem 1) ] ]
                        [ if Set.member recipeTitle shoppingList.selectedRecipes then
                            smallButton [] "Remove from shopping list" <| RemoveRecipeFromShoppingList recipeTitle

                          else
                            smallButton
                                [ Css.opacity (num 0.1)
                                , Css.hover [ Css.opacity (num 1) ]
                                ]
                                "Add to shopping list"
                            <|
                                AddRecipeToShoppingList recipeTitle
                        ]
                    ]
            )
            recipeTitles
        )


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
            , Html.div
                [ css
                    [ Css.margin2 (em 1) zero
                    , Css.displayFlex
                    , Css.alignItems Css.flexEnd
                    , Global.children
                        [ Global.everything
                            [ Global.adjacentSiblings
                                [ Global.everything
                                    [ Css.marginLeft (em 0.5)
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
                [ linkButton "Edit" (EditRoute <| Recipe.title recipe)
                , button [] "Delete" (DeleteRecipe <| Recipe.title recipe)
                ]
            ]
        , Html.article
            []
            (h1 [] [] [ Html.text <| Recipe.title recipe ]
                :: Html.details [ Attributes.attribute "open" "" ]
                    [ Html.summary [ css [ clickableStyle ] ]
                        [ h2
                            [ Css.display Css.inlineBlock
                            ]
                            []
                            [ Html.text "Ingredients" ]
                        ]
                    , ingredientsView
                    ]
                :: h2 [ headingStyle ] [] [ Html.text "Description" ]
                :: descriptionView
            )
        ]
    )


viewIngredientsList : IngredientMap -> Html Msg
viewIngredientsList ingredients =
    ingredients
        |> Dict.toList
        |> List.sortBy (\( name, _ ) -> name)
        |> List.map
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
        |> ul [] []


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
                , Css.height (em 20)
                ]
            ]
            []
        , button [] "Save" Save
        ]
    )


viewShoppingList : RecipeStore -> ShoppingList -> ( Maybe String, Html Msg )
viewShoppingList recipes shoppingList =
    ( Nothing
    , let
        selectedRecipesView =
            if Set.isEmpty shoppingList.selectedRecipes then
                []

            else
                [ Html.details
                    [ css
                        [ Css.margin2 (rem 1) zero
                        ]
                    ]
                    [ Html.summary [] [ Html.text "Recipes selected" ]
                    , Html.div
                        [ css [ Css.marginLeft (rem 0.5) ]
                        ]
                        [ viewRecipeList (shoppingList.selectedRecipes |> Set.toList) shoppingList ]
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
                [ Html.text "Your shopping list is empty. Select some recipes from the "
                , navLink [] "recipe list" OverviewRoute
                , Html.text "."
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
        [ buttonStyle ]
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


buttonWith : List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
buttonWith styles attributes children =
    styledNode
        Html.button
        styles
        attributes
        children


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


ul : List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
ul styles attributes children =
    styledNode
        Html.ul
        ([ Css.margin zero
         , Css.paddingLeft (em 1.5)
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
                [ Css.marginTop (em 0.6)
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
        , Css.hover
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
        , Css.marginTop (em 0.6)
        , Css.marginBottom (em 0.3)
        ]


headingFontStyle : Css.Style
headingFontStyle =
    Css.fontFamilies [ "Bree Serif", "Georgia", "serif" ]


bodyFontStyle : Css.Style
bodyFontStyle =
    Css.fontFamilies [ "Zilla Slab", "Palatino", "serif" ]


clickableStyle : Css.Style
clickableStyle =
    Css.batch
        [ Css.hover
            [ Css.cursor Css.pointer
            ]
        ]
