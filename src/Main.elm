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
import Language
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
    , language : Language
    , screen : Screen
    }


type alias Language =
    Language.Language (Html Msg)


type alias State =
    { recipes : RecipeStore
    , recipeChecks : Dict String (Set String)
    , shoppingList : ShoppingList
    }


type alias ShoppingList =
    { selectedRecipes : Set String
    , checked : Set String
    }


type Screen
    = Overview
    | Recipe Recipe
    | Edit { code : String, error : Maybe String }
    | Shopping


type alias RecipeStore =
    Dict String ( Recipe.Parts, String )


type alias Flags =
    { recipes : List String
    , recipeChecks : List ( String, List String )
    , shoppingList : PortShoppingList
    , language : String
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
                |> List.map (\( recipe, code ) -> ( Recipe.title recipe, ( Recipe.steps recipe, code ) ))
                |> Dict.fromList

        shoppingList =
            { selectedRecipes = flags.shoppingList.selectedRecipes |> Set.fromList
            , checked = flags.shoppingList.checked |> Set.fromList
            }

        state =
            { recipes = recipes
            , recipeChecks =
                List.map
                    (\( title, checks ) -> ( title, Set.fromList checks ))
                    flags.recipeChecks
                    |> Dict.fromList
            , shoppingList = shoppingList
            }
    in
    ( { key = key
      , state = state
      , language = Language.fromString flags.language
      , screen = parseRoute url |> screenFromRoute state |> Maybe.withDefault Overview
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | DeleteRecipe String
    | Edited String
    | Save
    | AddRecipeToShoppingList String
    | RemoveRecipeFromShoppingList String
    | UpdateCheckOnShoppingList String Bool
    | UpdateCheckOnRecipe String String Bool
    | SwitchLanguage String
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toScreen route =
            screenFromRoute model.state route |> Maybe.withDefault model.screen
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DeleteRecipe title ->
            let
                state =
                    model.state
            in
            ( { model
                | screen = toScreen OverviewRoute
                , state = { state | recipes = Dict.remove title state.recipes }
              }
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
                                state =
                                    model.state

                                title =
                                    Recipe.title recipe

                                parts =
                                    Recipe.steps recipe
                            in
                            ( { model
                                | screen = Recipe recipe
                                , state =
                                    { state
                                        | recipes =
                                            Dict.insert title
                                                ( parts, code )
                                                state.recipes
                                    }
                              }
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
            ( { model
                | state =
                    { state
                        | shoppingList = newShoppingList
                    }
              }
            , saveShoppingListCmd newShoppingList
            )

        RemoveRecipeFromShoppingList title ->
            let
                state =
                    model.state

                oldShoppingList =
                    model.state.shoppingList

                newSelectedRecipes =
                    Set.remove title oldShoppingList.selectedRecipes

                newIngredients =
                    ingredientsFromRecipes state.recipes newSelectedRecipes
                        |> List.map Recipe.getListName
                        |> Set.fromList

                newChecked =
                    Set.intersect newIngredients oldShoppingList.checked

                newShoppingList =
                    { oldShoppingList
                        | selectedRecipes = newSelectedRecipes
                        , checked = newChecked
                    }
            in
            ( { model
                | state =
                    { state | shoppingList = newShoppingList }
              }
            , saveShoppingListCmd newShoppingList
            )

        UpdateCheckOnShoppingList ingredientName checked ->
            let
                state =
                    model.state

                oldShoppingList =
                    model.state.shoppingList

                operation =
                    if checked then
                        Set.insert

                    else
                        Set.remove

                newShoppingList =
                    { oldShoppingList | checked = operation ingredientName oldShoppingList.checked }
            in
            ( { model
                | state =
                    { state | shoppingList = newShoppingList }
              }
            , saveShoppingListCmd newShoppingList
            )

        UpdateCheckOnRecipe recipeTitle ingredientName checked ->
            let
                state =
                    model.state

                operation =
                    if checked then
                        Set.insert

                    else
                        Set.remove

                oldChecks =
                    Dict.get recipeTitle state.recipeChecks
                        |> Maybe.withDefault Set.empty

                newChecks =
                    operation ingredientName oldChecks

                newRecipeChecks =
                    state.recipeChecks
                        |> Dict.insert recipeTitle newChecks
            in
            ( { model
                | state =
                    { state | recipeChecks = newRecipeChecks }
              }
            , saveRecipeChecksCmd recipeTitle newChecks
            )

        SwitchLanguage code ->
            ( { model
                | language =
                    Dict.get code Language.available
                        |> Maybe.map .content
                        |> Maybe.withDefault model.language
              }
            , saveLanguage code
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


port saveRecipe : { title : String, code : String } -> Cmd msg


saveRecipeChecksCmd : String -> Set String -> Cmd msg
saveRecipeChecksCmd recipeTitle checks =
    saveRecipeChecks
        { title = recipeTitle
        , checks = checks |> Set.toList
        }


port saveRecipeChecks : { title : String, checks : List String } -> Cmd msg


port removeRecipe : String -> Cmd msg


saveShoppingListCmd : ShoppingList -> Cmd msg
saveShoppingListCmd shoppingList =
    let
        selectedRecipes =
            shoppingList.selectedRecipes
                |> Set.toList

        checked =
            shoppingList.checked |> Set.toList
    in
    saveShoppingList
        { selectedRecipes = selectedRecipes
        , checked = checked
        }


type alias PortShoppingList =
    { selectedRecipes : List String
    , checked : List String
    }


port saveShoppingList : PortShoppingList -> Cmd msg


port saveLanguage : String -> Cmd msg


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
                    viewOverview model.language (Dict.keys state.recipes)

                Recipe recipe ->
                    let
                        title =
                            Recipe.title recipe

                        recipeChecks =
                            Dict.get title model.state.recipeChecks
                    in
                    viewRecipe model.language recipe recipeChecks

                Edit { code, error } ->
                    viewEditRecipe model.language code error

                Shopping ->
                    viewShoppingList model.language state.recipes state.shoppingList
    in
    { title =
        model.language.title
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


viewOverview : Language -> List String -> ( Maybe String, Html Msg )
viewOverview language recipeTitles =
    ( Nothing
    , Html.div []
        [ h1 [] [] [ Html.text language.title ]
        , Html.nav [ css [ Css.displayFlex, Css.justifyContent Css.spaceBetween ] ]
            [ Html.div [] [ navLink [] language.shoppingList.title ShoppingListRoute ]
            , Html.div [] [ languagePicker language ]
            ]
        , toolbar [ linkButton language.overview.newRecipe NewRoute ]
        , viewRecipeList language recipeTitles
        ]
    )


languagePicker : Language -> Html Msg
languagePicker currentLanguage =
    Html.select
        [ Events.onInput SwitchLanguage
        , css [ borderStyle, bodyFontStyle ]
        ]
        (Language.available
            |> Dict.toList
            |> List.map
                (\( code, language ) ->
                    Html.option
                        (Attributes.value code
                            :: (if language.content == currentLanguage then
                                    [ Attributes.selected True ]

                                else
                                    []
                               )
                        )
                        [ Html.text language.name ]
                )
        )


viewRecipeList : Language -> List String -> Html Msg
viewRecipeList language recipeTitles =
    contentList
        (noRecipes language)
        (ul
            [ recipeListStyle ]
            []
        )
        (\title ->
            Html.li [] [ viewRecipeLink title ]
        )
        recipeTitles


noRecipes : Language -> List (Html Msg)
noRecipes language =
    language.noRecipes Html.text (\text -> navLink [] text NewRoute)


recipeListStyle : Css.Style
recipeListStyle =
    Css.batch
        [ Css.property "list-style-type" "\">  \""
        , Css.paddingLeft (rem 1)
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
        , Css.display Css.inlineFlex
        , Css.padding (rem 0.5)
        , borderStyle
        , Css.borderColor Css.transparent
        , onHover
            [ Css.backgroundColor (Css.hsla 0 0 0.5 0.1)
            , Css.textDecoration Css.underline
            ]
        ]


viewRecipe : Language -> Recipe -> Maybe (Set String) -> ( Maybe String, Html Msg )
viewRecipe language recipe recipeChecks =
    let
        title =
            Recipe.title recipe

        ingredientMap =
            IngredientMap.fromDescription <| Recipe.steps recipe

        ingredientsView =
            viewIngredientList
                [ Html.text language.recipe.noIngredientsRequired ]
                ingredientMap
                (recipeChecks |> Maybe.withDefault Set.empty)
                (UpdateCheckOnRecipe title)

        stepsView =
            Recipe.map
                (\recipePart ->
                    case recipePart of
                        Recipe.PlainPart text ->
                            Html.text text

                        Recipe.IngredientPart ingredient ->
                            let
                                quantityText =
                                    case Recipe.getQuantity ingredient of
                                        Just quantity ->
                                            " ("
                                                ++ (case quantity of
                                                        Recipe.Description description ->
                                                            description

                                                        Recipe.Amount amount ->
                                                            String.fromFloat amount

                                                        Recipe.Measure amount unit ->
                                                            String.fromFloat amount ++ " " ++ unit
                                                   )
                                                ++ ")"

                                        Nothing ->
                                            ""
                            in
                            Html.text (Recipe.getText ingredient ++ quantityText)
                )
                recipe
                |> List.map (\paragraph -> p [] [] paragraph)
    in
    ( Just title
    , Html.div []
        [ Html.nav []
            [ backToOverview language
            , toolbar
                [ linkButton language.recipe.edit (EditRoute <| Recipe.title recipe)
                , button [] language.recipe.delete (DeleteRecipe <| Recipe.title recipe)
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
                        [ let
                            ingredientsText =
                                language.recipe.ingredientsWithCount (Dict.size ingredientMap)
                          in
                          Html.text ingredientsText
                        ]
                    ]
                    [ Css.marginTop (rem 1) ]
                    [ Attributes.attribute "open" "" ]
                    [ ingredientsView
                    ]
                :: h2 [ headingStyle ] [] [ Html.text language.recipe.steps ]
                :: stepsView
            )
        ]
    )


viewIngredientList : List (Html Msg) -> IngredientMap -> Set String -> (String -> Bool -> Msg) -> Html Msg
viewIngredientList empty ingredientsMap selected msg =
    let
        ingredients =
            ingredientsMap
                |> Dict.toList
                |> List.sortBy (\( name, _ ) -> name)
    in
    contentList
        empty
        (ul [ Css.listStyleType Css.none, Css.paddingLeft zero ] [])
        (\( name, quantities ) -> Html.li [] [ checkbox (viewIngredient ( name, quantities )) name selected msg ])
        ingredients


checkbox : Html Msg -> String -> Set String -> (String -> Bool -> Msg) -> Html Msg
checkbox label value set msg =
    let
        checked =
            Set.member value set
    in
    Html.label
        [ css
            ([ Css.display Css.inlineFlex
             , Css.alignItems Css.center
             ]
                ++ (if checked then
                        [ Css.textDecoration Css.lineThrough ]

                    else
                        []
                   )
            )
        ]
        [ Html.input
            [ Attributes.type_ "checkbox"
            , Attributes.value value
            , Attributes.checked checked
            , Events.onCheck (msg value)
            , css [ Css.marginRight (rem 0.5) ]
            ]
            []
        , label
        ]


viewIngredient : ( String, IngredientMap.Quantities ) -> Html Msg
viewIngredient ( name, quantities ) =
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
    Html.text text


viewEditRecipe : Language -> String -> Maybe String -> ( Maybe String, Html Msg )
viewEditRecipe language code errors =
    ( Nothing
    , Html.div []
        [ Html.nav [] [ backToOverview language ]
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
        , button [] language.editRecipe.save Save
        ]
    )


viewShoppingList : Language -> RecipeStore -> ShoppingList -> ( Maybe String, Html Msg )
viewShoppingList language recipes shoppingList =
    ( Nothing
    , let
        selectedRecipesView =
            [ let
                selectedRecipes =
                    shoppingList.selectedRecipes |> Set.toList

                unselectedRecipes =
                    recipes
                        |> Dict.keys
                        |> List.filter (\title -> not <| Set.member title shoppingList.selectedRecipes)

                summaryStyles =
                    [ headingFontStyle ]

                selectedRecipesText =
                    language.shoppingList.selectedRecipesWithCount <| List.length selectedRecipes
              in
              details
                summaryStyles
                [ Html.text selectedRecipesText ]
                [ Css.margin2 (rem 1) zero
                ]
                []
                [ details
                    summaryStyles
                    [ Html.text
                        (language.shoppingList.addRecipesWithCount <| List.length unselectedRecipes)
                    ]
                    [ Css.marginLeft (rem 0.5)
                    , Css.marginTop (rem 1)
                    , Css.marginBottom (rem 1)
                    ]
                    []
                    [ contentList
                        (if Dict.isEmpty recipes then
                            noRecipes language

                         else
                            [ Html.text language.shoppingList.allRecipesSelected ]
                        )
                        (ul [ recipeListStyle ] [])
                        (\title ->
                            Html.li []
                                [ viewRecipeLink title
                                , smallButton [ Css.marginLeft (rem 0.5) ] language.shoppingList.add (AddRecipeToShoppingList title)
                                ]
                        )
                        unselectedRecipes
                    ]
                , contentList
                    [ Html.text language.shoppingList.noRecipeSelected ]
                    (ul [ recipeListStyle ] [])
                    (\title ->
                        Html.li []
                            [ viewRecipeLink title
                            , smallButton [ Css.marginLeft (rem 0.5) ] language.shoppingList.remove (RemoveRecipeFromShoppingList title)
                            ]
                    )
                    selectedRecipes
                ]
            ]

        allIngredients =
            ingredientsFromRecipes recipes shoppingList.selectedRecipes

        ingredientsListView =
            viewIngredientList
                [ Html.text language.shoppingList.emptyShoppingList ]
                (IngredientMap.fromIngredients allIngredients)
                shoppingList.checked
                UpdateCheckOnShoppingList
      in
      Html.div []
        ([ Html.nav [] [ backToOverview language ]
         , h1 [] [] [ Html.text language.shoppingList.title ]
         ]
            ++ selectedRecipesView
            ++ [ ingredientsListView ]
        )
    )


ingredientsFromRecipes : RecipeStore -> Set String -> List Recipe.Ingredient
ingredientsFromRecipes recipes selectedRecipes =
    selectedRecipes
        |> Set.toList
        |> List.filterMap (\title -> Dict.get title recipes)
        |> List.concatMap (\( parts, _ ) -> Recipe.ingredients parts)


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


backToOverview : Language -> Html Msg
backToOverview language =
    navLink [ Css.before [ Css.property "content" "\"<< \"" ] ]
        language.goToOverview
        OverviewRoute


contentList : List (Html Msg) -> (List (Html Msg) -> Html Msg) -> (a -> Html Msg) -> List a -> Html Msg
contentList empty list itemFunc items =
    if List.isEmpty items then
        Html.span [ css [ Css.fontStyle Css.italic ] ] empty

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
        [ framedStyle
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


framedStyle : Css.Style
framedStyle =
    Css.batch
        [ Css.padding2 (rem 0.5) (rem 1)
        , borderStyle
        ]


borderStyle : Css.Style
borderStyle =
    Css.batch
        [ Css.borderRadius (rem 0.3)
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
