port module Main exposing (main, p)

import Browser
import Browser.Navigation as Navigation
import Css exposing (auto, num, pct, rem, zero)
import Css.Global as Global
import Dict exposing (Dict)
import Embed.Youtube
import Embed.Youtube.Attributes
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Ingredient exposing (Ingredient)
import IngredientMap exposing (IngredientMap)
import Language
import List.Extra as List
import Recipe exposing (Recipe)
import RecipeParser
import Set exposing (Set)
import TypedUrl
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
    , wakeVideoId : String
    , nextCloud : Maybe NextCloudState
    }


type alias NextCloudState =
    { server : String
    , credentials :
        Maybe
            { user : String
            , appPassword : String
            }
    }


type alias ShoppingList =
    { selectedRecipes : Set String
    , checked : Set String
    }


type Screen
    = Overview
    | Recipe Recipe { showWakeVideo : Bool }
    | Edit { code : String, failure : Maybe RecipeParser.Failure }
    | Shopping
    | Settings SettingsState


type alias SettingsState =
    { wakeVideoIdField : String
    , wakeVideoIdError : Bool
    , nextCloud :
        { serverField : String
        , error : Bool
        , user : Maybe String
        }
    }


type alias RecipeStore =
    Dict String ( Recipe.Parts, String )


type alias Flags =
    { recipes : List String
    , recipeChecks : List ( String, List String )
    , shoppingList : PortShoppingList
    , settings : Maybe JsonSettings
    , language : String
    }


type alias JsonSettings =
    { wakeVideoId : Maybe String
    , nextCloud :
        Maybe
            { server : String
            , credentials :
                Maybe
                    { user : String
                    , appPassword : String
                    }
            }
    }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        recipes =
            List.filterMap
                (\code ->
                    RecipeParser.parse code
                        |> Result.toMaybe
                        |> Maybe.map (\recipe -> ( recipe, code ))
                )
                flags.recipes
                |> List.map (\( recipe, code ) -> ( Recipe.title recipe, ( Recipe.method recipe, code ) ))
                |> Dict.fromList

        shoppingList =
            { selectedRecipes = flags.shoppingList.selectedRecipes |> Set.fromList
            , checked = flags.shoppingList.checked |> Set.fromList
            }

        settings =
            flags.settings
                |> Maybe.withDefault
                    { wakeVideoId = Nothing
                    , nextCloud = Nothing
                    }

        nextCloud =
            settings.nextCloud

        state =
            { recipes = recipes
            , recipeChecks =
                List.map
                    (\( title, checks ) -> ( title, Set.fromList checks ))
                    flags.recipeChecks
                    |> Dict.fromList
            , shoppingList = shoppingList
            , wakeVideoId = settings.wakeVideoId |> Maybe.withDefault "14Cf79j92xA"
            , nextCloud = nextCloud
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
    = DeleteRecipe String
    | Edited String
    | Save
    | AddRecipeToShoppingList String
    | RemoveRecipeFromShoppingList String
    | UpdateCheckOnShoppingList String Bool
    | ClearShoppingList
    | UpdateCheckOnRecipe String String Bool
    | ClearRecipeChecks String
    | ToggleVideo
    | SetWakeVideoUrl String
    | SetNextCloudServer String
    | SwitchLanguage String
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toScreen route =
            screenFromRoute model.state route |> Maybe.withDefault model.screen

        goTo route =
            Navigation.pushUrl model.key (stringFromRoute route)
    in
    case msg of
        DeleteRecipe title ->
            let
                state =
                    model.state
            in
            ( { model
                | state = { state | recipes = Dict.remove title state.recipes }
              }
            , Cmd.batch
                [ removeRecipe title
                , goTo OverviewRoute
                ]
            )

        Edited code ->
            let
                newScreen =
                    case model.screen of
                        Edit _ ->
                            Edit { code = code, failure = Nothing }

                        _ ->
                            model.screen
            in
            ( { model | screen = newScreen }, Cmd.none )

        Save ->
            case model.screen of
                Edit { code } ->
                    case RecipeParser.parse code of
                        Ok recipe ->
                            let
                                state =
                                    model.state

                                title =
                                    Recipe.title recipe

                                parts =
                                    Recipe.method recipe
                            in
                            ( { model
                                | state =
                                    { state
                                        | recipes =
                                            Dict.insert title
                                                ( parts, code )
                                                state.recipes
                                    }
                              }
                            , Cmd.batch
                                [ saveRecipe { title = title, code = code }
                                , goTo (RecipeRoute title)
                                ]
                            )

                        Err failure ->
                            ( { model
                                | screen =
                                    Edit
                                        { code = code
                                        , failure = Just failure
                                        }
                              }
                            , Cmd.none
                            )

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
                        |> List.map Ingredient.listText
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
                    state.shoppingList

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

        ClearShoppingList ->
            let
                state =
                    model.state

                oldShoppingList =
                    state.shoppingList

                newShoppingList =
                    { oldShoppingList | checked = Set.empty }
            in
            ( { model | state = { state | shoppingList = newShoppingList } }
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

        ClearRecipeChecks title ->
            let
                state =
                    model.state

                newRecipeChecks =
                    state.recipeChecks
                        |> Dict.remove title
            in
            ( { model
                | state =
                    { state | recipeChecks = newRecipeChecks }
              }
            , saveRecipeChecksCmd title Set.empty
            )

        ToggleVideo ->
            let
                newScreen =
                    case model.screen of
                        Recipe recipe options ->
                            Recipe recipe { options | showWakeVideo = not options.showWakeVideo }

                        _ ->
                            model.screen
            in
            ( { model | screen = newScreen }
            , Cmd.none
            )

        SetWakeVideoUrl raw ->
            let
                state =
                    model.state

                parsed =
                    TypedUrl.parse raw
                        |> Maybe.andThen
                            (\url ->
                                -- https://www.youtube.com/watch?v=14Cf79j92xA&feature=emb_title
                                case List.find (\( query, _ ) -> query == "v") url.query of
                                    Just ( _, id ) ->
                                        Just id

                                    Nothing ->
                                        -- https://youtube.com/embed/14Cf79j92xA
                                        -- https://youtu.be/14Cf79j92xA
                                        List.last url.path
                            )
                        |> Maybe.withDefault raw

                maybeId =
                    if
                        (String.length parsed > 0)
                            && String.all (\c -> Char.isAlphaNum c || c == '_' || c == '-') parsed
                    then
                        Just parsed

                    else
                        Nothing

                ( newModel, cmd ) =
                    case maybeId of
                        Just id ->
                            let
                                newState =
                                    { state | wakeVideoId = id }
                            in
                            ( { model
                                | state = newState
                                , screen =
                                    case model.screen of
                                        Settings s ->
                                            Settings
                                                { s
                                                    | wakeVideoIdError = False
                                                    , wakeVideoIdField = raw
                                                }

                                        _ ->
                                            model.screen
                              }
                            , saveSettingsCmd newState
                            )

                        Nothing ->
                            ( { model
                                | screen =
                                    case model.screen of
                                        Settings s ->
                                            Settings
                                                { s
                                                    | wakeVideoIdError = True
                                                    , wakeVideoIdField = raw
                                                }

                                        _ ->
                                            model.screen
                              }
                            , Cmd.none
                            )
            in
            ( newModel, cmd )

        SetNextCloudServer server ->
            let
                state =
                    model.state

                newServer =
                    TypedUrl.parse server
                        |> Maybe.map .authority
                        |> Maybe.map (String.join ".")
            in
            case newServer of
                Just authority ->
                    let
                        newNextCloud =
                            case state.nextCloud of
                                Just nc ->
                                    { nc | server = authority }

                                Nothing ->
                                    { server = authority, credentials = Nothing }

                        newState =
                            { state
                                | nextCloud = Just newNextCloud
                            }

                        newScreen =
                            case model.screen of
                                Settings s ->
                                    let
                                        oldNextCloud =
                                            s.nextCloud
                                    in
                                    Settings
                                        { s
                                            | nextCloud =
                                                { oldNextCloud
                                                    | serverField = server
                                                    , error = False
                                                }
                                        }

                                _ ->
                                    model.screen
                    in
                    ( { model
                        | state = newState
                        , screen = newScreen
                      }
                    , saveSettingsCmd newState
                    )

                Nothing ->
                    let
                        newScreen =
                            case model.screen of
                                Settings s ->
                                    let
                                        oldNextCloud =
                                            s.nextCloud
                                    in
                                    Settings
                                        { s
                                            | nextCloud =
                                                { oldNextCloud
                                                    | serverField = server
                                                    , error = True
                                                }
                                        }

                                _ ->
                                    model.screen
                    in
                    ( { model | screen = newScreen }, Cmd.none )

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
    | SettingsRoute


screenFromRoute : State -> Route -> Maybe Screen
screenFromRoute state route =
    case route of
        OverviewRoute ->
            Just Overview

        RecipeRoute title ->
            Dict.get title state.recipes
                |> Maybe.map
                    (\( recipe, _ ) ->
                        Recipe (Recipe.from title recipe) { showWakeVideo = False }
                    )

        NewRoute ->
            Just <| Edit { code = "", failure = Nothing }

        EditRoute title ->
            Dict.get title state.recipes
                |> Maybe.map
                    (\( _, code ) ->
                        Edit { code = code, failure = Nothing }
                    )

        ShoppingListRoute ->
            Just Shopping

        SettingsRoute ->
            let
                nextCloud =
                    state.nextCloud
                        |> Maybe.map
                            (\nc ->
                                { serverField = "https://" ++ nc.server
                                , error = False
                                , user = nc.credentials |> Maybe.map .user
                                }
                            )
                        |> Maybe.withDefault
                            { serverField = ""
                            , error = False
                            , user = Nothing
                            }
            in
            Just <|
                Settings
                    { wakeVideoIdError = False
                    , wakeVideoIdField = "https://youtu.be/" ++ state.wakeVideoId
                    , nextCloud = nextCloud
                    }


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

        SettingsRoute ->
            "#settings"


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

                else if raw == "settings" then
                    Just SettingsRoute

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


saveSettingsCmd : State -> Cmd msg
saveSettingsCmd state =
    saveSettings
        { wakeVideoId = Just state.wakeVideoId
        , nextCloud = state.nextCloud
        }


port saveSettings : JsonSettings -> Cmd msg


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

                Recipe recipe options ->
                    let
                        title =
                            Recipe.title recipe

                        recipeChecks =
                            Dict.get title model.state.recipeChecks
                    in
                    viewRecipe model.language
                        recipe
                        { maybeChecks = recipeChecks
                        , showVideo = options.showWakeVideo
                        , videoId =
                            state.wakeVideoId
                        }

                Edit { code, failure } ->
                    viewEditRecipe model.language code failure

                Shopping ->
                    viewShoppingList model.language state.recipes state.shoppingList

                Settings s ->
                    viewSettings model.language s state.wakeVideoId
    in
    { title =
        model.language.title
            ++ (maybeSubtitle
                    |> Maybe.map (\subtitle -> ": " ++ subtitle)
                    |> Maybe.withDefault ""
               )
    , body =
        List.map
            Html.toUnstyled
            [ Global.global [ Global.html [ Css.fontSize (pct 115) ] ]
            , Html.main_
                [ css
                    [ bodyFontStyle
                    , Css.lineHeight (num 1.4)
                    , Css.maxWidth (rem 48)
                    , Css.margin2 zero auto
                    ]
                ]
                [ body ]
            ]
    }


viewOverview : Language -> List String -> ( Maybe String, Html Msg )
viewOverview language recipeTitles =
    ( Nothing
    , Html.div []
        [ Html.div [ css [ toolbarSpacingStyle, Css.marginBottom (rem 1) ] ] [ languagePicker language, navLink [] language.overview.goToSettings SettingsRoute ]
        , h1 [] [] [ Html.text language.title ]
        , Html.nav [ css [ Css.displayFlex, Css.justifyContent Css.spaceBetween ] ]
            [ navLink [] language.overview.goToShoppingList ShoppingListRoute
            ]
        , toolbar [ linkButton language.overview.newRecipe NewRoute ]
        , viewRecipeList language recipeTitles
        ]
    )


languagePicker : Language -> Html Msg
languagePicker currentLanguage =
    Html.select
        [ Events.onInput SwitchLanguage
        , css [ borderStyle, bodyFontStyle, Css.fontSize Css.inherit ]
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


type alias RecipeViewOptions =
    { maybeChecks : Maybe (Set String)
    , showVideo : Bool
    , videoId : String
    }


viewRecipe : Language -> Recipe -> RecipeViewOptions -> ( Maybe String, Html Msg )
viewRecipe language recipe options =
    let
        title =
            Recipe.title recipe

        ingredientMap =
            Recipe.method recipe
                |> Recipe.ingredients
                |> IngredientMap.fromIngredients

        checks =
            options.maybeChecks |> Maybe.withDefault Set.empty

        ingredientsView =
            viewIngredientList
                [ Html.text language.recipe.noIngredientsRequired ]
                ingredientMap
                checks
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
                                    case Ingredient.quantity ingredient of
                                        Just quantity ->
                                            " ("
                                                ++ (case quantity of
                                                        Ingredient.Description description ->
                                                            description

                                                        Ingredient.Amount amount ->
                                                            String.fromFloat amount

                                                        Ingredient.Measure amount unit ->
                                                            String.fromFloat amount ++ " " ++ unit
                                                   )
                                                ++ ")"

                                        Nothing ->
                                            ""
                            in
                            Html.text (Ingredient.text ingredient ++ quantityText)
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
            , details [ Css.marginTop (rem 1), Css.marginBottom (rem 1) ]
                []
                { summary = ( [], [ Html.text language.recipe.moreOptions ] )
                , children =
                    [ Html.div [ css [ Css.marginTop (rem 0.5) ] ]
                        ([ p [] [] [ Html.text language.recipe.wakeVideoDescription ]
                         , toolbar
                            [ button []
                                (language.recipe.wakeVideoToggle options.showVideo)
                                ToggleVideo
                            ]
                         ]
                            ++ (if options.showVideo then
                                    [ viewVideo options.videoId
                                    ]

                                else
                                    []
                               )
                        )
                    ]
                }
            ]
        , Html.article
            []
            (h1 [] [] [ Html.text <| Recipe.title recipe ]
                :: details
                    [ Css.marginTop (rem 1) ]
                    [ Attributes.attribute "open" "" ]
                    { summary =
                        ( []
                        , [ h2
                                [ Css.display Css.inlineBlock
                                , Css.marginTop zero
                                ]
                                []
                                [ let
                                    ingredientsText =
                                        language.recipe.ingredients
                                  in
                                  Html.text ingredientsText
                                ]
                          ]
                        )
                    , children =
                        ingredientsView
                            :: (if Set.isEmpty checks then
                                    []

                                else
                                    [ toolbar
                                        [ smallButton []
                                            language.clearChecks
                                            (ClearRecipeChecks title)
                                        ]
                                    ]
                               )
                    }
                :: h2 [ headingStyle ] [] [ Html.text language.recipe.method ]
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
                |> List.sortBy (\( name, _ ) -> String.toLower name)
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


viewVideo : String -> Html Msg
viewVideo id =
    Html.div [ css [ Css.display Css.block ] ]
        [ Embed.Youtube.fromString id
            |> Embed.Youtube.attributes
                [ Embed.Youtube.Attributes.loop
                ]
            |> Embed.Youtube.toHtml
            |> Html.fromUnstyled
        ]


viewEditRecipe : Language -> String -> Maybe RecipeParser.Failure -> ( Maybe String, Html Msg )
viewEditRecipe language code maybeFailure =
    ( Nothing
    , Html.div []
        [ Html.nav [] [ backToOverview language ]
        , case maybeFailure of
            Just failure ->
                Html.div
                    [ css
                        [ framedStyle
                        , Css.borderColor (Css.rgb 255 0 0)
                        , Css.borderWidth (rem 0.5)
                        ]
                    ]
                    [ p []
                        []
                        [ Html.text <| language.editRecipe.problemCount (List.length failure) ]
                    , ul []
                        []
                        (List.map
                            (\deadEnd ->
                                Html.li
                                    []
                                    [ Html.text <|
                                        language.editRecipe.explainDeadEnd deadEnd
                                    ]
                            )
                            failure
                        )
                    ]

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
                [ Css.margin2 (rem 1) zero
                ]
                []
                { summary =
                    ( summaryStyles
                    , [ Html.text selectedRecipesText ]
                    )
                , children =
                    [ details
                        [ Css.marginLeft (rem 0.5)
                        , Css.marginTop (rem 1)
                        , Css.marginBottom (rem 1)
                        ]
                        []
                        { summary =
                            ( summaryStyles
                            , [ Html.text
                                    (language.shoppingList.addRecipesWithCount <| List.length unselectedRecipes)
                              ]
                            )
                        , children =
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
                        }
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
                }
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
        ([ Html.nav []
            [ backToOverview language
            , toolbar
                [ button [] language.clearChecks ClearShoppingList
                ]
            ]
         , h1 [] [] [ Html.text language.shoppingList.title ]
         ]
            ++ selectedRecipesView
            ++ [ ingredientsListView ]
        )
    )


ingredientsFromRecipes : RecipeStore -> Set String -> List Ingredient
ingredientsFromRecipes recipes selectedRecipes =
    selectedRecipes
        |> Set.toList
        |> List.filterMap (\title -> Dict.get title recipes)
        |> List.concatMap (\( parts, _ ) -> Recipe.ingredients parts)


viewSettings : Language -> SettingsState -> String -> ( Maybe String, Html Msg )
viewSettings language state videoId =
    ( Nothing
    , let
        wakeVideoError =
            if state.wakeVideoIdError then
                Just language.settings.videoUrlInvalid

            else
                Nothing

        wakeVideoSetting =
            Html.div []
                [ textInput language.settings.videoUrlLabel state.wakeVideoIdField SetWakeVideoUrl wakeVideoError
                , viewVideo videoId
                ]

        nextCloudSetting =
            let
                error =
                    if state.nextCloud.error then
                        Just "The URL is invalid."

                    else
                        Nothing
            in
            textInput "NextCloud Server" state.nextCloud.serverField SetNextCloudServer error
      in
      Html.div []
        [ Html.nav []
            [ backToOverview language ]
        , h1 [] [] [ Html.text language.settings.title ]
        , Html.div
            [ css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Global.children
                    [ Global.everything
                        [ Global.adjacentSiblings
                            [ Global.everything
                                [ Css.marginTop (rem 1)
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            [ wakeVideoSetting, nextCloudSetting ]
        ]
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
            , toolbarSpacingStyle
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


details :
    List Css.Style
    -> List (Html.Attribute Msg)
    ->
        { summary : ( List Css.Style, List (Html Msg) )
        , children : List (Html Msg)
        }
    -> Html Msg
details styles attributes options =
    let
        ( summaryStyles, summary ) =
            options.summary
    in
    styledNode
        Html.details
        (detailsStyle
            :: styles
        )
        attributes
        (Html.summary [ css <| [ clickableStyle ] ++ summaryStyles ] summary
            :: options.children
        )


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


textInput : String -> String -> (String -> Msg) -> Maybe String -> Html Msg
textInput label value onInput maybeError =
    Html.label [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.alignItems Css.start ] ]
        (Html.text label
            :: (case maybeError of
                    Just error ->
                        [ Html.span [ css [ Css.color (Css.rgb 255 0 0) ] ] [ Html.text error ] ]

                    Nothing ->
                        -- Do note remove this element, or the input will lose focus.
                        [ Html.span [ css [ Css.display Css.none ] ] [] ]
               )
            ++ [ Html.input
                    [ Attributes.type_ "text"
                    , Attributes.value value
                    , Events.onInput onInput
                    , css
                        [ bodyFontStyle
                        , Css.fontSize Css.inherit
                        , Css.width (rem 20)
                        , Css.padding (rem 0.5)
                        ]
                    ]
                    []
               ]
        )


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
        , bodyFontStyle
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


toolbarSpacingStyle : Css.Style
toolbarSpacingStyle =
    Css.batch
        [ Css.displayFlex
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
        , Css.border3 (rem 0.075) Css.solid (Css.hsl 0 0 0)
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
