port module Main exposing (main, p)

import Browser
import Browser.Navigation as Navigation
import Css exposing (auto, num, pct, rem, zero)
import Css.Global as Global
import Dict exposing (Dict)
import Dropbox
import Embed.Youtube
import Embed.Youtube.Attributes
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Ingredient exposing (Ingredient)
import IngredientMap exposing (IngredientMap)
import Json.Decode as Decode
import Language
import List.Extra as List
import Recipe exposing (Recipe)
import RecipeParser
import Set exposing (Set)
import Task
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
    , language : Language
    , state : State
    , screen : Screen
    }


type alias Language =
    Language.Language (Html Msg)


type alias State =
    { recipes : RecipeStore
    , shoppingList : ShoppingList
    , wakeVideoId : String
    , dropbox : Maybe Credentials
    , nonce : Maybe String
    }


type alias RecipeStore =
    Dict String RecipeEntry


type alias RecipeEntry =
    { method : Recipe.Parts
    , code : String
    , checks : Set String
    , revision : Revision
    }


type Revision
    = NewRevision
    | ChangedRevision String
    | SyncedRevision String


codeFromRevision : Revision -> Maybe String
codeFromRevision revision =
    case revision of
        NewRevision ->
            Nothing

        ChangedRevision code ->
            Just code

        SyncedRevision code ->
            Just code


type Credentials
    = LoggedIn Dropbox.UserAuth
    | CredentialsError


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
    , dropboxError : Bool
    }


type alias Flags =
    { localRecipes :
        List
            { code : String
            , checks : List String
            }
    , dropboxRecipes :
        List
            { code : String
            , rev : String
            , checks : List String
            }
    , shoppingList : PortShoppingList
    , settings : Maybe JsonSettings
    , language : String
    , nonce : Maybe String
    }


type alias JsonSettings =
    { wakeVideoId : Maybe String
    , dropbox : Maybe Decode.Value
    }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        localRecipes =
            flags.localRecipes
                |> List.filterMap
                    (\entry ->
                        RecipeParser.parse entry.code
                            |> Result.toMaybe
                            |> Maybe.map
                                (\recipe ->
                                    let
                                        title =
                                            Recipe.title recipe
                                    in
                                    ( title
                                    , { method = Recipe.method recipe
                                      , code = entry.code
                                      , checks = Set.fromList entry.checks
                                      }
                                    )
                                )
                    )
                |> Dict.fromList

        dropboxRecipes =
            flags.dropboxRecipes
                |> List.filterMap
                    (\entry ->
                        RecipeParser.parse entry.code
                            |> Result.toMaybe
                            |> Maybe.map
                                (\recipe ->
                                    let
                                        title =
                                            Recipe.title recipe
                                    in
                                    ( title
                                    , { method = Recipe.method recipe
                                      , code = entry.code
                                      , checks = Set.fromList entry.checks
                                      , revision = entry.rev |> SyncedRevision
                                      }
                                    )
                                )
                    )
                |> Dict.fromList

        shoppingList =
            { selectedRecipes = flags.shoppingList.selectedRecipes |> Set.fromList
            , checked = flags.shoppingList.checked |> Set.fromList
            }

        settings =
            flags.settings
                |> Maybe.withDefault
                    { wakeVideoId = Nothing
                    , dropbox = Nothing
                    }

        ( dropbox, dropboxCmd ) =
            settings.dropbox
                |> Maybe.andThen
                    (\raw ->
                        Decode.decodeValue Dropbox.decodeUserAuth raw
                            |> Result.toMaybe
                            |> Maybe.map (\auth -> ( Just (LoggedIn auth), syncDropbox auth ))
                    )
                |> Maybe.withDefault ( Nothing, Cmd.none )

        state =
            { recipes =
                dropboxRecipes
            , shoppingList = shoppingList
            , wakeVideoId = settings.wakeVideoId |> Maybe.withDefault "14Cf79j92xA"
            , dropbox = dropbox
            , nonce = flags.nonce
            }

        ( screen, newState, cmd ) =
            applyUrl state url
    in
    ( { key = key
      , state = newState
      , language = Language.fromString flags.language
      , screen = screen
      }
    , Cmd.batch [ cmd, dropboxCmd ]
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
    | SwitchLanguage String
    | StartLogin
    | NonceGenerated String
    | SyncDropbox (Result Dropbox.ListFolderError Dropbox.ListFolderResponse)
    | DropboxUpdates (Result Dropbox.DownloadError (List Dropbox.DownloadResponse))
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
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

                                oldRecipe =
                                    Dict.get title state.recipes

                                revision =
                                    oldRecipe
                                        |> Maybe.map .revision
                                        |> Maybe.andThen codeFromRevision
                                        |> Maybe.map ChangedRevision
                                        |> Maybe.withDefault NewRevision

                                method =
                                    Recipe.method recipe
                            in
                            ( { model
                                | state =
                                    { state
                                        | recipes =
                                            Dict.insert title
                                                { method = method
                                                , code = code
                                                , checks = Set.empty
                                                , revision = revision
                                                }
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
                    Dict.get recipeTitle state.recipes
                        |> Maybe.map .checks
                        |> Maybe.withDefault Set.empty

                newChecks =
                    operation ingredientName oldChecks

                newRecipes =
                    state.recipes
                        |> Dict.update recipeTitle
                            (Maybe.map (\entry -> { entry | checks = newChecks }))
            in
            ( { model
                | state =
                    { state | recipes = newRecipes }
              }
            , saveRecipeChecksCmd recipeTitle newChecks
            )

        ClearRecipeChecks title ->
            let
                state =
                    model.state

                newRecipes =
                    state.recipes
                        |> Dict.update title
                            (Maybe.map
                                (\entry ->
                                    { entry | checks = Set.empty }
                                )
                            )
            in
            ( { model
                | state =
                    { state | recipes = newRecipes }
              }
            , saveRecipeChecksCmd title Set.empty
            )

        ToggleVideo ->
            let
                newScreen =
                    case model.screen of
                        Recipe title oldOptions ->
                            let
                                newOptions =
                                    { oldOptions | showWakeVideo = not oldOptions.showWakeVideo }
                            in
                            Recipe title newOptions

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

        SwitchLanguage code ->
            ( { model
                | language =
                    Dict.get code Language.available
                        |> Maybe.map .content
                        |> Maybe.withDefault model.language
              }
            , saveLanguage code
            )

        StartLogin ->
            ( model, generateNonce () )

        NonceGenerated nonce ->
            let
                {-
                   deploymentUrl =
                       { protocol = Url.Https
                       , host = "y0hy0h.github.io"
                       , port_ = Nothing
                       , path = "/recipe-box"
                       , query = Nothing
                       , fragment = Nothing
                       }
                -}
                testUrl =
                    let
                        _ =
                            Debug.todo "Remove the localhost Dropbox auth redirect!"
                    in
                    { protocol = Url.Http
                    , host = "localhost"
                    , port_ = Just 8000
                    , path = ""
                    , query = Nothing
                    , fragment = Nothing
                    }

                cmd =
                    Dropbox.authorize
                        { clientId = "916swzhdm7w2eak"
                        , state = Just nonce
                        , requireRole = Nothing
                        , forceReapprove = False
                        , disableSignup = False
                        , locale = Nothing
                        , forceReauthentication = False
                        }
                        testUrl
            in
            ( model, cmd )

        SyncDropbox result ->
            let
                state =
                    model.state
            in
            case result of
                Ok response ->
                    let
                        filesToDownload =
                            response.entries
                                |> List.filterMap
                                    (\entry ->
                                        case entry of
                                            Dropbox.FileMeta data ->
                                                Just ( data.name, data.rev )

                                            _ ->
                                                Nothing
                                    )
                                |> List.filterMap
                                    (\( fileName, revision ) ->
                                        let
                                            recipeTitle =
                                                titleFromFileName fileName
                                        in
                                        Dict.get recipeTitle state.recipes
                                            |> Maybe.map
                                                (\entry ->
                                                    case entry.revision of
                                                        SyncedRevision code ->
                                                            if code == revision then
                                                                Nothing

                                                            else
                                                                Just fileName

                                                        _ ->
                                                            Nothing
                                                )
                                            |> Maybe.withDefault (Just fileName)
                                    )
                    in
                    case state.dropbox of
                        Just (LoggedIn auth) ->
                            let
                                cmd =
                                    filesToDownload
                                        |> List.map (\file -> "/recipes/" ++ file)
                                        |> List.map
                                            (\path ->
                                                Dropbox.download auth { path = path }
                                            )
                                        |> Task.sequence
                                        |> Task.attempt DropboxUpdates
                            in
                            ( model, cmd )

                        _ ->
                            -- TODO: Ask for login
                            ( model, Cmd.none )

                Err error ->
                    -- TODO: Handle error
                    ( model, Cmd.none )

        DropboxUpdates result ->
            let
                state =
                    model.state
            in
            case result of
                Ok responses ->
                    let
                        newRecipes =
                            responses
                                |> List.filterMap
                                    (\response ->
                                        let
                                            recipeTitle =
                                                titleFromFileName response.name
                                        in
                                        case RecipeParser.parse response.content of
                                            Ok recipe ->
                                                if Recipe.title recipe == recipeTitle then
                                                    Just
                                                        ( recipeTitle
                                                        , { method = Recipe.method recipe
                                                          , code = response.content
                                                          , checks = Set.empty
                                                          , revision = SyncedRevision response.rev
                                                          }
                                                        )

                                                else
                                                    -- TODO: Notify about error
                                                    Nothing

                                            Err err ->
                                                -- TODO: Notify about error
                                                Nothing
                                    )
                                |> List.foldl (\( title, entry ) recipes -> Dict.insert title entry recipes) state.recipes
                    in
                    ( { model | state = { state | recipes = newRecipes } }, Cmd.none )

                Err error ->
                    -- TODO: Handle error
                    ( model, Cmd.none )

        UrlChanged url ->
            let
                ( newScreen, newState, cmd ) =
                    applyUrl model.state url

                newModel =
                    { model
                        | state = newState
                        , screen = newScreen
                    }
            in
            ( newModel, cmd )

        LinkClicked request ->
            case request of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )


titleFromFileName : String -> String
titleFromFileName fileName =
    -- Remove .recipe.txt ending
    String.dropRight 11 fileName


type Route
    = OverviewRoute
    | RecipeRoute String
    | NewRoute
    | EditRoute String
    | ShoppingListRoute
    | SettingsRoute


screenFromRoute : State -> Route -> Screen
screenFromRoute state route =
    case route of
        OverviewRoute ->
            Overview

        RecipeRoute title ->
            Dict.get title state.recipes
                |> Maybe.map
                    (\{ method } ->
                        Recipe
                            (Recipe.from title method)
                            { showWakeVideo = False
                            }
                    )
                |> Maybe.withDefault Overview

        NewRoute ->
            Edit { code = "", failure = Nothing }

        EditRoute title ->
            Dict.get title state.recipes
                |> Maybe.map
                    (\{ code } ->
                        Edit { code = code, failure = Nothing }
                    )
                |> Maybe.withDefault Overview

        ShoppingListRoute ->
            Shopping

        SettingsRoute ->
            Settings
                { wakeVideoIdField = "https://youtu.be/" ++ state.wakeVideoId
                , wakeVideoIdError = False
                , dropboxError = False
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


type ParsedRoute
    = ParsedRoute Route
    | DropboxAuth Dropbox.AuthorizeResult


parseUrl : Url -> ParsedRoute
parseUrl url =
    case parseRoute url of
        Just route ->
            ParsedRoute route

        Nothing ->
            Dropbox.parseAuthorizeResult url
                |> Maybe.map DropboxAuth
                |> Maybe.withDefault (ParsedRoute OverviewRoute)


syncDropbox : Dropbox.UserAuth -> Cmd Msg
syncDropbox auth =
    Dropbox.listFolder auth
        { path = "/recipes"
        , recursive = False
        , includeMediaInfo = False
        , includeDeleted = False
        , includeHasExplicitSharedMembers = False
        }
        |> Task.attempt SyncDropbox


applyUrl : State -> Url -> ( Screen, State, Cmd Msg )
applyUrl state url =
    case parseUrl url of
        ParsedRoute route ->
            ( screenFromRoute state route, state, Cmd.none )

        DropboxAuth result ->
            case result of
                Dropbox.AuthorizeOk response ->
                    if response.state == state.nonce then
                        let
                            newState =
                                { state
                                    | dropbox = Just (LoggedIn response.userAuth)
                                }
                        in
                        ( screenFromRoute state SettingsRoute
                        , newState
                        , Cmd.batch [ saveSettingsCmd newState, syncDropbox response.userAuth ]
                        )

                    else
                        ( screenFromRoute state SettingsRoute
                        , { state
                            | dropbox = Just CredentialsError -- TODO: Inform user about possible attack.
                          }
                        , Cmd.none
                        )

                _ ->
                    ( screenFromRoute state SettingsRoute
                    , { state
                        | dropbox = Just CredentialsError -- TODO: Explain error to user.
                      }
                    , Cmd.none
                    )


parseRoute : Url -> Maybe Route
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
    let
        jsonDropbox =
            state.dropbox
                |> Maybe.andThen
                    (\credentials ->
                        case credentials of
                            LoggedIn userAuth ->
                                Just (Dropbox.encodeUserAuth userAuth)

                            _ ->
                                Nothing
                    )
    in
    saveSettings
        { wakeVideoId = Just state.wakeVideoId
        , dropbox = jsonDropbox
        }


port saveSettings : JsonSettings -> Cmd msg


port generateNonce : () -> Cmd msg


port nonceGenerated : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    nonceGenerated NonceGenerated



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

                        checks =
                            Dict.get title state.recipes
                                |> Maybe.map .checks
                                |> Maybe.withDefault Set.empty
                    in
                    viewRecipe model.language
                        recipe
                        { checks = checks
                        , showVideo = options.showWakeVideo
                        , videoId =
                            state.wakeVideoId
                        }

                Edit { code, failure } ->
                    viewEditRecipe model.language code failure

                Shopping ->
                    viewShoppingList model.language state.recipes state.shoppingList

                Settings s ->
                    viewSettings model.language s state
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
    { checks : Set String
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

        ingredientsView =
            viewIngredientList
                [ Html.text language.recipe.noIngredientsRequired ]
                ingredientMap
                options.checks
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
                            :: (if Set.isEmpty options.checks then
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
        |> List.concatMap (\{ method } -> Recipe.ingredients method)


viewSettings : Language -> SettingsState -> State -> ( Maybe String, Html Msg )
viewSettings language settingsState state =
    ( Nothing
    , let
        wakeVideoError =
            if settingsState.wakeVideoIdError then
                Just language.settings.videoUrlInvalid

            else
                Nothing

        wakeVideoSetting =
            Html.div []
                [ textInput language.settings.videoUrlLabel settingsState.wakeVideoIdField SetWakeVideoUrl wakeVideoError
                , viewVideo state.wakeVideoId
                ]

        dropboxSetting =
            Html.div []
                (case state.dropbox of
                    Just credentials ->
                        case credentials of
                            LoggedIn info ->
                                [ Html.text "Logged in" ]

                            CredentialsError ->
                                [ Html.text "An error occurred."
                                ]

                    Nothing ->
                        [ button [] "Log into Dropbox" StartLogin ]
                )
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
            [ wakeVideoSetting, dropboxSetting ]
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
