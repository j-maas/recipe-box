port module Main exposing (main)

import Browser
import Css exposing (auto, em, num, pct, rem, zero)
import Css.Global as Global
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import IngredientMap
import Recipe exposing (Recipe)


main : Program (List String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }


type alias Model =
    { recipes : Dict String ( Recipe.RecipeParts, String )
    , current : Current
    }


type Current
    = None
    | Viewing Recipe
    | Editing String (Maybe String)


init : List String -> ( Model, Cmd Msg )
init recipes =
    ( { recipes =
            List.filterMap
                (\code ->
                    Recipe.parse code
                        |> Result.toMaybe
                        |> Maybe.map (\recipe -> ( recipe, code ))
                )
                recipes
                |> List.map (\( recipe, code ) -> ( Recipe.title recipe, ( Recipe.description recipe, code ) ))
                |> Dict.fromList
      , current = None
      }
    , Cmd.none
    )


type Msg
    = SelectedRecipe String
    | ToOverview
    | EditRecipe String
    | DeleteRecipe String
    | Edited String
    | Save
    | NewRecipe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedRecipe title ->
            ( { model
                | current =
                    Dict.get title model.recipes
                        |> Maybe.map
                            (\( parts, _ ) -> Viewing <| Recipe.from title parts)
                        |> Maybe.withDefault None
              }
            , Cmd.none
            )

        ToOverview ->
            ( { model | current = None }, Cmd.none )

        EditRecipe title ->
            let
                newCurrent =
                    case Dict.get title model.recipes of
                        Just ( _, code ) ->
                            Editing code Nothing

                        Nothing ->
                            model.current
            in
            ( { model | current = newCurrent }, Cmd.none )

        DeleteRecipe title ->
            ( { model
                | recipes = Dict.remove title model.recipes
                , current = None
              }
            , delete title
            )

        Edited code ->
            let
                newCurrent =
                    case model.current of
                        Editing _ _ ->
                            Editing code Nothing

                        _ ->
                            model.current
            in
            ( { model | current = newCurrent }, Cmd.none )

        Save ->
            case model.current of
                Editing code _ ->
                    case Recipe.parse code of
                        Ok recipe ->
                            let
                                title =
                                    Recipe.title recipe

                                parts =
                                    Recipe.description recipe
                            in
                            ( { model
                                | current = Viewing recipe
                                , recipes =
                                    Dict.insert title
                                        ( parts, code )
                                        model.recipes
                              }
                            , save { title = title, code = code }
                            )

                        Err error ->
                            ( { model | current = Editing code (Just error) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NewRecipe ->
            ( { model | current = Editing "" Nothing }, Cmd.none )


port save : { title : String, code : String } -> Cmd msg


port delete : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    Html.main_
        [ css
            [ bodyFontStyle
            , Css.lineHeight (num 1.4)
            , Css.maxWidth (em 48)
            , Css.margin2 zero auto
            , Css.marginTop (em 2)
            ]
        ]
        [ case model.current of
            Viewing recipe ->
                viewRecipe recipe

            Editing code errors ->
                viewEditRecipe code errors

            None ->
                viewRecipeList (Dict.keys model.recipes)
        ]


viewRecipeList : List String -> Html Msg
viewRecipeList recipeTitles =
    Html.div []
        [ Html.h1 [ css [ headingStyle ] ] [ Html.text "Recipe Box" ]
        , Html.div [ css [ Css.margin2 (em 1) zero ] ] [ button "New recipe" NewRecipe ]
        , let
            recipeList =
                List.map
                    (\recipeTitle ->
                        Html.li
                            [ css
                                [ clickableStyle
                                , Css.padding (em 0.5)
                                , Css.hover
                                    [ Css.backgroundColor (Css.hsla 0 0 0.5 0.1)
                                    ]
                                ]
                            , Events.onClick
                                (SelectedRecipe recipeTitle)
                            ]
                            [ Html.text recipeTitle
                            ]
                    )
                    recipeTitles
          in
          if List.isEmpty recipeList then
            Html.text "You do not have any recipes yet. Create a new recipe!"

          else
            ul
                [ Css.property "list-style-type" "\">  \""
                , Css.listStylePosition Css.inside
                , Css.paddingLeft zero
                ]
                []
                recipeList
        ]


viewRecipe : Recipe -> Html Msg
viewRecipe recipe =
    let
        ingredientsView =
            ul []
                []
                (IngredientMap.fromDescription (Recipe.description recipe)
                    |> Dict.toList
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
                )

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
                |> List.map (\paragraph -> Html.p [] paragraph)
    in
    Html.div []
        [ Html.nav []
            [ Html.span
                [ css [ clickableStyle, Css.fontStyle Css.italic ]
                , Events.onClick ToOverview
                ]
                [ Html.text "<< Back to list" ]
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
                [ button "Edit" (EditRecipe <| Recipe.title recipe)
                , button "Delete" (DeleteRecipe <| Recipe.title recipe)
                ]
            ]
        , Html.article
            []
            (Html.h2 [ css [ headingStyle ] ] [ Html.text <| Recipe.title recipe ]
                :: Html.details []
                    [ Html.summary [ css [ clickableStyle ] ]
                        [ Html.h3
                            [ css
                                [ headingStyle
                                , Css.display Css.inlineBlock
                                ]
                            ]
                            [ Html.text "Ingredients" ]
                        ]
                    , ingredientsView
                    ]
                :: Html.h3 [ css [ headingStyle ] ] [ Html.text "Description" ]
                :: descriptionView
            )
        ]


viewEditRecipe : String -> Maybe String -> Html Msg
viewEditRecipe code errors =
    Html.div []
        ((case errors of
            Just error ->
                [ Html.text error ]

            Nothing ->
                []
         )
            ++ [ Html.textarea
                    [ Events.onInput Edited
                    , Attributes.value code
                    , css
                        [ Css.width (pct 100)
                        , Css.maxWidth (pct 100)
                        , Css.height (em 20)
                        ]
                    ]
                    []
               , button "Save" Save
               ]
        )


button : String -> Msg -> Html Msg
button text msg =
    Html.button
        [ Events.onClick msg
        , css
            [ borderStyle
            , clickableStyle
            , Css.boxShadow4 zero (rem 0.1) (rem 0.1) (Css.hsla 0 0 0 0.3)
            , Css.hover
                [ Css.backgroundColor (Css.hsla 0 0 0.5 0.1)
                , Css.boxShadow4 zero (rem 0.1) (rem 0.2) (Css.hsla 0 0 0 0.3)
                ]
            , headingFontStyle
            ]
        ]
        [ Html.text text ]


ul : List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
ul styles attributes children =
    Html.ul
        (attributes
            ++ [ css
                    ([ Css.margin zero
                     , Css.paddingLeft (em 1.5)
                     ]
                        ++ styles
                    )
               ]
        )
        children


p : List Css.Style -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
p styles attributes children =
    Html.p
        (attributes
            ++ [ css
                    ([ Css.margin zero
                     , Global.adjacentSiblings
                        [ Global.typeSelector "p"
                            [ Css.marginTop (em 0.6)
                            ]
                        ]
                     ]
                        ++ styles
                    )
               ]
        )
        children


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
