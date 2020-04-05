module Main exposing (main)

import Browser
import Dict
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import IngredientMap
import Recipe exposing (Recipe)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> Html.toUnstyled
        }


type alias Model =
    { recipe : String
    }


init : Model
init =
    { recipe = "Chop the <onion (1: large onion)>. Fry the onion in <butter (15 g)>. Boil the <egg (1)> in the <water (0.5 l)>. Season with <salt (a pinch of)>. Wait one minute, then boil the other <egg (1)>."
    }


type Msg
    = EditedRecipe String


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditedRecipe recipe ->
            { model | recipe = recipe }


view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Recipe Box" ]
        , case Recipe.parse model.recipe of
            Ok recipe ->
                viewRecipe recipe

            Err error ->
                Html.text error
        , Html.textarea [ Events.onInput EditedRecipe, Attributes.value model.recipe ] []
        ]


viewRecipe : Recipe -> Html Msg
viewRecipe recipe =
    let
        ingredientsView =
            Html.ul []
                (IngredientMap.fromRecipe recipe
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
                                        String.join " + " quantitiesTexts ++ " "

                                text =
                                    quantitiesText ++ name
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
    in
    Html.article []
        (Html.h2 [] [ Html.text "Ingredients" ]
            :: ingredientsView
            :: Html.h2 [] [ Html.text "Description" ]
            :: descriptionView
        )
