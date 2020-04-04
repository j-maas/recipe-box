module Main exposing (main)

import Browser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
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
    { recipe = "Chop the <onion (1: large onion)>. Fry the onion in <butter (15 g)>. Boil the <egg (1)> in the <water (0.5 l)>. Season with <salt (a pinch of)>."
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
        [ Html.h1 [] [ Html.text "Recipe Shopper" ]
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
        ingredients =
            Recipe.ingredients recipe

        ingredientsView =
            Html.ul []
                (List.map
                    (\ingredient ->
                        let
                            quantityText =
                                Recipe.getQuantity ingredient
                                    |> Maybe.map (\quantity -> stringFromQuantity quantity ++ " ")
                                    |> Maybe.withDefault ""

                            text =
                                quantityText ++ Recipe.getListName ingredient
                        in
                        Html.li [] [ Html.text text ]
                    )
                    ingredients
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


stringFromQuantity : Recipe.Quantity -> String
stringFromQuantity quantity =
    case quantity of
        Recipe.Description unit ->
            unit

        Recipe.Amount amount ->
            String.fromFloat amount

        Recipe.Measure amount unit ->
            String.fromFloat amount ++ " " ++ unit
