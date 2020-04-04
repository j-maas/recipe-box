module Main exposing (main)

import Browser
import Html.Styled as Html exposing (Html)
import Recipe exposing (Recipe)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> Html.toUnstyled
        }


type alias Model =
    { recipe : Result Recipe.ParsingError Recipe
    }


init : Model
init =
    { recipe = Recipe.parse "Chop the <onion (1: large onion)>. Fry the onion in <butter (15 g)>. Boil the <egg (1)> in the <water (0.5 l)>."
    }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html msg
view model =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Recipe Shopper" ]
        , case model.recipe of
            Ok recipe ->
                viewRecipe recipe

            Err error ->
                Html.text error
        ]


viewRecipe : Recipe -> Html msg
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
                                    |> Maybe.map (\quantity -> Recipe.quantityToString quantity ++ " ")
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
