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
    { recipe = Recipe.parse "Fry the <egg (1)> in <butter (15 g)>."
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
                                ingredient.quantity
                                    |> Maybe.map (\quantity -> Recipe.quantityToString quantity ++ " ")
                                    |> Maybe.withDefault ""

                            text =
                                quantityText ++ ingredient.name
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
                            Html.text ingredient.name
                )
                recipe
    in
    Html.article []
        (Html.h2 [] [ Html.text "Ingredients" ]
            :: ingredientsView
            :: Html.h2 [] [ Html.text "Description" ]
            :: descriptionView
        )
