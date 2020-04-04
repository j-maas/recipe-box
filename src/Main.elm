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
    { recipe : Recipe
    }


init : Model
init =
    { recipe = [ Recipe.Plain "Order some pizza." ]
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
        , viewRecipe model.recipe
        ]


viewRecipe : Recipe -> Html msg
viewRecipe recipe =
    Html.article []
        (Html.h2 [] [ Html.text "Description" ]
            :: List.map
                (\recipePart ->
                    case recipePart of
                        Recipe.Plain text ->
                            Html.text text
                )
                recipe
        )
