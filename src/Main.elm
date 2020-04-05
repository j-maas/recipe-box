module Main exposing (main)

import Browser
import Css exposing (auto, em, num, pct, zero)
import Css.Global as Global
import Dict
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
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
    Html.main_
        [ css
            [ Css.fontFamilies [ "Zilla Slab", "Palatino", "serif" ]
            , Css.lineHeight (num 1.4)
            , Css.maxWidth (em 48)
            , Css.margin2 zero auto
            , Css.marginTop (em 2)
            , Global.descendants
                [ Global.typeSelector "ul"
                    [ Css.margin zero
                    ]
                , Global.typeSelector "p"
                    [ Css.margin zero
                    , Global.adjacentSiblings
                        [ Global.typeSelector "p"
                            [ Css.marginTop (em 0.6)
                            ]
                        ]
                    ]
                ]
            ]
        ]
        [ Html.h1 [ css [ headingStyle ] ] [ Html.text "Recipe Box" ]
        , case Recipe.parse model.recipe of
            Ok recipe ->
                viewRecipe recipe

            Err error ->
                Html.text error
        , Html.textarea
            [ Events.onInput EditedRecipe
            , Attributes.value model.recipe
            , css
                [ Css.width (pct 100)
                , Css.maxWidth (pct 100)
                ]
            ]
            []
        ]


viewRecipe : Recipe -> Html Msg
viewRecipe recipe =
    let
        ingredientsView =
            Html.ul []
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
                |> List.map (\paragraph -> Html.p [] paragraph)
    in
    Html.article
        []
        (Html.h2 [ css [ headingStyle ] ] [ Html.text <| Recipe.title recipe ]
            :: Html.details []
                [ Html.summary []
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


headingStyle : Css.Style
headingStyle =
    Css.batch
        [ Css.fontFamilies [ "Bree Serif", "Georgia", "serif" ]
        , Css.lineHeight (num 1)
        , Css.margin zero
        , Css.marginTop (em 0.6)
        , Css.marginBottom (em 0.3)
        ]
