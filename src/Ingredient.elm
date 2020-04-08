module Ingredient exposing (Ingredient, Quantity(..), from, listText, quantity, text)


type Ingredient
    = Ingredient
        { text : String
        , quantity : Maybe Quantity
        , listText : Maybe String
        }


type Quantity
    = Amount Float
    | Measure Float String
    | Description String


from : String -> Maybe Quantity -> Maybe String -> Ingredient
from text_ quantity_ listText_ =
    Ingredient
        { text = text_
        , quantity = quantity_
        , listText = listText_
        }


text : Ingredient -> String
text (Ingredient ingred) =
    ingred.text


quantity : Ingredient -> Maybe Quantity
quantity (Ingredient ingred) =
    ingred.quantity


listText : Ingredient -> String
listText (Ingredient ingred) =
    Maybe.withDefault ingred.text ingred.listText
