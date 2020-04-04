module Recipe exposing (Recipe, RecipePart(..), parse)


type alias Recipe =
    List RecipePart


type RecipePart
    = Plain String


parse : String -> Maybe Recipe
parse input =
    Just [ Plain input ]
