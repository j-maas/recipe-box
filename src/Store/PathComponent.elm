module Store.PathComponent exposing (NameCollision(..), PathComponent, autorename, fromString, nameCollisionFromMember, toString, unsafe)

import Set exposing (Set)
import String.Extra as String


type PathComponent
    = PathComponent String


fromString : String -> Maybe PathComponent
fromString raw =
    if String.all isSafeCharacter raw && not (String.isEmpty raw) then
        Just (PathComponent raw)

    else
        Nothing


toString : PathComponent -> String
toString (PathComponent raw) =
    raw


unsafe : String -> PathComponent
unsafe raw =
    PathComponent raw


autorename : String -> (PathComponent -> NameCollision) -> PathComponent
autorename proposal nameCollision =
    let
        proposalId =
            sanitize proposal
    in
    case nameCollision proposalId of
        Collision ->
            autorenameWithNumber 1 proposal nameCollision

        NewName ->
            proposalId


type NameCollision
    = Collision
    | NewName


nameCollisionFromMember : Bool -> NameCollision
nameCollisionFromMember isMember =
    if isMember then
        Collision

    else
        NewName


autorenameWithNumber : Int -> String -> (PathComponent -> NameCollision) -> PathComponent
autorenameWithNumber suffix proposal nameCollision =
    let
        proposalId =
            sanitize (proposal ++ "-" ++ String.fromInt suffix)
    in
    case nameCollision proposalId of
        Collision ->
            autorenameWithNumber (suffix + 1) proposal nameCollision

        NewName ->
            proposalId


isSafeCharacter : Char -> Bool
isSafeCharacter c =
    Char.isAlphaNum c || Set.member c charWhitelist


sanitize : String -> PathComponent
sanitize raw =
    let
        replace ( pattern, replacement ) =
            String.replace pattern replacement

        doReplacements s =
            List.foldl replace s replacements
    in
    raw
        |> doReplacements
        |> String.removeAccents
        |> String.filter isSafeCharacter
        |> PathComponent


replacements : List ( String, String )
replacements =
    [ ( "Ä", "Ae" )
    , ( "ä", "ae" )
    , ( "Ö", "Oe" )
    , ( "ö", "oe" )
    , ( "Ü", "Ue" )
    , ( "ü", "ue" )
    , ( "ß", "ss" )
    ]


charWhitelist : Set Char
charWhitelist =
    Set.fromList
        [ ' ', '-', '(', ')', '_', '.' ]
