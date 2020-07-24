module Store.PathComponent exposing (PathComponent, autorename, fromString, toString, unsafe)

import Set exposing (Set)
import String.Extra as String


type PathComponent
    = PathComponent String


fromString : String -> Maybe PathComponent
fromString raw =
    if String.all isSafeCharacter raw then
        Just (PathComponent raw)

    else
        Nothing


toString : PathComponent -> String
toString (PathComponent raw) =
    raw


unsafe : String -> PathComponent
unsafe raw =
    PathComponent raw


autorename : String -> (PathComponent -> Bool) -> PathComponent
autorename proposal isMember =
    let
        proposalId =
            sanitize proposal
    in
    if isMember proposalId then
        autorenameWithNumber 1 proposal isMember

    else
        proposalId


autorenameWithNumber : Int -> String -> (PathComponent -> Bool) -> PathComponent
autorenameWithNumber suffix proposal isMember =
    let
        proposalId =
            sanitize (proposal ++ String.fromInt suffix)
    in
    if isMember proposalId then
        autorenameWithNumber (suffix + 1) proposal isMember

    else
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
        [ ' ', '-', '(', ')', '_' ]
