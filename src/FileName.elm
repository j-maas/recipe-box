module FileName exposing (FileName, autorename)

import Db exposing (Db)
import Id exposing (Id)
import Regex exposing (Regex)
import Set exposing (Set)
import String.Extra as String


type alias FileName a =
    Id a


autorename : String -> Db a -> FileName a
autorename proposal db =
    let
        proposalId =
            sanitize proposal
    in
    if isMember db proposalId then
        autorenameWithNumber 1 proposal db

    else
        proposalId


autorenameWithNumber : Int -> String -> Db a -> FileName a
autorenameWithNumber suffix proposal db =
    let
        proposalId =
            sanitize (proposal ++ String.fromInt suffix)
    in
    if isMember db proposalId then
        autorenameWithNumber (suffix + 1) proposal db

    else
        proposalId


sanitize : String -> FileName a
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
        |> String.filter
            (\c ->
                Char.isAlphaNum c
                    || Set.member c charWhitelist
            )
        |> Id.fromString


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


isMember : Db a -> Id a -> Bool
isMember db id =
    case Db.get db id of
        Just _ ->
            True

        Nothing ->
            False
