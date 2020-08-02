module SyncedStoreTest exposing (suite)

import Expect
import Fuzz
import Store.FilePath exposing (FilePath)
import Store.FolderPath exposing (FolderPath)
import Store.Store as Store exposing (Store)
import Store.SyncedStore as SyncedStore exposing (LocalStoreAccess, RemoteStoreAccess, SyncedStore)
import Test exposing (..)
import TestUtils exposing (entriesFuzzer, filePathFuzzer, sortEntries)


suite : Test
suite =
    describe "synced store"
        [ fuzz2 filePathFuzzer Fuzz.int "inserts into both stores" <|
            \filePath item ->
                emptySyncedStore
                    |> SyncedStore.insert filePath item
                    |> expectEqualInAllStores filePath (Just item)
        , fuzz3 filePathFuzzer Fuzz.int Fuzz.int "updates existing item in both stores" <|
            \filePath firstItem secondItem ->
                emptySyncedStore
                    |> SyncedStore.insert filePath firstItem
                    |> SyncedStore.update filePath (Maybe.map ((+) secondItem))
                    |> expectEqualInAllStores filePath (Just <| firstItem + secondItem)
        , fuzz2 entriesFuzzer entriesFuzzer "sync results in same items in both stores" <|
            \localEntries remoteEntries ->
                let
                    localStore =
                        List.map (\( path, item ) -> ( path, ( item, Nothing ) )) localEntries
                            |> Store.insertList Store.empty

                    remoteStore =
                        ( List.length remoteEntries
                        , List.indexedMap (\index ( path, item ) -> ( path, ( item, String.fromInt index ) )) remoteEntries
                            |> Store.insertList Store.empty
                        )
                in
                SyncedStore.with
                    { local = ( localStore, localStoreAccess )
                    , remote = ( remoteStore, remoteStoreAccess )
                    }
                    |> SyncedStore.sync []
                    |> (\syncedStore ->
                            let
                                locals =
                                    SyncedStore.local syncedStore
                                        |> Store.listAll []
                                        |> List.map (\( path, ( item, _ ) ) -> ( path, item ))
                                        |> sortEntries

                                remotes =
                                    SyncedStore.remote syncedStore
                                        |> Tuple.second
                                        |> Store.listAll []
                                        |> List.map (\( path, ( item, _ ) ) -> ( path, item ))
                                        |> sortEntries
                            in
                            locals
                                |> Expect.equalLists remotes
                       )
        ]


emptySyncedStore : SyncedStore LocalStore RemoteStore Int
emptySyncedStore =
    SyncedStore.with { local = ( Store.empty, localStoreAccess ), remote = ( ( 0, Store.empty ), remoteStoreAccess ) }


type alias LocalStore =
    Store ( Int, Maybe SyncedStore.SyncTag )


localStoreAccess : LocalStoreAccess LocalStore Int
localStoreAccess =
    { insert = \path item syncTag store -> Store.insert path ( item, syncTag ) store
    , insertWithRename = \path item syncTag store -> Store.insertWithRename path ( item, syncTag ) store
    , insertList =
        \store list ->
            Store.insertList store
                (List.map
                    (\( path, item, syncTag ) ->
                        ( path, ( item, syncTag ) )
                    )
                    list
                )
    , read = Store.read
    , update = Store.update
    , delete = Store.delete
    , listAll =
        \folder store ->
            Store.listAll folder store
                |> List.map (\( path, ( item, syncTag ) ) -> ( path, item, syncTag ))
    }


type alias RemoteStore =
    ( Int, Store ( Int, String ) )


remoteStoreAccess : RemoteStoreAccess RemoteStore Int
remoteStoreAccess =
    { insert = \path item ( count, store ) -> ( count + 1, Store.insert path ( item, String.fromInt count ) store )
    , insertList =
        \( count, store ) list ->
            let
                ( newCount, entries ) =
                    List.foldl
                        (\( path, item ) ( c, es ) ->
                            ( c + 1, ( path, ( item, String.fromInt c ) ) :: es )
                        )
                        ( count, [] )
                        list
            in
            ( newCount, Store.insertList store entries )
    , read = \path ( _, store ) -> Store.read path store
    , update =
        \path f ( count, store ) ->
            ( count + 1
            , Store.update path
                (\maybeEntry ->
                    let
                        newVersion =
                            String.fromInt count
                    in
                    case maybeEntry of
                        Just ( item, version ) ->
                            f (Just item)
                                |> Maybe.map
                                    (\i ->
                                        ( i
                                        , if i == item then
                                            version

                                          else
                                            newVersion
                                        )
                                    )

                        Nothing ->
                            f Nothing |> Maybe.map (\i -> ( i, newVersion ))
                )
                store
            )
    , delete = \path ( count, store ) -> ( count, Store.delete path store )
    , listAll =
        \folder ( _, store ) ->
            Store.listAll folder store
                |> List.map (\( path, ( item, version ) ) -> ( path, item, version ))
    }



-- Expectation helpers


expectEqualInAllStores : FilePath -> Maybe Int -> SyncedStore LocalStore RemoteStore Int -> Expect.Expectation
expectEqualInAllStores path expected syncedStore =
    let
        local =
            let
                actual =
                    Store.read path (SyncedStore.local syncedStore)
                        |> Maybe.map (\( item, _ ) -> item)
            in
            if actual == expected then
                Nothing

            else
                Just ("Local store failed:\n\n" ++ reportInequality actual expected)

        remote =
            let
                actual =
                    SyncedStore.remote syncedStore
                        |> Tuple.second
                        |> Store.read path
                        |> Maybe.map (\( item, _ ) -> item)
            in
            if actual == expected then
                Nothing

            else
                Just ("Remote store failed:\n\n" ++ reportInequality actual expected)

        combined =
            String.join "\n\n\n" (List.filterMap identity [ local, remote ])
    in
    if String.isEmpty combined then
        Expect.pass

    else
        Expect.fail combined


reportInequality : item -> item -> String
reportInequality top bottom =
    String.join "\n"
        [ "    " ++ Debug.toString top
        , "    ╷"
        , "    │ Expect.equal"
        , "    ╵"
        , "    " ++ Debug.toString bottom
        ]
