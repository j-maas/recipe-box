module DropboxSync exposing (DeletedResult, DownloadResult, FetchResult, LoginError(..), Msg, State, UploadResult, decodeState, deleteCmd, encodeState, loginCmd, logoutCmd, parseLoginUrl, syncCmd, update, uploadCmd)

import Db exposing (Db)
import Dropbox
import Http
import Id exposing (Id)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Revision exposing (Revision(..))
import Store.FilePath as FilePath exposing (FilePath)
import Task exposing (Task)
import Url exposing (Url)


type alias State =
    { auth : Dropbox.UserAuth
    }


type Msg
    = Fetched FetchResult
    | Uploaded UploadResult
    | Downloaded DownloadResult
    | Deleted DeletedResult


update :
    Msg
    -> State
    -> Db a
    ->
        { revision : a -> Revision
        , content : a -> String
        , create : String -> Revision -> Maybe a
        , updateRevision : Revision -> a -> a
        }
    -> ( State, List (Db.Row a), Cmd Msg )
update msg state db item =
    case msg of
        Fetched result ->
            ( state
            , []
            , processFetched
                result
                db
                state
                item
            )

        Uploaded result ->
            ( state
            , processUploads result item.updateRevision db
            , Cmd.none
            )

        Downloaded result ->
            ( state
            , processDownloads result item.create db
            , Cmd.none
            )

        Deleted result ->
            ( state, [], Cmd.none )


encodeState : State -> Encode.Value
encodeState state =
    Dropbox.encodeUserAuth state.auth


decodeState : Decoder State
decodeState =
    Dropbox.decodeUserAuth
        |> Decode.map
            (\auth ->
                { auth = auth
                }
            )


loginCmd : String -> Cmd msg
loginCmd nonce =
    let
        {-
           deploymentUrl =
               { protocol = Url.Https
               , host = "y0hy0h.github.io"
               , port_ = Nothing
               , path = "/recipe-box"
               , query = Nothing
               , fragment = Nothing
               }
        -}
        testUrl =
            let
                _ =
                    Debug.log "Remove the localhost Dropbox auth redirect!" ()
            in
            { protocol = Url.Http
            , host = "localhost"
            , port_ = Just 8000
            , path = ""
            , query = Nothing
            , fragment = Nothing
            }
    in
    Dropbox.authorize
        { clientId = "916swzhdm7w2eak"
        , state = Just nonce
        , requireRole = Nothing
        , forceReapprove = False
        , disableSignup = False
        , locale = Nothing
        , forceReauthentication = False
        }
        testUrl


parseLoginUrl : String -> Url -> Maybe (Result LoginError State)
parseLoginUrl nonce url =
    Dropbox.parseAuthorizeResult url
        |> Maybe.map (processLoginResult nonce)


type alias LoginResult =
    Dropbox.AuthorizeResult


type LoginError
    = AccessDenied
    | ManipulatedRequest
    | TemporarilyUnavailable
      -- An error in the protocol, the user cannot do anything.
    | ProtocolError


processLoginResult : String -> LoginResult -> Result LoginError State
processLoginResult nonce result =
    let
        parseLoginResult res =
            case res of
                Dropbox.AuthorizeOk response ->
                    Ok response

                Dropbox.DropboxAuthorizeErr error ->
                    Err
                        (case error.error of
                            "access_denied" ->
                                AccessDenied

                            "temporarily_unavailable" ->
                                TemporarilyUnavailable

                            _ ->
                                ProtocolError
                        )

                Dropbox.UnknownAccessTokenErr _ ->
                    Err ProtocolError
    in
    case parseLoginResult result of
        Ok response ->
            if response.state == Just nonce then
                Ok { auth = response.userAuth }

            else
                Err ManipulatedRequest

        Err error ->
            Err error


logoutCmd : State -> (Result Http.Error () -> msg) -> Cmd msg
logoutCmd state msg =
    Dropbox.tokenRevoke state.auth
        |> Task.attempt msg


syncCmd : State -> Cmd Msg
syncCmd state =
    Dropbox.listFolder state.auth
        { path = "/recipes"
        , recursive = False
        , includeMediaInfo = False
        , includeDeleted = False
        , includeHasExplicitSharedMembers = False
        }
        |> Task.attempt Fetched


type alias FetchResult =
    Result Dropbox.ListFolderError Dropbox.ListFolderResponse


processFetched :
    FetchResult
    -> Db a
    -> State
    ->
        { b
            | revision : a -> Revision
            , content : a -> String
        }
    -> Cmd Msg
processFetched result db state extract =
    case result of
        Ok response ->
            let
                remoteRecipes =
                    response.entries
                        |> List.filterMap
                            (\entry ->
                                case entry of
                                    Dropbox.FileMeta data ->
                                        Just ( data.name, data.rev )

                                    -- TODO: Handle deleted files.
                                    _ ->
                                        Nothing
                            )

                ( statuses, localOnlyRecipes ) =
                    remoteRecipes
                        |> List.foldl
                            (\( fileName, revision ) ( stats, database ) ->
                                let
                                    name =
                                        idFromDropboxFileName fileName

                                    item =
                                        Db.get database (Id.fromString name)

                                    newRecipes =
                                        Db.remove (Id.fromString name) database

                                    newStatus =
                                        item
                                            |> Maybe.map
                                                (\entry ->
                                                    case extract.revision entry of
                                                        SyncedRevision code ->
                                                            if code == revision then
                                                                SyncDone

                                                            else
                                                                NeedsDownload name

                                                        ChangedRevision revisionCode ->
                                                            if revisionCode == revision then
                                                                NeedsUpload
                                                                    { name = name
                                                                    , revision = revisionCode
                                                                    , content = extract.content entry
                                                                    }

                                                            else
                                                                Conflict name

                                                        NewRevision ->
                                                            -- The file already exists on the remote.
                                                            Conflict name
                                                )
                                            |> Maybe.withDefault (NeedsDownload name)
                                in
                                ( newStatus :: stats, newRecipes )
                            )
                            ( [], db )

                ( uploadUpdateTasks, downloadTasks ) =
                    statuses
                        |> List.foldl
                            (\status ( up, down ) ->
                                case status of
                                    SyncDone ->
                                        ( up, down )

                                    Conflict _ ->
                                        -- TODO: Notify user
                                        ( up, down )

                                    NeedsUpload entry ->
                                        ( uploadFile state entry.name (Just entry.revision) entry.content :: up, down )

                                    NeedsDownload file ->
                                        ( up, downloadFile state file :: down )
                            )
                            ( [], [] )

                uploadTasks =
                    uploadUpdateTasks
                        ++ (localOnlyRecipes
                                |> Db.toList
                                |> List.map
                                    (\( id, entry ) ->
                                        uploadFile state (Id.toString id) Nothing (extract.content entry)
                                    )
                           )
            in
            Cmd.batch
                [ Task.sequence uploadTasks |> Task.attempt Uploaded
                , Task.sequence downloadTasks |> Task.attempt Downloaded
                ]

        Err error ->
            -- TODO: Handle error
            Cmd.none


uploadCmd : State -> String -> Revision -> String -> Cmd Msg
uploadCmd state fileName revision content =
    uploadFile state fileName (Revision.toString revision) content
        |> Task.map List.singleton
        |> Task.attempt Uploaded


type alias UploadResult =
    Result Dropbox.UploadError (List Dropbox.FileMetadata)


processUploads : UploadResult -> (Revision -> a -> a) -> Db a -> List (Db.Row a)
processUploads result updateRevision db =
    case result of
        Ok responses ->
            responses
                |> List.filterMap
                    (\response ->
                        let
                            id =
                                Id.fromString (idFromDropboxFileName response.name)
                        in
                        Db.get db id
                            -- TODO: Handle missing ids.
                            |> Maybe.map
                                (\item ->
                                    let
                                        revision =
                                            SyncedRevision response.rev
                                    in
                                    ( id, updateRevision revision item )
                                )
                    )

        Err error ->
            -- TODO: Handle error
            []


type alias DownloadResult =
    Result Dropbox.DownloadError (List Dropbox.DownloadResponse)


processDownloads : DownloadResult -> (String -> Revision -> Maybe a) -> Db a -> List (Db.Row a)
processDownloads result parse db =
    case result of
        Ok responses ->
            let
                newRecipeEntries =
                    responses
                        -- TODO: Handle failed creation of items from content.
                        |> List.filterMap
                            (\response ->
                                let
                                    id =
                                        Id.fromString (idFromDropboxFileName response.name)
                                in
                                case parse response.content (SyncedRevision response.rev) of
                                    Just item ->
                                        Just
                                            ( id
                                            , item
                                            )

                                    Nothing ->
                                        -- TODO: Notify about error
                                        Nothing
                            )
            in
            newRecipeEntries

        Err error ->
            -- TODO: Handle error
            []


deleteCmd : State -> String -> Revision -> Cmd Msg
deleteCmd state fileName revision =
    removeFile state fileName revision
        |> Task.attempt Deleted


type alias DeletedResult =
    Result Dropbox.DeleteError Dropbox.Metadata


processDeleted : DeletedResult -> Bool
processDeleted result =
    case result of
        Ok metadata ->
            case metadata of
                Dropbox.FileMeta info ->
                    True

                -- TODO: Express error
                _ ->
                    False

        Err error ->
            False


idFromDropboxFileName : String -> String
idFromDropboxFileName fileName =
    -- Remove .recipe.txt ending
    String.dropRight 11 fileName


type SyncUpdate a
    = SyncDone
    | NeedsDownload String
    | NeedsUpload { name : String, revision : String, content : String }
    | Conflict String



-- API methods


uploadFile : State -> String -> Maybe String -> String -> Task Dropbox.UploadError Dropbox.FileMetadata
uploadFile state fileName maybeRevisionCode content =
    let
        mode =
            case maybeRevisionCode of
                Just revisionCode ->
                    Dropbox.Update revisionCode

                Nothing ->
                    Dropbox.Add
    in
    Dropbox.upload state.auth
        { path = "/recipes/" ++ fileName ++ ".recipe.txt"
        , mode = mode
        , autorename = False
        , clientModified = Nothing
        , mute = False
        , content = content
        }


downloadFile : State -> String -> Task Dropbox.DownloadError Dropbox.DownloadResponse
downloadFile state fileName =
    Dropbox.download state.auth { path = "/recipes/" ++ fileName ++ ".recipe.txt" }


removeFile : State -> String -> Revision -> Task Dropbox.DeleteError Dropbox.Metadata
removeFile state fileName revision =
    Dropbox.delete state.auth
        { path = "/recipes/" ++ fileName ++ ".recipe.txt"
        , parentRev = Revision.toString revision
        }
