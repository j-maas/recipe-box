module DropboxSync exposing (DeletedResult, DownloadResult, FetchResult, LoginError(..), State, UploadResult, Msg, update, decodeState, deleteCmd, encodeState, loginCmd, logoutCmd, parseLoginUrl, syncCmd, uploadCmd)

import Db exposing (Db)
import Dropbox
import Http
import Id exposing (Id)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Revision exposing (Revision(..))
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
                                    id =
                                        idFromDropboxFileName fileName

                                    item =
                                        Db.get database id

                                    newRecipes =
                                        Db.remove id database

                                    newStatus =
                                        item
                                            |> Maybe.map
                                                (\entry ->
                                                    case extract.revision entry of
                                                        SyncedRevision code ->
                                                            if code == revision then
                                                                SyncDone

                                                            else
                                                                NeedsDownload id

                                                        ChangedRevision revisionCode ->
                                                            if revisionCode == revision then
                                                                NeedsUpload
                                                                    { id = id
                                                                    , revision = revisionCode
                                                                    , content = extract.content entry
                                                                    }

                                                            else
                                                                Conflict id

                                                        NewRevision ->
                                                            -- The file already exists on the remote.
                                                            Conflict id
                                                )
                                            |> Maybe.withDefault (NeedsDownload id)
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
                                        ( uploadFile state entry.id (Just entry.revision) entry.content :: up, down )

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
                                        uploadFile state id Nothing (extract.content entry)
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


uploadCmd : State -> Id a -> Revision -> String -> Cmd Msg
uploadCmd state id revision content =
    uploadFile state id (Revision.toString revision) content
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
                                idFromDropboxFileName response.name
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
                                        idFromDropboxFileName response.name
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


deleteCmd : State -> Id a -> Revision -> Cmd Msg
deleteCmd state id revision =
    removeFile state id revision
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


idFromDropboxFileName : String -> Id a
idFromDropboxFileName fileName =
    -- Remove .recipe.txt ending
    String.dropRight 11 fileName
        |> Id.fromString


type SyncUpdate a
    = SyncDone
    | NeedsDownload (Id a)
    | NeedsUpload { id : Id a, revision : String, content : String }
    | Conflict (Id a)



-- API methods


uploadFile : State -> Id a -> Maybe String -> String -> Task Dropbox.UploadError Dropbox.FileMetadata
uploadFile state id maybeRevisionCode content =
    let
        fileName =
            Id.toString id

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


downloadFile : State -> Id a -> Task Dropbox.DownloadError Dropbox.DownloadResponse
downloadFile state id =
    let
        fileName =
            Id.toString id
    in
    Dropbox.download state.auth { path = "/recipes/" ++ fileName ++ ".recipe.txt" }


removeFile : State -> Id a -> Revision -> Task Dropbox.DeleteError Dropbox.Metadata
removeFile state id revision =
    let
        fileName =
            Id.toString id
    in
    Dropbox.delete state.auth
        { path = "/recipes/" ++ fileName ++ ".recipe.txt"
        , parentRev = Revision.toString revision
        }
