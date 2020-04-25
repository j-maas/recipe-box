module DropboxSync exposing (DeletedResult, DownloadResult, LoginError(..), StartSyncResult, State(..), UploadResult, downloadFile, loginCmd, logoutCmd, parseLoginUrl, processDeleted, processDownloads, processStartSync, processUploads, removeFile, startSyncCmd, uploadFile)

import Db exposing (Db)
import Dropbox
import Http
import Id exposing (Id)
import Revision exposing (Revision(..))
import Task exposing (Task)
import Url exposing (Url)


type State
    = LoggedIn Dropbox.UserAuth
    | LoginErr LoginError


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


parseLoginUrl : String -> Url -> Maybe State
parseLoginUrl nonce url =
    Dropbox.parseAuthorizeResult url
        |> Maybe.map (processLoginResult nonce)


type alias LoginResult =
    Dropbox.AuthorizeResult


type LoginError
    = AccessDenied
    | TemporarilyUnavailable
    | ManipulatedRequest
      -- An error in the protocol, the user cannot do anything.
    | ProtocolError


processLoginResult : String -> LoginResult -> State
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
                LoggedIn response.userAuth

            else
                LoginErr ManipulatedRequest

        Err error ->
            LoginErr error


logoutCmd : State -> (Result Http.Error () -> msg) -> Cmd msg
logoutCmd state msg =
    case state of
        LoggedIn auth ->
            Dropbox.tokenRevoke auth
                |> Task.attempt msg

        _ ->
            Cmd.none


startSyncCmd : Dropbox.UserAuth -> (StartSyncResult -> msg) -> Cmd msg
startSyncCmd auth msg =
    Dropbox.listFolder auth
        { path = "/recipes"
        , recursive = False
        , includeMediaInfo = False
        , includeDeleted = False
        , includeHasExplicitSharedMembers = False
        }
        |> Task.attempt msg


type alias StartSyncResult =
    Result Dropbox.ListFolderError Dropbox.ListFolderResponse


processStartSync :
    StartSyncResult
    -> Db a
    -> Maybe State
    ->
        { revision : a -> Revision
        , content : a -> String
        }
    ->
        { upload : UploadResult -> msg
        , download : DownloadResult -> msg
        }
    -> Cmd msg
processStartSync result db state extract msg =
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
                                                                Synced

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
            in
            case state of
                Just (LoggedIn auth) ->
                    let
                        ( uploadUpdateTasks, downloadTasks ) =
                            statuses
                                |> List.foldl
                                    (\status ( up, down ) ->
                                        case status of
                                            Synced ->
                                                ( up, down )

                                            Conflict _ ->
                                                -- TODO: Notify user
                                                ( up, down )

                                            NeedsUpload entry ->
                                                ( uploadFile auth entry.id (Just entry.revision) entry.content :: up, down )

                                            NeedsDownload file ->
                                                ( up, downloadFile auth file :: down )
                                    )
                                    ( [], [] )

                        uploadTasks =
                            uploadUpdateTasks
                                ++ (localOnlyRecipes
                                        |> Db.toList
                                        |> List.map
                                            (\( id, entry ) ->
                                                uploadFile auth id Nothing (extract.content entry)
                                            )
                                   )

                        cmd =
                            Cmd.batch
                                [ Task.sequence uploadTasks |> Task.attempt msg.upload
                                , Task.sequence downloadTasks |> Task.attempt msg.download
                                ]
                    in
                    cmd

                _ ->
                    -- TODO: Ask for login
                    Cmd.none

        Err error ->
            -- TODO: Handle error
            Cmd.none


type alias UploadResult =
    Result Dropbox.UploadError (List Dropbox.FileMetadata)


processUploads : UploadResult -> (Revision -> a -> a) -> Db a -> Db a
processUploads result updateRevision db =
    case result of
        Ok responses ->
            responses
                |> List.foldl
                    (\response recipes ->
                        let
                            id =
                                idFromDropboxFileName response.name
                        in
                        Db.update id
                            (Maybe.map (updateRevision <| SyncedRevision response.rev))
                            recipes
                    )
                    db

        Err error ->
            -- TODO: Handle error
            db


type alias DownloadResult =
    Result Dropbox.DownloadError (List Dropbox.DownloadResponse)


processDownloads : DownloadResult -> (String -> Revision -> Maybe a) -> Db a -> ( Db a, List (Maybe (Db.Row a)) )
processDownloads result parse db =
    case result of
        Ok responses ->
            let
                newRecipeEntries =
                    responses
                        |> List.map
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

                newRecipes =
                    newRecipeEntries
                        |> List.filterMap identity
                        |> List.foldl (\row database -> Db.insert row database) db
            in
            ( newRecipes, newRecipeEntries )

        Err error ->
            -- TODO: Handle error
            ( db, [] )


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


type SyncStatus a
    = Synced
    | NeedsDownload (Id a)
    | NeedsUpload { id : Id a, revision : String, content : String }
    | Conflict (Id a)



-- API methods


uploadFile : Dropbox.UserAuth -> Id a -> Maybe String -> String -> Task Dropbox.UploadError Dropbox.FileMetadata
uploadFile auth id revision content =
    let
        fileName =
            Id.toString id

        mode =
            case revision of
                Just revisionCode ->
                    Dropbox.Update revisionCode

                Nothing ->
                    Dropbox.Add
    in
    Dropbox.upload auth
        { path = "/recipes/" ++ fileName ++ ".recipe.txt"
        , mode = mode
        , autorename = False
        , clientModified = Nothing
        , mute = False
        , content = content
        }


downloadFile : Dropbox.UserAuth -> Id a -> Task Dropbox.DownloadError Dropbox.DownloadResponse
downloadFile auth id =
    let
        fileName =
            Id.toString id
    in
    Dropbox.download auth { path = "/recipes/" ++ fileName ++ ".recipe.txt" }


removeFile : Dropbox.UserAuth -> Id a -> Revision -> Task Dropbox.DeleteError Dropbox.Metadata
removeFile auth id revision =
    let
        fileName =
            Id.toString id
    in
    Dropbox.delete auth
        { path = "/recipes/" ++ fileName ++ ".recipe.txt"
        , parentRev = Revision.toString revision
        }
