module Revision exposing (Revision(..), toString)


type Revision
    = NewRevision
    | ChangedRevision String
    | SyncedRevision String


toString : Revision -> Maybe String
toString revision =
    case revision of
        NewRevision ->
            Nothing

        ChangedRevision code ->
            Just code

        SyncedRevision code ->
            Just code
