module NextCloud exposing (CompleteLoginResponse, Server, StartLoginResponse, authorityFromServer, completeLogin, serverFromAuthority, serverFromUrl, startLogin)

import Http
import Json.Decode as Decode exposing (Decoder)
import TypedUrl


type Server
    = Server String


serverFromAuthority : String -> Server
serverFromAuthority authority =
    Server authority


serverFromUrl : String -> Maybe Server
serverFromUrl url =
    TypedUrl.parse url
        |> Maybe.map .authority
        |> Maybe.map (String.join ".")
        |> Maybe.map Server


authorityFromServer : Server -> String
authorityFromServer (Server authority) =
    authority


startLogin : Server -> (Result Http.Error StartLoginResponse -> msg) -> Cmd msg
startLogin server toMsg =
    Http.riskyRequest
        { method = "POST"
        , headers = [ Http.header "User-Agent" "Recipe Box" ]
        , url = "https://" ++ authorityFromServer server ++ "/index.php/login/v2"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg startLoginDecoder
        , timeout = Just 10000
        , tracker = Nothing
        }


type alias StartLoginResponse =
    { link : String
    , state : LoginState
    }


type alias LoginState =
    { token : String
    , endpoint : String
    }


startLoginDecoder : Decoder StartLoginResponse
startLoginDecoder =
    Decode.map2
        StartLoginResponse
        (Decode.field "login" Decode.string)
        (Decode.field "poll"
            (Decode.map2 LoginState
                (Decode.field "token" Decode.string)
                (Decode.field "endpoint" Decode.string)
            )
        )


completeLogin : LoginState -> (Result Http.Error CompleteLoginResponse -> msg) -> Cmd msg
completeLogin state toMsg =
    Http.post
        { url = state.endpoint
        , body = Http.multipartBody [ Http.stringPart "token" state.token ]
        , expect = Http.expectJson toMsg completeLoginDecoder
        }


type alias CompleteLoginResponse =
    { server : String
    , loginName : String
    , appPassword : String
    }


completeLoginDecoder : Decoder CompleteLoginResponse
completeLoginDecoder =
    Decode.map3 CompleteLoginResponse
        (Decode.field "server" Decode.string)
        (Decode.field "loginName" Decode.string)
        (Decode.field "appPassword" Decode.string)
