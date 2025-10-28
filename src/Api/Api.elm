module Api.Api exposing (..)

-- Inner types
--

import Api.Data exposing (ApiToken, getApiToken)
import Api.Decode exposing (getUserInfoResponseDecoder)
import Http exposing (Error(..), Header, emptyBody)
import Http.Tasks exposing (resolveJson)
import Json.Decode as Decode
import Model.Player
    exposing
        ( Player
        , PlayerId(..)
        )
import Task exposing (Task)
import Time exposing (Posix)


getUserInfoCmd : ApiToken -> PlayerId -> (Result Error Player -> msg) -> Cmd msg
getUserInfoCmd apiToken userId toMsg =
    Time.now
        |> Task.andThen (getUserInfoTask apiToken userId)
        |> Task.attempt toMsg


getUserInfoTask : ApiToken -> PlayerId -> Posix -> Task Error Player
getUserInfoTask apiToken (PlayerId userId) timestamp =
    let
        decoder : Decode.Decoder Player
        decoder =
            getUserInfoResponseDecoder timestamp
                |> Decode.map (\response -> response.profile)
    in
    Http.task
        { method = "GET"
        , headers = createHeaders apiToken
        , url = "https://api.torn.com/v2/user/" ++ String.fromInt userId ++ "?comment=TornChain"
        , body = emptyBody
        , resolver = resolveJson decoder
        , timeout = Nothing
        }


createHeaders : ApiToken -> List Header
createHeaders apiToken =
    [ Http.header "Authorization" ("ApiKey " ++ getApiToken apiToken)
    ]


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server, check your network connection"

        BadStatus 500 ->
            "The server had a problem, try again later"

        BadStatus 400 ->
            "Verify your information and try again"

        BadStatus _ ->
            "Unknown error"

        BadBody errorMessage ->
            errorMessage
