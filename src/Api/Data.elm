module Api.Data exposing
    ( ApiToken
    , GetUserInfoResponse
    , createApiToken
    , getApiToken
    )

import Model.Player exposing (Player)


type ApiToken
    = ApiToken String


getApiToken : ApiToken -> String
getApiToken (ApiToken apiToken) =
    apiToken


createApiToken : String -> ApiToken
createApiToken value =
    ApiToken value


type alias GetUserInfoResponse =
    { profile : Player
    }
