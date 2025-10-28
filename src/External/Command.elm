module External.Command exposing (..)

import Api.Data exposing (ApiToken)
import Model.Player exposing (Player, PlayerId)
import Utils.Throttle exposing (Throttle)


type Command
    = SaveApiToken ApiToken
    | SaveApiRate Int
    | LoadPlayers
    | LoadThrottle
    | SavePlayer Player
    | DeletePlayer PlayerId
    | SaveThrottle Throttle
    | SetRunning Bool
