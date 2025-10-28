port module External.Storage exposing
    ( deletePlayer
    , loadPlayers
    , loadThrottle
    , saveApiRate
    , saveApiToken
    , savePlayer
    , saveThrottle
    , setRunning
    , subscribe
    )

import Api.Data exposing (ApiToken)
import External.Command exposing (Command(..))
import External.Decode exposing (messageDecoder)
import External.Encode as External
import External.Message exposing (Message(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Player exposing (Player, PlayerId)
import Utils.Throttle exposing (Throttle)



{--
 Public API
--}


saveApiToken : ApiToken -> Cmd msg
saveApiToken apiToken =
    performCommand (SaveApiToken apiToken)


saveApiRate : Int -> Cmd msg
saveApiRate apiRate =
    performCommand (SaveApiRate apiRate)


setRunning : Bool -> Cmd msg
setRunning running =
    performCommand (SetRunning running)


loadPlayers : () -> Cmd msg
loadPlayers () =
    performCommand LoadPlayers


loadThrottle : () -> Cmd msg
loadThrottle () =
    performCommand LoadThrottle


savePlayer : Player -> Cmd msg
savePlayer player =
    performCommand (SavePlayer player)


deletePlayer : PlayerId -> Cmd msg
deletePlayer playerId =
    performCommand (DeletePlayer playerId)


saveThrottle : Throttle -> Cmd msg
saveThrottle throttle =
    performCommand (SaveThrottle throttle)


subscribe : (Message -> msg) -> Sub msg
subscribe toMsg =
    receive (decodeMessage >> toMsg)



{--
 Private
--}


performCommand : Command -> Cmd msg
performCommand =
    External.commandEncoder >> command


decodeMessage : Decode.Value -> Message
decodeMessage value =
    case Decode.decodeValue messageDecoder value of
        Ok message ->
            message

        Err error ->
            InvalidMessage (Decode.errorToString error)


port command : Decode.Value -> Cmd msg


port receive : (Encode.Value -> msg) -> Sub msg
