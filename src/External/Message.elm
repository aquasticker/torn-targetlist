module External.Message exposing (..)

import Model.Player exposing (Player)
import Utils.Throttle exposing (ThrottleHistory)


type Message
    = InvalidMessage String
    | ReceivePlayer Player
    | ReceiveThrottleHistory ThrottleHistory
    | ReceiveRunning Bool
