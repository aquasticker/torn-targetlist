module Model.Player exposing (..)

import Model.Country exposing (Country)
import Model.Life exposing (Life)
import Time exposing (Posix(..))


type PlayerId
    = PlayerId Int


type FactionId
    = FactionId Int


type Gender
    = Male
    | Female
    | Enby


type DonatorStatus
    = Subscriber
    | Donator
    | RegularAccount


getPlayerId : PlayerId -> Int
getPlayerId (PlayerId userId) =
    userId


createPlayerId : Int -> PlayerId
createPlayerId id =
    PlayerId id


type OnlineStatus
    = Online Posix
    | Idle Posix
    | Offline Posix


getLastActivity : OnlineStatus -> Posix
getLastActivity onlineStatus =
    case onlineStatus of
        Online posix ->
            posix

        Idle posix ->
            posix

        Offline posix ->
            posix


type PlayerStatus
    = Okay
    | Hospital Posix
    | Jail Posix
    | Abroad Country
    | TravelingTo Country
    | ReturningFrom Country


type alias Player =
    { asOf : Posix
    , id : PlayerId
    , name : String
    , level : Int
    , rank : String
    , title : String
    , age : Int
    , factionId : Maybe FactionId
    , gender : Gender
    , donatorStatus : DonatorStatus
    , status : PlayerStatus
    , onlineStatus : OnlineStatus
    , revivable : Bool
    , life : Life
    }
