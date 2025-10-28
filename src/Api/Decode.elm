module Api.Decode exposing (getUserInfoResponseDecoder)

import Api.Data exposing (GetUserInfoResponse)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Model.Country as Country
    exposing
        ( Country
        )
import Model.Life exposing (Life, createLife)
import Model.Player
    exposing
        ( DonatorStatus(..)
        , FactionId(..)
        , Gender(..)
        , OnlineStatus(..)
        , Player
        , PlayerStatus(..)
        , createPlayerId
        )
import Time exposing (Posix(..))


type alias UserStatusPayload =
    { description : String
    , state : String
    , until : Maybe Int
    }


type alias OnlineStatusPayload =
    { status : String
    , timestamp : Int
    }


decoderFromMaybe : (String -> Maybe b) -> String -> String -> Decoder b
decoderFromMaybe toMaybe message value =
    toMaybe value
        |> Maybe.map Json.Decode.succeed
        |> Maybe.withDefault (Json.Decode.fail (message ++ "'" ++ value ++ "'"))


detectCountry : String -> Decoder Country
detectCountry =
    decoderFromMaybe Country.detectCountry "Failed to detect country"


getUserInfoResponseDecoder : Posix -> Decoder GetUserInfoResponse
getUserInfoResponseDecoder timestamp =
    Json.Decode.succeed GetUserInfoResponse
        |> required "profile" (profileDataResponseDecoder timestamp)


userStatusPayloadDecoder : Decoder UserStatusPayload
userStatusPayloadDecoder =
    Json.Decode.succeed UserStatusPayload
        |> required "description" Json.Decode.string
        |> required "state" Json.Decode.string
        |> required "until" (Json.Decode.nullable Json.Decode.int)


userStatusMapper : UserStatusPayload -> Decoder PlayerStatus
userStatusMapper { state, until, description } =
    case ( state, until ) of
        ( "Okay", _ ) ->
            Json.Decode.succeed Okay

        ( "Hospital", Just ts ) ->
            Json.Decode.succeed (Hospital (Time.millisToPosix (ts * 1000)))

        ( "Jail", Just ts ) ->
            Json.Decode.succeed (Jail (Time.millisToPosix (ts * 1000)))

        ( "Abroad", _ ) ->
            detectCountry description
                |> Json.Decode.map Abroad

        ( "Traveling", _ ) ->
            if String.contains "returning" (String.toLower description) then
                detectCountry description
                    |> Json.Decode.map ReturningFrom

            else
                detectCountry description
                    |> Json.Decode.map TravelingTo

        _ ->
            Json.Decode.fail ("Failed to decode user status '" ++ state ++ "'")


profileDataResponseDecoder : Posix -> Decoder Player
profileDataResponseDecoder timestamp =
    Json.Decode.succeed (Player timestamp)
        |> required "id" (Json.Decode.int |> Json.Decode.map createPlayerId)
        |> required "name" Json.Decode.string
        |> required "level" Json.Decode.int
        |> required "rank" Json.Decode.string
        |> required "title" Json.Decode.string
        |> required "age" Json.Decode.int
        |> required "faction_id" (factionIdDecoder |> Json.Decode.nullable)
        |> required "gender" genderDecoder
        |> required "donator_status" donatorStatusDecoder
        |> required "status" userStatusDecoder
        |> required "last_action" onlineStatusDecoder
        |> required "revivable" Json.Decode.bool
        |> required "life" (lifeDecoder timestamp)


lifeDecoder : Posix -> Decoder Life
lifeDecoder ts =
    Json.Decode.succeed (createLife ts)
        |> required "current" Json.Decode.int
        |> required "maximum" Json.Decode.int


donatorStatusDecoder : Decoder DonatorStatus
donatorStatusDecoder =
    let
        mapSubscriber : String -> DonatorStatus
        mapSubscriber id =
            case id of
                "Subscriber" ->
                    Subscriber

                "Donator" ->
                    Donator

                _ ->
                    RegularAccount
    in
    Json.Decode.nullable Json.Decode.string
        |> Json.Decode.map (Maybe.withDefault "")
        |> Json.Decode.map mapSubscriber


genderDecoder : Decoder Gender
genderDecoder =
    let
        get id =
            case id of
                "Male" ->
                    Json.Decode.succeed Male

                "Female" ->
                    Json.Decode.succeed Female

                "Enby" ->
                    Json.Decode.succeed Enby

                _ ->
                    Json.Decode.fail ("unknown value for Gender: " ++ id)
    in
    Json.Decode.string |> Json.Decode.andThen get


factionIdDecoder : Decoder FactionId
factionIdDecoder =
    Json.Decode.map FactionId Json.Decode.int


userStatusDecoder : Decoder PlayerStatus
userStatusDecoder =
    userStatusPayloadDecoder
        |> Json.Decode.andThen userStatusMapper


onlineStatusPayloadDecoder : Decoder OnlineStatusPayload
onlineStatusPayloadDecoder =
    Json.Decode.succeed OnlineStatusPayload
        |> required "status" Json.Decode.string
        |> required "timestamp" Json.Decode.int


onlineStatusDecoder : Decoder OnlineStatus
onlineStatusDecoder =
    let
        get { status, timestamp } =
            let
                posix =
                    Time.millisToPosix (timestamp * 1000)
            in
            case status of
                "Online" ->
                    Json.Decode.succeed (Online posix)

                "Idle" ->
                    Json.Decode.succeed (Idle posix)

                "Offline" ->
                    Json.Decode.succeed (Offline posix)

                _ ->
                    Json.Decode.fail ("unknown value for OnlineStatus: " ++ status)
    in
    onlineStatusPayloadDecoder
        |> Json.Decode.andThen get
