module External.Decode exposing (messageDecoder, playerDecoder)

import External.Message exposing (Message(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Model.Country
    exposing
        ( Country
        , countryFromCode
        )
import Model.Life exposing (Life, lifeDecoder)
import Model.Location exposing (Location(..))
import Model.Player
    exposing
        ( DonatorStatus(..)
        , FactionId(..)
        , Gender(..)
        , OnlineStatus(..)
        , Player
        , PlayerId(..)
        , PlayerStatus(..)
        , createPlayerId
        )
import Time exposing (Posix(..))
import Utils.Throttle exposing (decodeThrottleHistory)


messageDecoder : Decoder Message
messageDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen messageDecoderByType


messageDecoderByType : String -> Decoder Message
messageDecoderByType type_ =
    case type_ of
        "player" ->
            Decode.field "payload" playerDecoder
                |> Decode.map ReceivePlayer

        "throttle" ->
            Decode.field "payload" decodeThrottleHistory
                |> Decode.map ReceiveThrottleHistory

        "running" ->
            Decode.field "payload" Decode.bool
                |> Decode.map ReceiveRunning

        _ ->
            Decode.fail ("Failed to decode message with type '" ++ type_ ++ "'")


playerDecoder : Decoder Player
playerDecoder =
    Decode.succeed Player
        |> required "asOf" posixDecoder
        |> required "id" (Decode.int |> Decode.map createPlayerId)
        |> required "name" Decode.string
        |> required "level" Decode.int
        |> required "rank" Decode.string
        |> required "title" Decode.string
        |> required "age" Decode.int
        |> required "factionId" (factionIdDecoder |> Decode.nullable)
        |> required "gender" genderDecoder
        |> required "donatorStatus" donatorStatusDecoder
        |> required "status" userStatusDecoder
        |> required "onlineStatus" onlineStatusDecoder
        |> required "revivable" Decode.bool
        |> required "life" lifeDecoder


posixDecoder : Decoder Posix
posixDecoder =
    Decode.int
        |> Decode.map Time.millisToPosix


factionIdDecoder : Decoder FactionId
factionIdDecoder =
    Decode.map FactionId Decode.int


genderDecoder : Decoder Gender
genderDecoder =
    let
        get id =
            case id of
                "Male" ->
                    Decode.succeed Male

                "Female" ->
                    Decode.succeed Female

                "Enby" ->
                    Decode.succeed Enby

                _ ->
                    Decode.fail ("unknown value for Gender: " ++ id)
    in
    Decode.string |> Decode.andThen get


donatorStatusDecoder : Decoder DonatorStatus
donatorStatusDecoder =
    let
        get id =
            case id of
                "Subscriber" ->
                    Decode.succeed Subscriber

                "Donator" ->
                    Decode.succeed Donator

                "RegularAccount" ->
                    Decode.succeed RegularAccount

                _ ->
                    Decode.fail ("unknown value for DonatorStatus: " ++ id)
    in
    Decode.string |> Decode.andThen get


userStatusDecoder : Decoder PlayerStatus
userStatusDecoder =
    let
        resolve id =
            case id of
                "OK" ->
                    Decode.succeed Okay

                "Hospital" ->
                    Decode.succeed Hospital
                        |> required "until" posixDecoder
                        |> optional "country" (countryDecoder |> Decode.map LocationAbroad) LocationTorn

                "Jail" ->
                    Decode.succeed Jail
                        |> required "until" posixDecoder

                "Federal" ->
                    Decode.succeed FederalJail
                        |> required "until" posixDecoder

                "Abroad" ->
                    Decode.succeed Abroad
                        |> required "country" countryDecoder

                "Traveling" ->
                    Decode.succeed TravelingTo
                        |> required "country" countryDecoder

                "Returning" ->
                    Decode.succeed ReturningFrom
                        |> required "country" countryDecoder

                _ ->
                    Decode.fail ("unknown value for UserStatus: " ++ id)
    in
    Decode.field "status" Decode.string
        |> Decode.andThen resolve


countryDecoder : Decoder Country
countryDecoder =
    let
        get code =
            case countryFromCode code of
                Just country ->
                    Decode.succeed country

                Nothing ->
                    Decode.fail ("unknown value for country: " ++ code)
    in
    Decode.string
        |> Decode.andThen get


onlineStatusDecoder : Decoder OnlineStatus
onlineStatusDecoder =
    let
        resolve id =
            case id of
                "Online" ->
                    Decode.succeed Online
                        |> required "timestamp" posixDecoder

                "Idle" ->
                    Decode.succeed Idle
                        |> required "timestamp" posixDecoder

                "Offline" ->
                    Decode.succeed Offline
                        |> required "timestamp" posixDecoder

                _ ->
                    Decode.fail ("unknown value for OnlineStatus: " ++ id)
    in
    Decode.field "status" Decode.string
        |> Decode.andThen resolve
