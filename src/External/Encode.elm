module External.Encode exposing (commandEncoder, playerEncoder)

import Api.Data exposing (getApiToken)
import External.Command exposing (Command(..))
import Json.Encode as Encode
import Model.Country
    exposing
        ( Country
        , countryCode
        )
import Model.Life exposing (Life, encodeLife)
import Model.Player
    exposing
        ( DonatorStatus(..)
        , FactionId(..)
        , Gender(..)
        , OnlineStatus(..)
        , Player
        , PlayerId(..)
        , PlayerStatus(..)
        )
import Time exposing (Posix(..))
import Utils.Throttle exposing (encodeThrottle)


commandEncoder : Command -> Encode.Value
commandEncoder command =
    case command of
        SavePlayer player ->
            playerEncoder player
                |> encodeCommand "savePlayer"

        SaveThrottle throttle ->
            encodeThrottle throttle
                |> encodeCommand "saveThrottle"

        LoadPlayers ->
            encodeCommandSimple "loadPlayers"

        DeletePlayer playerId ->
            encodePlayerId playerId
                |> encodeCommand "deletePlayer"

        SaveApiToken apiToken ->
            apiToken
                |> getApiToken
                |> Encode.string
                |> encodeCommand "saveApiToken"

        SaveApiRate apiRate ->
            apiRate
                |> Encode.int
                |> encodeCommand "saveApiRate"

        SetRunning running ->
            Encode.bool running
                |> encodeCommand "setRunning"

        LoadThrottle ->
            encodeCommandSimple "loadThrottle"


encodeCommand : String -> Encode.Value -> Encode.Value
encodeCommand command payload =
    Encode.object <|
        [ ( "command", Encode.string command )
        , ( "payload", payload )
        ]


encodeCommandSimple : String -> Encode.Value
encodeCommandSimple command =
    Encode.object <|
        [ ( "command", Encode.string command ) ]


playerEncoder : Player -> Encode.Value
playerEncoder user =
    Encode.object <|
        [ ( "asOf", encodePosix user.asOf )
        , ( "id", encodePlayerId user.id )
        , ( "name", Encode.string user.name )
        , ( "level", Encode.int user.level )
        , ( "rank", Encode.string user.rank )
        , ( "title", Encode.string user.title )
        , ( "age", Encode.int user.age )
        , ( "factionId", encodeMaybeFactionId user.factionId )
        , ( "gender", encodeGender user.gender )
        , ( "donatorStatus", encodeDonatorStatus user.donatorStatus )
        , ( "status", encodeUserStatus user.status )
        , ( "onlineStatus", encodeOnlineStatus user.onlineStatus )
        , ( "revivable", Encode.bool user.revivable )
        , ( "life", encodeLife user.life )
        ]


encodePlayerId : PlayerId -> Encode.Value
encodePlayerId (PlayerId playerId) =
    Encode.int playerId


encodePosix : Time.Posix -> Encode.Value
encodePosix posix =
    Encode.int (Time.posixToMillis posix)


encodeMaybeFactionId : Maybe FactionId -> Encode.Value
encodeMaybeFactionId maybeFactionId =
    (Maybe.map encodeFactionId >> Maybe.withDefault Encode.null) maybeFactionId


encodeFactionId : FactionId -> Encode.Value
encodeFactionId (FactionId factionId) =
    Encode.int factionId


encodeGender : Gender -> Encode.Value
encodeGender gender =
    case gender of
        Male ->
            Encode.string "Male"

        Female ->
            Encode.string "Female"

        Enby ->
            Encode.string "Enby"


encodeDonatorStatus : DonatorStatus -> Encode.Value
encodeDonatorStatus donatorStatus =
    case donatorStatus of
        Subscriber ->
            Encode.string "Subscriber"

        Donator ->
            Encode.string "Donator"

        RegularAccount ->
            Encode.string "RegularAccount"


encodeCountry : Country -> Encode.Value
encodeCountry country =
    countryCode country
        |> Encode.string


encodeUserStatus : PlayerStatus -> Encode.Value
encodeUserStatus userStatus =
    case userStatus of
        Okay ->
            Encode.object <|
                [ ( "status", Encode.string "OK" )
                ]

        Hospital posix ->
            Encode.object <|
                [ ( "status", Encode.string "Hospital" )
                , ( "until", encodePosix posix )
                ]

        Jail posix ->
            Encode.object <|
                [ ( "status", Encode.string "Jail" )
                , ( "until", encodePosix posix )
                ]

        FederalJail posix ->
            Encode.object <|
                [ ( "status", Encode.string "Federal" )
                , ( "until", encodePosix posix )
                ]

        Abroad country ->
            Encode.object <|
                [ ( "status", Encode.string "Abroad" )
                , ( "country", encodeCountry country )
                ]

        TravelingTo country ->
            Encode.object <|
                [ ( "status", Encode.string "Traveling" )
                , ( "country", encodeCountry country )
                ]

        ReturningFrom country ->
            Encode.object <|
                [ ( "status", Encode.string "Returning" )
                , ( "country", encodeCountry country )
                ]


encodeOnlineStatus : OnlineStatus -> Encode.Value
encodeOnlineStatus onlineStatus =
    case onlineStatus of
        Online posix ->
            Encode.object <|
                [ ( "status", Encode.string "Online" )
                , ( "timestamp", encodePosix posix )
                ]

        Idle posix ->
            Encode.object <|
                [ ( "status", Encode.string "Idle" )
                , ( "timestamp", encodePosix posix )
                ]

        Offline posix ->
            Encode.object <|
                [ ( "status", Encode.string "Offline" )
                , ( "timestamp", encodePosix posix )
                ]
