module Page.PlayerList exposing (..)

import Api.Api as Api
import Api.Data exposing (ApiToken)
import Dict exposing (Dict)
import External.Message exposing (Message(..))
import External.Storage as Storage
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http exposing (Error(..))
import Icons.Icons as Icons
import Maybe.Extra exposing (isJust)
import Model.Country exposing (countryName)
import Model.Life exposing (Life, LifeValue, getLife)
import Model.Player exposing (OnlineStatus(..), Player, PlayerId, PlayerStatus(..), createPlayerId, getLastActivity, getPlayerId)
import OutMessage exposing (OutMessage)
import Parser
import Set exposing (Set)
import Task
import Time exposing (Posix)
import Utils.Throttle as Throttle exposing (Throttle)
import Utils.TimeDiff as TimeDiff


type alias Model =
    { addPlayerId : String
    , players : Dict Int Player
    , running : Bool
    , throttle : Throttle
    , ts : Posix
    , pending : Set Int
    }


type PlayerActivity
    = ActivityInactiveOld
    | ActivityInactiveTemp
    | ActivityActive


type Msg
    = MsgReceivedPlayerApi (Result Error Player)
    | MsgReceivedMessage Message
    | MsgTick Posix
    | MsgChangePlayerId String
    | MsgAddPlayer PlayerId
    | MsgDeletePlayer PlayerId
    | MsgToggleRunning
    | MsgOpenSettings


subscription : Model -> Sub Msg
subscription model =
    Sub.batch
        [ Storage.subscribe MsgReceivedMessage
        , tickSub model
        ]


tickSub _ =
    Time.every 500 MsgTick


init : Int -> ( Model, Cmd Msg )
init apiRate =
    let
        model : Model
        model =
            { addPlayerId = ""
            , players = Dict.empty
            , running = False
            , throttle = Throttle.init apiRate
            , ts = Time.millisToPosix 0
            , pending = Set.empty
            }
    in
    ( model
    , Cmd.batch
        [ Storage.loadPlayers ()
        , Task.perform MsgTick Time.now
        ]
    )


view : Model -> Html Msg
view model =
    Html.div
        []
        [ viewHeader model
        , viewPlayers model.ts model.players
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    Html.div
        [ Attr.class "chain-header chain-flex-split" ]
        [ viewAddPlayer model.addPlayerId
        , Html.div
            []
            [ viewSettingsButton
            , viewRunButton model.running
            ]
        ]


viewSettingsButton : Html Msg
viewSettingsButton =
    Html.button
        [ Attr.class "chain-button chain-button-running"
        , Events.onClick MsgOpenSettings
        ]
        [ Icons.settings 14
        ]


viewRunButton : Bool -> Html Msg
viewRunButton running =
    if running then
        Html.button
            [ Attr.class "chain-button chain-button-running"
            , Attr.title "Pause API updates"
            , Events.onClick MsgToggleRunning
            ]
            [ Icons.pause 14
            ]

    else
        Html.button
            [ Attr.class "chain-button chain-button-running"
            , Attr.title "Start API updates"
            , Events.onClick MsgToggleRunning
            ]
            [ Icons.play 14
            ]


viewPlayers : Posix -> Dict Int Player -> Html Msg
viewPlayers ts dict =
    let
        players : List Player
        players =
            dict
                |> Dict.values
                |> List.sortWith (playerOrder ts)
    in
    Html.div
        []
        (List.map (viewPlayer ts) players)


viewPlayer : Posix -> Player -> Html Msg
viewPlayer ts player =
    let
        life =
            getLife ts player.life

        userIdStr =
            player.id
                |> getPlayerId
                |> String.fromInt

        ( statusName, statusClass, _ ) =
            getPlayerStatus ts player.status

        lastActive =
            getLastActivity player.onlineStatus

        activityDiff =
            TimeDiff.between lastActive ts

        lastUpdatedSec =
            TimeDiff.between ts player.asOf
                |> TimeDiff.asSeconds

        upToDateClass =
            if lastUpdatedSec < 60 then
                "chain-last-update-ok"

            else
                "chain-last-update-outdated"
    in
    Html.div
        [ Attr.class ("chain-player " ++ upToDateClass ++ " " ++ getPlayerBlockClass ts player)
        ]
        [ Html.div
            [ Attr.class "chain-player-wrap" ]
            [ Html.div
                [ Attr.class "chain-player-online" ]
                [ Html.div
                    [ Attr.class ("chain-online-status " ++ onlineStatusClass player.onlineStatus) ]
                    []
                ]
            , Html.div
                [ Attr.class "chain-player-info"
                ]
                [ Html.a
                    [ Attr.class "chain-player-name"
                    , Attr.href ("https://www.torn.com/profiles.php?XID=" ++ userIdStr)
                    ]
                    [ Html.text (player.name ++ " [" ++ userIdStr ++ "]") ]
                , Html.div
                    [ Attr.class statusClass ]
                    [ Html.text statusName ]
                , Html.div
                    [ Attr.class "chain-player-last-action" ]
                    [ Html.text (TimeDiff.formatInHuman activityDiff) ]
                ]
            , Html.div
                [ Attr.class "chain-life-wrapper" ]
                [ Html.div
                    [ Attr.class "chain-progressbar-text" ]
                    [ Html.text (String.fromInt life.current ++ " / " ++ String.fromInt life.maximum) ]
                , viewLifeProgressbar life
                ]
            , Html.div
                [ Attr.class "chain-player-buttons" ]
                [ Html.button
                    [ Attr.class "button chain-player-delete"
                    , Events.onClick (MsgDeletePlayer player.id)
                    , Attr.title "Delete player"
                    ]
                    [ Icons.trash 14
                    ]
                ]
            ]
        ]


onlineStatusClass : OnlineStatus -> String
onlineStatusClass onlineStatus =
    case onlineStatus of
        Online _ ->
            "chain-online-status-online"

        Idle _ ->
            "chain-online-status-idle"

        Offline _ ->
            "chain-online-status-offline"


viewLifeProgressbar : LifeValue -> Html Msg
viewLifeProgressbar life =
    let
        progress =
            100.0
                * (toFloat life.current / toFloat life.maximum)
                |> round
                |> String.fromInt
    in
    Html.div
        []
        [ Html.div
            [ Attr.class "chain-progressbar" ]
            [ Html.div
                [ Attr.class "chain-progressbar-inner"
                , Attr.style "width" (progress ++ "%")
                ]
                []
            ]
        ]


viewAddPlayer : String -> Html Msg
viewAddPlayer addPlayerId =
    let
        val : Maybe Int
        val =
            Parser.run Parser.int addPlayerId
                |> Result.toMaybe

        validInt : Bool
        validInt =
            isJust val

        onclick =
            val
                |> Maybe.withDefault 0
                |> createPlayerId
                |> MsgAddPlayer
    in
    Html.div
        []
        [ Html.input
            [ Attr.value addPlayerId
            , Attr.placeholder "Player ID"
            , Events.onInput MsgChangePlayerId
            ]
            []
        , Html.button
            [ Attr.disabled (not validInt)
            , Attr.class "chain-button"
            , Events.onClick onclick
            ]
            [ Html.text "Add" ]
        ]


update : ApiToken -> Msg -> Model -> ( Model, Cmd Msg, OutMessage )
update apiToken msg model =
    case msg of
        MsgReceivedPlayerApi result ->
            case result of
                Ok userData ->
                    ( updateUserData userData model
                    , Storage.savePlayer userData
                    , OutMessage.none
                    )

                Err err ->
                    ( model
                    , Cmd.none
                      -- , OutMessage.showApiError (Api.errorToString err)
                    , OutMessage.none
                    )

        MsgChangePlayerId val ->
            setModel
                { model | addPlayerId = val }

        MsgAddPlayer userId ->
            ( { model | addPlayerId = "" }
            , processRequestUserInfo apiToken userId
            , OutMessage.none
            )

        MsgDeletePlayer playerId ->
            ( processDeleteUser playerId model
            , Storage.deletePlayer playerId
            , OutMessage.none
            )

        MsgTick now ->
            processTick apiToken now model

        MsgReceivedMessage message ->
            case message of
                InvalidMessage error ->
                    ( model
                    , Cmd.none
                    , OutMessage.showApiError error
                    )

                ReceivePlayer player ->
                    ( updateUserData player model
                    , Cmd.none
                    , OutMessage.none
                    )

                ReceiveThrottleHistory history ->
                    ( { model | throttle = Throttle.setHistory history model.throttle }
                    , Cmd.none
                    , OutMessage.none
                    )

                ReceiveRunning running ->
                    ( { model | running = running }
                    , Cmd.none
                    , OutMessage.none
                    )

        MsgToggleRunning ->
            let
                running =
                    not model.running
            in
            ( { model | running = running }
            , Storage.setRunning running
            , OutMessage.none
            )

        MsgOpenSettings ->
            ( model
            , Cmd.none
            , OutMessage.showSettingsPage
            )


setModel : Model -> ( Model, Cmd Msg, OutMessage )
setModel model =
    ( model
    , Cmd.none
    , OutMessage.none
    )


processRequestUserInfo : ApiToken -> PlayerId -> Cmd Msg
processRequestUserInfo apiToken userId =
    Api.getUserInfoCmd apiToken userId MsgReceivedPlayerApi


updateUserData : Player -> Model -> Model
updateUserData user model =
    let
        id =
            getPlayerId user.id

        newPlayers =
            Dict.insert id user model.players

        newPending =
            Set.remove id model.pending
    in
    { model
        | players = newPlayers
        , pending = newPending
    }


processDeleteUser : PlayerId -> Model -> Model
processDeleteUser userId model =
    let
        id =
            getPlayerId userId

        newPlayers =
            Dict.remove id model.players

        newPending =
            Set.remove id model.pending
    in
    { model
        | players = newPlayers
        , pending = newPending
    }


processTick : ApiToken -> Posix -> Model -> ( Model, Cmd Msg, OutMessage )
processTick apiToken posix model =
    let
        modelWithTs =
            { model | ts = posix }
    in
    case ( model.running, getUserToRefresh posix model ) of
        ( True, Just user ) ->
            tryRefreshUser apiToken posix modelWithTs user

        ( _, _ ) ->
            setModel modelWithTs


getUserToRefresh : Posix -> Model -> Maybe Player
getUserToRefresh posix model =
    model.players
        |> Dict.values
        |> List.foldl (resolveUserToRefresh posix model.pending) Nothing


resolveUserToRefresh : Posix -> Set Int -> Player -> Maybe Player -> Maybe Player
resolveUserToRefresh currentTs pending user maybeUser =
    let
        userId =
            getPlayerId user.id
    in
    if Set.member userId pending then
        maybeUser

    else
        resolveUserToRefresh2 currentTs user maybeUser


resolveUserToRefresh2 : Posix -> Player -> Maybe Player -> Maybe Player
resolveUserToRefresh2 currentTs user maybeUser =
    let
        nextRefresh =
            getNextRefresh currentTs user

        diff =
            posixDiff currentTs nextRefresh
    in
    if diff > 0 then
        case maybeUser of
            Just user2 ->
                if posixDiff nextRefresh (getNextRefresh currentTs user2) < 0 then
                    Just user

                else
                    Just user2

            Nothing ->
                Just user

    else
        maybeUser


tryRefreshUser : ApiToken -> Posix -> Model -> Player -> ( Model, Cmd Msg, OutMessage )
tryRefreshUser apiToken posix model user =
    let
        maybeNewThrottle =
            Throttle.canExecute posix model.throttle
    in
    case maybeNewThrottle of
        Just newThrottle ->
            ( { model
                | throttle = newThrottle
                , pending = Set.insert (getPlayerId user.id) model.pending
              }
            , Cmd.batch
                [ Storage.saveThrottle newThrottle
                , Api.getUserInfoCmd apiToken user.id MsgReceivedPlayerApi
                ]
            , OutMessage.none
            )

        Nothing ->
            setModel model


getNextRefresh : Posix -> Player -> Posix
getNextRefresh currentTs user =
    case user.status of
        Okay ->
            case getPlayerActivity currentTs user of
                ActivityInactiveOld ->
                    posixAdd 7000 user.asOf

                ActivityInactiveTemp ->
                    posixAdd 3000 user.asOf

                ActivityActive ->
                    posixAdd 1000 user.asOf

        Hospital posix ->
            case getPlayerActivity currentTs user of
                ActivityInactiveOld ->
                    posix

                ActivityInactiveTemp ->
                    posixAdd 10000 user.asOf

                ActivityActive ->
                    posixAdd 2000 user.asOf

        Abroad _ ->
            posixAdd 10000 user.asOf

        TravelingTo _ ->
            posixAdd 10000 user.asOf

        ReturningFrom _ ->
            posixAdd 10000 user.asOf

        Jail posix ->
            case getPlayerActivity currentTs user of
                ActivityInactiveOld ->
                    posix

                ActivityInactiveTemp ->
                    posixAdd 10000 user.asOf

                ActivityActive ->
                    posixAdd 2000 user.asOf

        FederalJail posix ->
            posix


getPlayerActivity : Posix -> Player -> PlayerActivity
getPlayerActivity currentTs user =
    case user.onlineStatus of
        Online _ ->
            ActivityActive

        Idle _ ->
            ActivityActive

        Offline lastAction ->
            if posixDiff currentTs lastAction > 1000 * 60 * 60 * 24 * 7 then
                ActivityInactiveOld

            else
                ActivityInactiveTemp


posixDiff : Posix -> Posix -> Int
posixDiff t1 t2 =
    Time.posixToMillis t1 - Time.posixToMillis t2


posixAdd : Int -> Posix -> Posix
posixAdd millis posix =
    posix
        |> Time.posixToMillis
        |> (+) millis
        |> Time.millisToPosix


getPlayerStatus : Posix -> PlayerStatus -> ( String, String, PlayerStatus )
getPlayerStatus ts userStatus =
    case userStatus of
        Okay ->
            ( "Okay"
            , "chain-user-status-ok"
            , Okay
            )

        Hospital posix ->
            temporaryStatus ts
                posix
                (\remaining ->
                    ( "In hospital for " ++ remaining
                    , "chain-user-status-hospital"
                    , Hospital posix
                    )
                )

        Abroad country ->
            ( "In " ++ countryName country
            , "chain-user-status-travel"
            , Abroad country
            )

        TravelingTo country ->
            ( "Traveling to " ++ countryName country
            , "chain-user-status-travel"
            , Abroad country
            )

        ReturningFrom country ->
            ( "Returning from " ++ countryName country
            , "chain-user-status-travel"
            , Abroad country
            )

        Jail posix ->
            temporaryStatus ts
                posix
                (\remaining ->
                    ( "In jail for " ++ remaining
                    , "chain-user-status-jail"
                    , Jail posix
                    )
                )

        FederalJail posix ->
            temporaryStatus ts
                posix
                (\remaining ->
                    ( "In federal jail for " ++ remaining
                    , "chain-user-status-federal-jail"
                    , FederalJail posix
                    )
                )


temporaryStatus : Posix -> Posix -> (String -> ( String, String, PlayerStatus )) -> ( String, String, PlayerStatus )
temporaryStatus ts posix value =
    let
        diff : TimeDiff.TimeDiff
        diff =
            TimeDiff.between posix ts
    in
    if TimeDiff.inFuture diff then
        if TimeDiff.asSeconds diff < (24 * 60 * 60) then
            value (TimeDiff.formatAsClock diff)

        else
            value (TimeDiff.formatInHumanSimple diff)

    else
        ( "Okay"
        , "chain-user-status-ok"
        , Okay
        )


playerOrder : Posix -> Player -> Player -> Order
playerOrder posix a b =
    let
        ( _, _, statusA ) =
            getPlayerStatus posix a.status

        ( _, _, statusB ) =
            getPlayerStatus posix b.status
    in
    case statusOrder posix statusA statusB of
        LT ->
            LT

        EQ ->
            case onlineStatusOrder a.onlineStatus b.onlineStatus of
                LT ->
                    LT

                EQ ->
                    lifeOrder posix a.life b.life

                GT ->
                    GT

        GT ->
            GT


lifeOrder : Posix -> Life -> Life -> Order
lifeOrder posix a b =
    let
        lifeA : LifeValue
        lifeA =
            getLife posix a

        lifeB : LifeValue
        lifeB =
            getLife posix b
    in
    compare lifeA.current lifeB.current


onlineStatusOrder : OnlineStatus -> OnlineStatus -> Order
onlineStatusOrder a b =
    compare
        (onlineStatusOrderValue a)
        (onlineStatusOrderValue b)


onlineStatusOrderValue : OnlineStatus -> Int
onlineStatusOrderValue onlineStatus =
    case onlineStatus of
        Online _ ->
            1

        Idle _ ->
            2

        Offline _ ->
            3


statusOrder : Posix -> PlayerStatus -> PlayerStatus -> Order
statusOrder ts a b =
    compare
        (statusOrderValue ts a)
        (statusOrderValue ts b)


statusOrderValue : Posix -> PlayerStatus -> Int
statusOrderValue now userStatus =
    case userStatus of
        Okay ->
            100000

        Hospital ts ->
            temporaryStatusOrderValue 200000 now ts

        ReturningFrom _ ->
            300000

        Abroad _ ->
            400000

        TravelingTo _ ->
            500000

        Jail ts ->
            temporaryStatusOrderValue 600000 now ts

        FederalJail ts ->
            temporaryStatusOrderValue 700000 now ts


temporaryStatusOrderValue : Int -> Posix -> Posix -> Int
temporaryStatusOrderValue base now ts =
    let
        diff =
            TimeDiff.between ts now

        remainingSecs : Int
        remainingSecs =
            round (TimeDiff.asSeconds diff)
                |> min 99999
                |> max 0
    in
    base + remainingSecs


getPlayerBlockClass : Posix -> Player -> String
getPlayerBlockClass now user =
    let
        ( _, _, status ) =
            getPlayerStatus now user.status
    in
    case status of
        Okay ->
            case user.onlineStatus of
                Online _ ->
                    "chain-player-ok-active"

                Idle _ ->
                    "chain-player-ok-active"

                Offline _ ->
                    "chain-player-ok"

        Hospital ts ->
            if posixDiff ts now < (3 * 60 * 1000) then
                "chain-player-hosp-short"

            else
                "chain-player-hosp"

        Abroad _ ->
            "chain-player-travel"

        TravelingTo _ ->
            "chain-player-travel"

        ReturningFrom _ ->
            "chain-player-travel"

        Jail ts ->
            if posixDiff ts now < (3 * 60 * 1000) then
                "chain-player-jail-short"

            else
                "chain-player-jail"

        FederalJail ts ->
            if posixDiff ts now < (3 * 60 * 1000) then
                "chain-player-federal-jail-short"

            else
                "chain-player-federal-jail"
