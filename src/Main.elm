module Main exposing (..)

import Api.Data exposing (ApiToken, createApiToken)
import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (errorToString)
import Model.Flags exposing (Flags, flagsDecoder)
import OutMessage exposing (GlobalMsg(..), OutMessage)
import Page.PlayerList as PlayerList
import Page.Settings as Settings


type AppModel
    = InitializedApp Model
    | UninitializedApp String


type Page
    = PagePlayerList PlayerList.Model
    | PageSettings Settings.Model


type alias Model =
    { apiToken : Maybe ApiToken
    , apiRate : Int
    , error : Maybe String
    , page : Page
    }


type Msg
    = MsgPlayerList PlayerList.Msg
    | MsgSettings Settings.Msg
    | MsgHideError


main : Program Decode.Value AppModel Msg
main =
    Browser.element
        { init = appInit
        , update = appUpdate
        , view = appView
        , subscriptions = appSubscriptions
        }



{--APP Level --}


appInit : Decode.Value -> ( AppModel, Cmd Msg )
appInit flagsInput =
    case Decode.decodeValue flagsDecoder flagsInput of
        Ok flags ->
            let
                ( model, cmd ) =
                    init flags
            in
            ( InitializedApp model
            , cmd
            )

        Err error ->
            ( UninitializedApp (errorToString error)
            , Cmd.none
            )


appView : AppModel -> Html Msg
appView appModel =
    case appModel of
        InitializedApp model ->
            view model

        UninitializedApp error ->
            viewError error


appUpdate : Msg -> AppModel -> ( AppModel, Cmd Msg )
appUpdate msg appModel =
    case appModel of
        InitializedApp model ->
            let
                ( newModel, cmd ) =
                    update msg model
            in
            ( InitializedApp newModel
            , cmd
            )

        UninitializedApp _ ->
            ( appModel, Cmd.none )


appSubscriptions : AppModel -> Sub Msg
appSubscriptions appModel =
    case appModel of
        InitializedApp model ->
            subscription model

        UninitializedApp _ ->
            Sub.none



{--Initialized level --}


subscription : Model -> Sub Msg
subscription model =
    case model.page of
        PagePlayerList subModel ->
            PlayerList.subscription subModel
                |> Sub.map MsgPlayerList

        PageSettings subModel ->
            Settings.subscription subModel
                |> Sub.map MsgSettings


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        maybeApiToken : Maybe ApiToken
        maybeApiToken =
            Maybe.map createApiToken flags.apiToken

        ( page, cmd ) =
            case maybeApiToken of
                Just _ ->
                    PlayerList.init flags.apiRate
                        |> Tuple.mapFirst PagePlayerList
                        |> Tuple.mapSecond (Cmd.map MsgPlayerList)

                Nothing ->
                    Settings.init maybeApiToken flags.apiRate
                        |> Tuple.mapFirst PageSettings
                        |> Tuple.mapSecond (Cmd.map MsgSettings)

        model : Model
        model =
            { apiToken = maybeApiToken
            , apiRate = flags.apiRate
            , error = Nothing
            , page = page
            }
    in
    ( model
    , cmd
    )


view : Model -> Html Msg
view model =
    let
        inner =
            case model.error of
                Just err ->
                    viewError err

                Nothing ->
                    case model.page of
                        PagePlayerList subModel ->
                            PlayerList.view subModel
                                |> Html.map MsgPlayerList

                        PageSettings subModel ->
                            Settings.view subModel
                                |> Html.map MsgSettings
    in
    Html.div
        [ Attr.class "cont-gray chain-main" ]
        [ inner ]


viewError : String -> Html Msg
viewError err =
    Html.div
        [ Attr.class "chain-error" ]
        [ Html.div
            [ Attr.class "chain-flex-split" ]
            [ Html.h3
                [ Attr.class "chain-error-header" ]
                [ Html.text "Error" ]
            , Html.button
                [ Attr.class "chain-button"
                , Events.onClick MsgHideError
                ]
                [ Html.text "Close" ]
            ]
        , Html.pre
            []
            [ Html.text err ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( PagePlayerList subModel, MsgPlayerList subMsg ) ->
            case model.apiToken of
                Just apiToken ->
                    PlayerList.update apiToken subMsg subModel
                        |> mapUpdate PagePlayerList MsgPlayerList model

                Nothing ->
                    ignoreMessage model

        ( PageSettings subModel, MsgSettings subMsg ) ->
            Settings.update subMsg subModel
                |> mapUpdate PageSettings MsgSettings model

        ( _, MsgHideError ) ->
            ( { model | error = Nothing }
            , Cmd.none
            )

        _ ->
            ignoreMessage model


mapUpdate : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg, OutMessage ) -> ( Model, Cmd Msg )
mapUpdate modelTagger msgTagger model ( subModel, subCmd, out ) =
    let
        newPage : Page
        newPage =
            modelTagger subModel

        newModel : Model
        newModel =
            { model | page = newPage }

        newCmd : Cmd Msg
        newCmd =
            Cmd.map msgTagger subCmd
    in
    processOutMessage out
        ( newModel
        , newCmd
        )


setModel : Model -> ( Model, Cmd Msg )
setModel model =
    ( model
    , Cmd.none
    )


ignoreMessage : Model -> ( Model, Cmd Msg )
ignoreMessage =
    setModel


processOutMessage : OutMessage -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
processOutMessage outMessage carry =
    outMessage
        |> OutMessage.getMessages
        |> List.foldl processGlobalMessage carry


processGlobalMessage : GlobalMsg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
processGlobalMessage globalMsg ( model, cmd ) =
    case globalMsg of
        GlobalMsgApiError apiError ->
            ( { model | error = Just apiError }
            , cmd
            )

        GlobalMsgSetSettings apiToken apiRate ->
            let
                ( pageModel, pageCmd ) =
                    PlayerList.init model.apiRate
            in
            ( { model
                | apiToken = Just apiToken
                , apiRate = apiRate
                , page = PagePlayerList pageModel
              }
            , Cmd.batch
                [ cmd
                , Cmd.map MsgPlayerList pageCmd
                ]
            )

        GlobalMsgOpenSettingsPage ->
            let
                ( pageModel, pageCmd ) =
                    Settings.init model.apiToken model.apiRate
            in
            ( { model | page = PageSettings pageModel }
            , Cmd.batch
                [ cmd
                , Cmd.map MsgSettings pageCmd
                ]
            )
