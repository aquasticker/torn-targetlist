module Page.Settings exposing (..)

import Api.Data exposing (ApiToken, createApiToken, getApiToken)
import External.Storage as Storage
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import OutMessage exposing (OutMessage)
import Parser


type alias Model =
    { apiToken : String
    , apiRate : Int
    }


type Msg
    = MsgChangeApiToken String
    | MsgChangeApiRate Int
    | MsgSave


subscription : Model -> Sub Msg
subscription _ =
    Sub.none


init : Maybe ApiToken -> Int -> ( Model, Cmd Msg )
init apiToken apiRate =
    let
        apiTokenValue =
            apiToken
                |> Maybe.map getApiToken
                |> Maybe.withDefault ""

        model : Model
        model =
            { apiToken = apiTokenValue
            , apiRate = apiRate
            }
    in
    ( model
    , Storage.loadThrottle ()
    )


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "chain-settings-page" ]
        [ viewApiToken model.apiToken
        , viewApiRate model.apiRate
        , viewSaveButton model.apiToken
        ]


viewApiToken : String -> Html Msg
viewApiToken apiToken =
    Html.div
        []
        [ Html.label
            []
            [ Html.text "Api key" ]
        , Html.input
            [ Attr.value apiToken
            , Attr.class "chain-input"
            , Attr.placeholder "Enter Torn api key"
            , Events.onInput MsgChangeApiToken
            ]
            []
        ]


viewApiRate : Int -> Html Msg
viewApiRate rate =
    let
        options =
            List.range 1 10
                |> List.map ((*) 10)
                |> List.map (viewApiRateOption rate)

        onInput =
            Parser.run Parser.int
                >> Result.withDefault rate
                >> MsgChangeApiRate
    in
    Html.div
        []
        [ Html.label
            []
            [ Html.text "API rate" ]
        , Html.select
            [ Attr.class "chain-select"
            , Attr.placeholder "Max requests"
            , Events.onInput onInput
            ]
            options
        , Html.div
            [ Attr.class "chain-input-help" ]
            [ Html.text "Max requests per minute. Torn allows max 100 api calls per minute, shared across all API keys" ]
        ]


viewApiRateOption : Int -> Int -> Html Msg
viewApiRateOption selected rate =
    let
        str =
            String.fromInt rate
    in
    Html.option
        [ Attr.value str
        , Attr.selected (selected == rate)
        ]
        [ Html.text str ]


viewSaveButton : String -> Html Msg
viewSaveButton apiToken =
    Html.button
        [ Attr.disabled (String.isEmpty apiToken)
        , Attr.class "chain-button"
        , Events.onClick MsgSave
        ]
        [ Html.text "Save" ]


update : Msg -> Model -> ( Model, Cmd Msg, OutMessage )
update msg model =
    case msg of
        MsgChangeApiToken apiToken ->
            setModel
                { model | apiToken = apiToken }

        MsgChangeApiRate apiRate ->
            setModel
                { model | apiRate = apiRate }

        MsgSave ->
            let
                apiToken =
                    createApiToken model.apiToken
            in
            ( model
            , Cmd.batch
                [ Storage.saveApiToken apiToken
                , Storage.saveApiRate model.apiRate
                ]
            , OutMessage.setSettings apiToken model.apiRate
            )


setModel : Model -> ( Model, Cmd Msg, OutMessage )
setModel model =
    ( model
    , Cmd.none
    , OutMessage.none
    )
