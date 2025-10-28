module OutMessage exposing
    ( GlobalMsg(..)
    , OutMessage(..)
    , batch
    , getMessages
    , none
    , setSettings
    , showApiError
    , showSettingsPage
    , single
    )

import Api.Data exposing (ApiToken)


type GlobalMsg
    = GlobalMsgApiError String
    | GlobalMsgSetSettings ApiToken Int
    | GlobalMsgOpenSettingsPage


type OutMessage
    = None
    | Single GlobalMsg
    | Batch (List GlobalMsg)


none : OutMessage
none =
    None


single : GlobalMsg -> OutMessage
single globalMsg =
    Single globalMsg


batch : List OutMessage -> OutMessage
batch list =
    let
        messages : List GlobalMsg
        messages =
            list
                |> List.map getMessages
                |> List.concat
    in
    if List.isEmpty messages then
        None

    else
        Batch messages


getMessages : OutMessage -> List GlobalMsg
getMessages outMessage =
    case outMessage of
        None ->
            []

        Single globalMsg ->
            [ globalMsg ]

        Batch list ->
            list


showApiError : String -> OutMessage
showApiError apiError =
    single (GlobalMsgApiError apiError)


setSettings : ApiToken -> Int -> OutMessage
setSettings apiToken apiRate =
    single (GlobalMsgSetSettings apiToken apiRate)


showSettingsPage : OutMessage
showSettingsPage =
    single GlobalMsgOpenSettingsPage
