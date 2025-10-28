module Utils.Throttle exposing (Throttle, ThrottleHistory, calculateIntervalRate, canExecute, decodeThrottleHistory, encodeThrottle, getRate, init, setHistory)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import List.Extra
import Time exposing (Posix(..))


type Throttle
    = Throttle Data


type ThrottleHistory
    = ThrottleHistory (List Posix)


type alias Data =
    { rate : Int
    , list : List Posix
    }


setHistory : ThrottleHistory -> Throttle -> Throttle
setHistory (ThrottleHistory list) (Throttle throttle) =
    Throttle { throttle | list = list }


decodeThrottleHistory : Decoder ThrottleHistory
decodeThrottleHistory =
    Decode.oneOf
        [ Decode.list posixDecoder
        , Decode.succeed []
        ]
        |> Decode.map ThrottleHistory


encodeThrottle : Throttle -> Json.Encode.Value
encodeThrottle (Throttle data) =
    encodeRecord1 data


encodePosix : Posix -> Json.Encode.Value
encodePosix posix =
    Json.Encode.int (Time.posixToMillis posix)


encodeRecord1 : Data -> Json.Encode.Value
encodeRecord1 data =
    Json.Encode.list encodePosix data.list


init : Int -> Throttle
init rate =
    Throttle
        { rate = rate
        , list = []
        }


canExecute : Posix -> Throttle -> Maybe Throttle
canExecute now (Throttle { rate, list }) =
    let
        ( interval, max ) =
            calculateIntervalRate rate

        threshold : Int
        threshold =
            Time.posixToMillis now - interval

        after : Posix -> Bool
        after posix =
            Time.posixToMillis posix > threshold

        cleanedList : List Posix
        cleanedList =
            list
                |> List.Extra.takeWhile after

        len : Int
        len =
            List.length cleanedList
    in
    if len < max && ensureSpacing now interval max cleanedList then
        Just
            (Throttle
                { rate = rate
                , list = now :: cleanedList
                }
            )

    else
        Nothing


ensureSpacing : Posix -> Int -> Int -> List Posix -> Bool
ensureSpacing now interval max list =
    case List.head list of
        Just lastPosix ->
            let
                minDelay : Int
                minDelay =
                    floor (toFloat interval / (toFloat max * 4.0))

                lastTs =
                    Time.posixToMillis lastPosix

                nowTs =
                    Time.posixToMillis now
            in
            lastTs < (nowTs - minDelay)

        Nothing ->
            True


posixDecoder : Decoder Posix
posixDecoder =
    Decode.int
        |> Decode.map Time.millisToPosix


getRate : Throttle -> Int
getRate (Throttle data) =
    data.rate


calculateIntervalRate : Int -> ( Int, Int )
calculateIntervalRate inputRate =
    let
        denominator =
            getMinDenominator inputRate

        interval =
            round (60000.0 / denominator)

        rate =
            round (toFloat inputRate / denominator)
    in
    ( interval, rate )


getMinDenominator : Int -> Float
getMinDenominator val =
    [ 5, 3, 2 ]
        |> List.Extra.find (\v -> modBy v val == 0)
        |> Maybe.withDefault 1
        |> toFloat
