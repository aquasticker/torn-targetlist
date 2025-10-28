module Model.Life exposing (Life, LifeValue, createLife, encodeLife, getLife, lifeDecoder)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Time exposing (Posix(..))


type Life
    = Life InnerData


type alias InnerData =
    { current : Int
    , maximum : Int
    , ts : Posix
    }


type alias LifeValue =
    { current : Int
    , maximum : Int
    }


createLife : Posix -> Int -> Int -> Life
createLife ts current maximum =
    Life
        { current = current
        , maximum = maximum
        , ts = ts
        }


getLife : Posix -> Life -> LifeValue
getLife now (Life lifeData) =
    let
        nowTs =
            Time.posixToMillis now

        snapshotTs =
            Time.posixToMillis lifeData.ts
    in
    if nowTs < snapshotTs then
        { current = lifeData.current
        , maximum = lifeData.maximum
        }

    else
        calculateLife snapshotTs nowTs lifeData


calculateLife : Int -> Int -> InnerData -> LifeValue
calculateLife snapshotTs ts life =
    let
        firstIncrement : Int
        firstIncrement =
            ceiling (toFloat snapshotTs / 300000.0)

        lastIncrement : Int
        lastIncrement =
            ceiling (toFloat ts / 300000.0)

        increments : Int
        increments =
            max 0 (lastIncrement - firstIncrement)

        incrementIncrease : Int
        incrementIncrease =
            floor (toFloat life.maximum * 0.06)

        currentLife : Int
        currentLife =
            min life.maximum (life.current + incrementIncrease * increments)
    in
    { current = currentLife
    , maximum = life.maximum
    }


encodeLife : Life -> Json.Encode.Value
encodeLife (Life { current, maximum, ts }) =
    Json.Encode.object <|
        [ ( "current", Json.Encode.int current )
        , ( "maximum", Json.Encode.int maximum )
        , ( "ts", Json.Encode.int (Time.posixToMillis ts) )
        ]


lifeDecoder : Json.Decode.Decoder Life
lifeDecoder =
    Json.Decode.succeed InnerData
        |> Json.Decode.Pipeline.required "current" Json.Decode.int
        |> Json.Decode.Pipeline.required "maximum" Json.Decode.int
        |> Json.Decode.Pipeline.required "ts" (Json.Decode.int |> Json.Decode.map Time.millisToPosix)
        |> Json.Decode.map Life
