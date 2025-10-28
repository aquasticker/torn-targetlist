module Model.Flags exposing (..)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


type alias Flags =
    { apiToken : Maybe String
    , apiRate : Int
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Json.Decode.succeed Flags
        |> required "apiToken" (Json.Decode.nullable Json.Decode.string)
        |> optional "apiRate" Json.Decode.int 30
