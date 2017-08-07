module Decoders exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Types exposing (..)


decodeDump : Decode.Decoder Dump
decodeDump =
    decode Dump
        |> required "state" decodeState
        |> required "moves" decodeMoves


decodeMoves : Decode.Decoder (List Move)
decodeMoves =
    Decode.list decodeMove


decodeState : Decode.Decoder State
decodeState =
    decode State
        |> required "map" decodeMap
        |> required "punter" Decode.int
        |> required "punters" Decode.int


decodeMap : Decode.Decoder Map
decodeMap =
    decode Map
        |> required "mines" (Decode.list Decode.int)
        |> required "sites" (Decode.list decodeSite)
        |> required "rivers" (Decode.list decodeRiver)


decodeSite : Decode.Decoder Site
decodeSite =
    decode Site
        |> required "id" Decode.int
        |> required "x" Decode.float
        |> required "y" Decode.float


decodeRiver : Decode.Decoder River
decodeRiver =
    decode River
        |> required "source" Decode.int
        |> required "target" Decode.int


decodeMove : Decode.Decoder Move
decodeMove =
    Decode.oneOf
        [ decodeClaim
        , decodePass
        ]


decodeClaim : Decode.Decoder Move
decodeClaim =
    Decode.map Claim <|
        Decode.map3 ClaimMove
            (Decode.at [ "claim", "punter" ] Decode.int)
            (Decode.at [ "claim", "source" ] Decode.int)
            (Decode.at [ "claim", "target" ] Decode.int)


decodePass : Decode.Decoder Move
decodePass =
    Decode.map Pass
        (Decode.field "punter" Decode.int)
