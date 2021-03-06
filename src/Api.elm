module Api exposing (..)

import Http
import HttpBuilder exposing (..)
import RemoteData exposing (..)
import Json.Encode
import Json.Decode exposing (int, string, bool, float, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Formatting exposing ((<>))
import Formatting as F


decodeFan : Decoder Fan
decodeFan =
    decode Fan
        |> required "id" decodeFanId
        |> required "info" decodeDeviceResponse
        |> required "status" decodeControlResponse


decodeFanId : Decoder FanId
decodeFanId =
    decode FanId
        |> required "ip" string
        |> required "uid" string


decodeDeviceResponse : Decoder DeviceResponse
decodeDeviceResponse =
    decode DeviceResponse
        |> required "name" string
        |> required "version" string
        |> required "config" string
        |> required "model" string
        |> required "role" string
        |> required "online" string
        |> required "status" string
        |> required "hubid" string


decodeControlResponse : Decoder ControlResponse
decodeControlResponse =
    decode ControlResponse
        |> required "mode" string
        |> required "sequence" string
        |> required "speed" string
        |> required "duration" string
        |> required "started" string
        |> required "remaining" string
        |> required "source" string


decodeFans : Decoder Fans
decodeFans =
    decode Fans
        |> required "fans" (Json.Decode.list decodeFan)


type Power
    = On
    | Off


type Speeds
    = One
    | Two
    | Three


type CurrentSpeed
    = High
    | Medium
    | Low


type alias FanId =
    { ip : String
    , uid : String
    }


type alias ControlResponse =
    { mode : String
    , sequence : String
    , speed : String
    , duration : String
    , started : String
    , remaining : String
    , source : String
    }


type alias DeviceResponse =
    { name : String
    , version : String
    , config : String
    , model : String
    , role : String
    , online : String
    , status : String
    , hubid : String
    }


type alias Fan =
    { id : FanId
    , info : DeviceResponse
    , status : ControlResponse
    }


type alias Fans =
    { fans : List Fan
    }


apiGet : String -> Decoder a -> Cmd (WebData a)
apiGet url x =
    get url
        |> withExpect (Http.expectJson x)
        |> toRequest
        |> RemoteData.sendRequest


apiPost : String -> Decoder a -> Json.Encode.Value -> Cmd (WebData a)
apiPost url x body =
    post url
        |> withJsonBody body
        |> withExpect (Http.expectJson x)
        |> toRequest
        |> RemoteData.sendRequest


fanPost : FanId -> String -> Decoder a -> Json.Encode.Value -> Cmd (WebData a)
fanPost id method x body =
    apiPost (fanUrl id method) x body


rootUrl : F.Format r (String -> r)
rootUrl =
    F.s "/api/fans/" <> F.string


apiUrl : F.Format r (String -> String -> String -> r)
apiUrl =
    rootUrl <> F.s "/" <> F.string <> F.s "/" <> F.string


fanUrl : FanId -> String -> String
fanUrl id method =
    F.print apiUrl id.ip id.uid method


getFans : String -> Cmd (WebData Fans)
getFans ip =
    apiGet (F.print rootUrl ip) decodeFans


updateSpeeds : FanId -> Speeds -> Cmd (WebData ControlResponse)
updateSpeeds id speed =
    let
        desiredSpeed =
            case speed of
                One ->
                    "1"

                Two ->
                    "2"

                Three ->
                    "3"

        body =
            Json.Encode.object [ ( "speeds", Json.Encode.string desiredSpeed ) ]
    in
        fanPost id "updateSpeeds" decodeControlResponse body


getCurrentSpeed fan =
    case fan.status.speed of
        "3" ->
            High

        "2" ->
            Medium

        _ ->
            Low


getAvailableSpeeds : Fan -> Speeds
getAvailableSpeeds fan =
    case fan.status.sequence of
        "4" ->
            One

        "1" ->
            Two

        _ ->
            Three


setSpeed : FanId -> CurrentSpeed -> Cmd (WebData ControlResponse)
setSpeed id speed =
    let
        desiredSpeed =
            case speed of
                High ->
                    "3"

                Medium ->
                    "2"

                Low ->
                    "1"

        body =
            Json.Encode.object [ ( "speed", Json.Encode.string desiredSpeed ) ]
    in
        fanPost id "setCurrentSpeed" decodeControlResponse body


power : FanId -> Power -> Cmd (WebData ControlResponse)
power id power =
    let
        desiredState =
            power == On

        body =
            Json.Encode.object [ ( "on", Json.Encode.bool desiredState ) ]
    in
        fanPost id "power" decodeControlResponse body
