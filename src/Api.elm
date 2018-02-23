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


apiGet url x =
    get url
        |> withExpect (Http.expectJson x)
        |> toRequest
        |> RemoteData.sendRequest


apiPost url x body =
    post url
        |> withJsonBody (Json.Encode.object body)
        |> withExpect (Http.expectJson x)
        |> toRequest
        |> RemoteData.sendRequest


rootUrl : F.Format r (String -> r)
rootUrl =
    F.s "/api/fans/" <> F.string


fanUrl : F.Format r (String -> String -> String -> r)
fanUrl =
    rootUrl <> F.s "/" <> F.string <> F.s "/" <> F.string


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
    in
        apiPost (F.print fanUrl id.ip id.uid "updateSpeeds") decodeControlResponse [ ( "speeds", Json.Encode.string desiredSpeed ) ]


power : FanId -> Power -> Cmd (WebData ControlResponse)
power id power =
    let
        desiredState =
            if power == On then
                "on"
            else
                "off"
    in
        apiPost (F.print fanUrl id.ip id.uid desiredState) decodeControlResponse []
