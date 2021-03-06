module Main exposing (..)

import List
import Debug
import Dict exposing (..)
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (href, src)
import RemoteData exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Badge as Badge
import Api exposing (Fan, Fans, Power, Speeds)


type alias Model =
    { fans : WebData (Dict String Api.Fan)
    , ip : String
    }


getFans : String -> Cmd Msg
getFans ip =
    Api.getFans ip |> Cmd.map FansResponse


init : ( Model, Cmd Msg )
init =
    let
        initialIp =
            "10.0.0.151"
    in
        { ip = initialIp, fans = NotAsked } ! [ getFans initialIp ]


type Msg
    = Refresh
    | FansResponse (WebData Fans)
    | FanControlResponse Api.FanId (WebData Api.ControlResponse)
    | FanDeviceResponse Api.FanId (WebData Api.DeviceResponse)
    | UpdateSpeeds Speeds Fan
    | SetPower Power Fan
    | SetSpeed Api.CurrentSpeed Fan


toDict : Api.Fans -> Dict String Api.Fan
toDict fans =
    fans.fans
        |> List.map (\f -> ( f.id.uid, f ))
        |> Dict.fromList


updateStatus : Api.ControlResponse -> Api.Fan -> Api.Fan
updateStatus resp fan =
    { fan | status = resp }


updateInfo : Api.DeviceResponse -> Api.Fan -> Api.Fan
updateInfo resp fan =
    { fan | info = resp }


updateFan : (a -> Api.Fan -> Api.Fan) -> Api.FanId -> WebData a -> Model -> Model
updateFan updateMethod id webResponse model =
    let
        dictUpdate response fans =
            Dict.update id.uid (Maybe.map (updateMethod response)) fans

        updatedFans =
            RemoteData.map2 dictUpdate webResponse model.fans
    in
        { model | fans = updatedFans }


updateFanStatus =
    updateFan updateStatus


updateFanInfo =
    updateFan updateInfo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            { model | fans = Loading } ! [ getFans model.ip ]

        FansResponse response ->
            let
                dict =
                    response |> RemoteData.map toDict
            in
                { model | fans = dict } ! []

        FanDeviceResponse id response ->
            updateFanInfo id response model ! []

        FanControlResponse id response ->
            updateFanStatus id response model ! []

        SetPower power fan ->
            model ! [ Api.power fan.id power |> Cmd.map (FanControlResponse fan.id) ]

        SetSpeed speed fan ->
            model ! [ Api.setSpeed fan.id speed |> Cmd.map (FanControlResponse fan.id) ]

        UpdateSpeeds speed fan ->
            model ! [ Api.updateSpeeds fan.id speed |> Cmd.map (FanControlResponse fan.id) ]



---- VIEW ----


powerButton : Fan -> Html Msg
powerButton fan =
    let
        button style msg txt =
            Button.linkButton
                [ style, Button.attrs [ href "#" ], Button.onClick (msg fan) ]
                [ text txt ]

        isOn =
            not (fan.status.remaining == "0")
    in
        if isOn then
            button Button.danger (SetPower Api.Off) "Turn Off"
        else
            button Button.success (SetPower Api.On) "Turn On"


currentSpeed : Fan -> Html Msg
currentSpeed fan =
    let
        available =
            Api.getAvailableSpeeds fan

        speedCount =
            case available of
                Api.One ->
                    1

                Api.Two ->
                    2

                Api.Three ->
                    3

        speeds =
            Api.getCurrentSpeed fan

        radio speed txt =
            ButtonGroup.radioButton
                (speeds == speed)
                [ Button.primary, Button.onClick <| SetSpeed speed fan ]
                [ text txt ]

        speedList =
            [ radio Api.High "High"
            , radio Api.Medium "Medium"
            , radio Api.Low "Low"
            ]
                |> List.take speedCount
    in
        div []
            [ text "Current speed"
            , ButtonGroup.radioButtonGroup [] speedList
            ]


speedControls : Fan -> Html Msg
speedControls fan =
    let
        speeds =
            Api.getAvailableSpeeds fan

        radio speed txt =
            ButtonGroup.radioButton
                (speeds == speed)
                [ Button.primary, Button.onClick <| UpdateSpeeds speed fan ]
                [ text txt ]
    in
        div []
            [ text "Available Speeds: "
            , ButtonGroup.radioButtonGroup []
                [ radio Api.One "1"
                , radio Api.Two "2"
                , radio Api.Three "3"
                ]
            ]


fanCard : Fan -> Grid.Column Msg
fanCard fan =
    Grid.col []
        [ Card.config []
            |> Card.headerH4 [] [ text fan.info.name ]
            |> Card.block []
                [ Card.custom <| powerButton fan
                , Card.custom <| currentSpeed fan
                , Card.custom <| speedControls fan
                ]
            |> Card.view
        ]


view : Model -> Html Msg
view model =
    Grid.container []
        -- Responsive fixed width container
        [ CDN.stylesheet -- Inlined Bootstrap CSS for use with reactor
        , mainContent model
        ]


mainContent : Model -> Html Msg
mainContent model =
    case model.fans of
        NotAsked ->
            text "Loading ..."

        Loading ->
            text "Loading ..."

        Failure err ->
            text ("Error: " ++ toString err)

        Success fans ->
            fans
                |> Dict.values
                |> List.sortBy (\x -> x.info.hubid)
                |> List.map fanCard
                |> Grid.row []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
