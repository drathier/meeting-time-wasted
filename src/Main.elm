module Main exposing (..)

import Html exposing (program, div, button, text)
import Html.Events exposing (onClick)
import Time exposing (Time, every, hour, minute, second, millisecond, inHours, inMinutes, inSeconds)


main =
    program
        { init = ( { people = 1, time = 0 }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view model =
    div []
        [ div [] [ text <| String.join ":" <| List.map leftPad2 <| timeToIntList model.time ]
        , div []
            [ button [ onClick Increment ] [ text "+" ]
            , div [] [ text (toString model) ]
            , button [ onClick Decrement ] [ text "-" ]
            ]
        ]


leftPad2 : Int -> String
leftPad2 n =
    if n == 0 then
        "00"
    else if n < 10 then
        "0" ++ toString n
    else
        toString n


timeToIntList : Time -> List Int
timeToIntList t =
    timeToIntListImpl t ""


timeToIntListImpl : Time -> String -> List Int
timeToIntListImpl t last =
    let
        d =
            truncate <| (inHours t) / 24

        h =
            truncate <| inHours t

        m =
            truncate <| inMinutes t

        s =
            truncate <| inSeconds t
    in
        if d >= 1 then
            [ d ] ++ timeToIntListImpl (t - ((toFloat d) * 24 * hour)) "d"
        else if last == "d" || h >= 1 then
            [ h ] ++ timeToIntListImpl (t - ((toFloat h) * hour)) "h"
        else if last == "h" || m >= 1 then
            [ m ] ++ timeToIntListImpl (t - ((toFloat m) * minute)) ""
        else
            [ s ]


type alias Model =
    { people : Int
    , time : Time
    }


type Msg
    = Increment
    | Decrement
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            if model.people < 1000 then
                ( { model | people = model.people + 1 }, Cmd.none )
            else
                ( model, Cmd.none )

        Decrement ->
            if model.people > 1 then
                ( { model | people = model.people - 1 }, Cmd.none )
            else
                ( model, Cmd.none )

        Tick t ->
            ( { model | time = model.time + 1 * second }, Cmd.none )


subscriptions model =
    every (toFloat (1000 // model.people) * millisecond) Tick
