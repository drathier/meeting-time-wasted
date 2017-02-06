module Main exposing (..)

import Html exposing (a, button, div, input, program, text)
import Html.Attributes exposing (href, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time, every, hour, minute, second, millisecond, inHours, inMinutes, inSeconds)


main =
    program
        { init = ( { people = 1, time = 0, emptyPeopleInput = False }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view model =
    div []
        [ div
            [ style
                [ ( "position", "absolute" )
                , ( "top", "50%" )
                , ( "left", "50%" )
                , ( "transform", "translate(-50%, -50%)" )
                , ( "font-size", "20vmin" )
                , ( "font-family", "'Lucida Console', Consolas, monospace" )
                ]
            ]
            [ text <|
                String.concat
                    [ if model.time < 0 then
                        "-"
                      else
                        ""
                    , String.join ":" <|
                        List.map leftPad2 <|
                            timeToIntList <|
                                abs model.time
                    ]
            ]
        , div
            [ style
                [ ( "position", "fixed" )
                , ( "bottom", "0px" )
                , ( "float", "left" )
                ]
            ]
            [ input
                [ style
                    [ ( "float", "left" )
                    , ( "width", "3rem" )
                    ]
                , placeholder "1"
                , onInput PeopleCountChange
                , type_ "number"
                , value <|
                    if model.emptyPeopleInput then
                        ""
                    else
                        toString model.people
                ]
                []
            , div
                [ style
                    [ ( "float", "left" )
                    , ( "padding-left", "5px" )
                    ]
                ]
                [ text ("People") ]
            ]
        , div
            [ style
                [ ( "position", "fixed" )
                , ( "bottom", "0px" )
                , ( "right", "0px" )
                , ( "margin", "0.3em" )
                ]
            ]
            [ a
                [ href "https://github.com/drathier/meeting-time-wasted"
                ]
                [ text "Source is on Github" ]
            ]
        ]


type alias Model =
    { people : Int
    , time : Time
    , emptyPeopleInput : Bool
    }


type Msg
    = PeopleCountChange String
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PeopleCountChange newCountStr ->
            case String.toInt newCountStr of
                Err _ ->
                    ( { model | people = 1, emptyPeopleInput = True }, Cmd.none )

                Ok newCount ->
                    let
                        count =
                            clamp -1000 1000 newCount
                    in
                        ( { model | people = count, emptyPeopleInput = False }, Cmd.none )

        Tick t ->
            let
                diff =
                    if model.people < 0 then
                        -1
                    else
                        1
            in
                ( { model | time = model.time + diff * second }, Cmd.none )


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
    timeToIntListImpl t "d"


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


subscriptions model =
    if model.people == 0 then
        Sub.none
    else
        every (toFloat (1000 // abs model.people) * millisecond) Tick
