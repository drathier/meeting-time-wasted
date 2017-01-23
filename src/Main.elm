import Html exposing (program, div, button, text)
import Html.Events exposing (onClick)
import Time exposing (Time, every, millisecond)


main =
  program {
    init = ({people=1, time=0}, Cmd.none),
    view = view,
    update = update,
    subscriptions = subscriptions
    }


view model =
  div []
    [ button [ onClick Increment ] [ text "+" ]
    , div [] [ text (toString model) ]
    , button [ onClick Decrement ] [ text "-" ]
    ]


type alias Model = {
  people : Int,
  time : Float
  }


type Msg = Increment | Decrement | Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      ({model | people = model.people + 1}, Cmd.none)

    Decrement ->
      if model.people > 1 then
        ({model | people = model.people - 1}, Cmd.none)
      else
        (model, Cmd.none)

    Tick time ->
      ({model | time = model.time + 1}, Cmd.none)

subscriptions model =
  every (toFloat (1000//model.people) * millisecond) Tick
