module Main (..) where
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing ((:=))
import Http
import Task
import Effects exposing (Effects, Never)
import Debug
import StartApp

type alias Model =
  {input: String , stars: Int}

type Action = 
  NoOp |
  KeyPress String  |
  Submit |
  OnSubmit (Result Http.Error (List Stargazers))

update : Action -> Model -> (Model, Effects.Effects Action)
update action model = 
  case Debug.log "action" action of 
    KeyPress value -> 
      ({model | input = value}, Effects.none)
    Submit -> 
      (model, submitEffects model.input)
    OnSubmit result ->
      (Model model.input (Result.map totalStars result|> Result.toMaybe |> Maybe.withDefault 0), Effects.none)
    _ -> 
      (model, Effects.none)

type alias Stargazers =
  { stargazers_count: Int }

httpString : String -> String
httpString user = 
  "https://api.github.com/users/" ++ user ++ "/repos"

httpTask : String -> Task.Task Http.Error (List Stargazers)
httpTask user =
  Http.get decodeResult (httpString user)

decodeResult : Json.Decoder (List Stargazers)
decodeResult =
  Json.list <| 
    Json.object1 Stargazers
      ("stargazers_count" := Json.int)

totalStars : (List Stargazers) -> Int
totalStars list = 
  List.sum (List.map .stargazers_count list)

submitEffects : String -> Effects.Effects Action
submitEffects user = 
  (httpTask user)
  |> Task.toResult
  |> Task.map OnSubmit
  |> Effects.task


onEnter : Signal.Address a -> a -> Attribute
onEnter address value = 
  on "keydown" 
    (Json.customDecoder keyCode is13)
    (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 key =
  if key == 13 then Ok () else Err "incorrect keyCode"

view : Signal.Address Action -> Model -> Html
view address model =
  div [] [
    input 
      [ placeholder "conorhastings"
      , value model.input
      , on "input" targetValue (Signal.message address << KeyPress)
      , onEnter address Submit
      ]
      []
    ,
    div [] [text (toString model.stars)]
  ]

init : (Model, Effects Action)
init = 
  ({ input = "", stars = 0}, Effects.none)

app : StartApp.App Model
app =
  StartApp.start
  { init = init
  , inputs = []
  , update = update
  , view = view
  }

main : Signal.Signal Html.Html
main = 
  app.html

port fetch : Signal (Task.Task Never ())
port fetch =
  app.tasks