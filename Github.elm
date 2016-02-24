module Github (..) where
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
  case action of 
    KeyPress value -> 
      ({model | input = value}, Effects.none)
    Submit -> 
      (model, submitEffects model.input)
    OnSubmit result ->
      (Model model.input (Result.map totalStars result |> Result.toMaybe |> Maybe.withDefault 0), Effects.none)
    _ -> 
      (model, Effects.none)

type alias Stargazers =
  { stargazers_count: Int }

httpString : String -> String
httpString user = 
  "https://api.github.com/users/" ++ user ++ "/repos?per_page=100"

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

containerStyle: Html.Attribute
containerStyle =
  style
  [ ("display", "flex")
  , ("alignItems", "center")
  , ("justifyContent", "center")
  , ("textAlign", "center")
  , ("fontFamily", "'Helvetica Neue', Helvetica, Arial, sans-serif")
  , ("flexDirection", "column")
  ]

h1Style : Html.Attribute
h1Style = 
  style
  [ ("fontSize", "56px") 
  , ("color", "#343434")
  ]

starsStyle : Html.Attribute
starsStyle =
  style
  [ ("fontSize", "56px")
  , ("fontWeight", "600")
  , ("color", "#343434")
  ]

inputStyle: Html.Attribute
inputStyle = 
  style
  [ ("width", "40vw")
  , ("height", "15vh")
  , ("fontSize", "4vw")
  , ("textAlign", "center")
  , ("fontWeight", "300")
  ]

star : Model -> Html.Html
star model =
  if model.stars > 0 then div [starsStyle] [text (toString model.stars)] else text ""

view : Signal.Address Action -> Model -> Html
view address model =
  div [containerStyle] [
    h1 [h1Style] [text "enter your github username and press enter"]
    ,
    input 
      [ placeholder "conorhastings"
      , value model.input
      , on "input" targetValue (Signal.message address << KeyPress)
      , onEnter address Submit
      , inputStyle
      ]
      []
    , star model
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

port title : String
port title = "Get your total github stars"

port fetch : Signal (Task.Task Never ())
port fetch =
  app.tasks