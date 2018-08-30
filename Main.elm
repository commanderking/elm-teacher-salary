import Browser
import Html exposing (Html, button, div, text, select, option)
import Html.Events exposing (onClick)
import Html.Attributes exposing (value)
import Http 
import Json.Decode as Decode exposing(string, list)
import Url.Builder as Url

main =
  Browser.sandbox { init = init, update = update, view = view }

names = ["Abington", "Arlington", "Lexington"]

districtOption name = 
    option [ value name ] [ text name ]

-- MODEL

type alias Model = { incrementValue : Int }

init : Model
init = { incrementValue = 0 }


-- UPDATE

type Msg = Increment | Decrement | Reset
-- Takes two arguments, msg and model and returns model
update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model | incrementValue = model.incrementValue + 1 }

    Decrement ->
      { model | incrementValue = model.incrementValue - 1 }
    Reset -> 
      { model | incrementValue = 0 }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.incrementValue) ]
    , button [ onClick Increment ] [ text "+" ]
    , button [ onClick Reset ] [ text "Reset" ]
    , select []
        (List.map districtOption (names))
    ]
        
graphQLUrl : String
graphQLUrl = "localhost:4000/graphql" 

type alias TeacherSalary =
  { name : String
  , averageSalary : String
  }

dataDecoder : Decode.Decoder TeacherSalary
dataDecoder =
  Decode.map2 TeacherSalary
    (Decode.field "name" string)
    (Decode.field "averageSalary" string)

postTeacherSalary : Http.Request (List TeacherSalary)
postTeacherSalary = Http.post graphQLUrl Http.emptyBody (list dataDecoder)
