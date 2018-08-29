import Browser
import Html exposing (Html, button, div, text, select, option)
import Html.Events exposing (onClick)
import Html.Attributes exposing (value)
import Http 
import Json.Decode as Decode
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

-- teacherSalary : { name: String, averageSalary: String }
-- teacherSalary = { name = "", averageSalary = "" }

teacherSalary : String
teacherSalary = ""

dataDecoder : Decode.Decoder String
dataDecoder = Decode.field "data" (Decode.field "teacherSalary" Decode.string)