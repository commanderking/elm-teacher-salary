import Browser
import Html exposing (Html, button, div, text, select, option)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value)
import Http 
import Json.Decode as Decode exposing(string, list)
import Url.Builder as Url
import Maybe
main =
  Browser.sandbox { init = init, update = update, view = view }

districts = [ {name = "Abington", code = "10000"}, { name = "Arlington", code = "20000"}]

districtOption district = 
    option [ value district.code ] [ text district.name ]

-- MODEL
type alias Model = { incrementValue : Int, districtCode: String }
init : Model
init = { incrementValue = 0, districtCode = "" }


-- UPDATE

type Msg = UpdateDistrict String
-- Takes two arguments, msg and model and returns model
update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateDistrict code ->
      { model | districtCode = code }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ select [ onInput UpdateDistrict]
        (List.map districtOption (districts))
    , div [] [ text model.districtCode ]
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
