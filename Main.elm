import Browser
import Html exposing (Html, button, div, text, select, option, ul)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value)
import Http
import Json.Decode as Decode exposing(string, list)
import Url.Builder as Url
import Maybe
main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

districts = [ {name = "Abington", code = "10000"}, { name = "Arlington", code = "20000"}]

districtOption district = 
    option [ value district.code ] [ text district.name ]

-- MODEL
type alias SalaryData = { name : String, averageSalary : String }
type alias Model = { incrementValue : Int, districtCode: String, salaryData: List SalaryData }
init : () -> (Model, Cmd Msg)
init _ = ({ incrementValue = 0, districtCode = "", salaryData = [] }, Cmd.none)
updateCode model code = { model | districtCode = code }
-- UPDATE

type Msg = UpdateDistrict String | ApplyDistrict | FetchTeacherSalary (Result Http.Error (List SalaryData))
-- Takes two arguments, msg and model and returns model
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateDistrict code ->
      (updateCode model code, Cmd.none)
    ApplyDistrict ->
      ( model, getTeacherSalary model.districtCode )
    FetchTeacherSalary result ->
      case result of
        Ok newSalaryData ->
          ( { model | salaryData = newSalaryData }
          , Cmd.none
          )

        Err _ ->
          Debug.log (Debug.toString result)
          Debug.log "error"
          ( model
          , Cmd.none
          )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

renderSalaries salaryData =  
    div [ value salaryData.name ] [ text salaryData.averageSalary ]

view : Model -> Html Msg
view model =
  div []
    [ select [ onInput UpdateDistrict]
        (List.map districtOption (districts))
    , div [] [ text model.districtCode ]
    , button [ onClick ApplyDistrict ] [ text "Apply" ]
    , ul []
     ( List.map renderSalaries (model.salaryData) )
    ]
        
graphQLUrl : String
graphQLUrl = "http://localhost:4000/graphql"

type alias TeacherSalary =
  { name : String
  , averageSalary : String
  }

dataDecoder : Decode.Decoder TeacherSalary
dataDecoder =
  Decode.map2 TeacherSalary
    (Decode.field "name" string)
    (Decode.field "averageSalary" string)


dataListDecoder : Decode.Decoder (List TeacherSalary)
dataListDecoder = 
  Decode.list dataDecoder

salaryQuery : String
salaryQuery =
  """
  { teacherSalaries(codes: [10000, 30000]) { name averageSalary } }
  """

{-
    Might be needed for posting
postTeacherSalary : Http.Request (List TeacherSalary)
postTeacherSalary = Http.post graphQLUrl Http.emptyBody (list dataDecoder)
-}

request : Http.Request (List TeacherSalary)
request = 
  let 
    encoded = salaryQuery
  in 
    Http.get ("http://localhost:4000/graphql?query=" ++ encoded) dataListDecoder


getTeacherSalary : String -> Cmd Msg
getTeacherSalary code =
  Http.send FetchTeacherSalary request
