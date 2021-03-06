import Browser
import Html exposing (Html, button, div, text, select, option, ul)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value)
import Http
import Json.Decode as Decode exposing(string, list, int)

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL
type alias SalaryData = { 
    name : String, 
    averageSalary : String }

type alias DistrictData = {
    name: String,
    code: Int }

type alias Model = { 
    incrementValue : Int
    , districts: List DistrictData
    , districtCode: String
    , salaryData: List SalaryData }

init : () -> (Model, Cmd Msg)
init _ = ({ 
    incrementValue = 0, 
    districts = [],
    districtCode = "10000", 
    salaryData = [] }, 
    Cmd.batch [(getTeacherSalary "10000"), getDistricts ]
    )

updateCode model code = { model | districtCode = code }


-- UPDATE
type Msg = 
  UpdateDistrict String | 
  ApplyDistrict | 
  FetchTeacherSalary (Result Http.Error (List SalaryData)) |
  FetchDistricts (Result Http.Error (List DistrictData))

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
          ( model
          , Cmd.none
          )
    FetchDistricts result -> 
      case result of 
        Ok districtsData -> 
          -- TODO: Looks like some districts don't have salary data?
          ( { model | districts = districtsData }
          , Cmd.none
          )
        Err _ -> 
          ( model
          , Cmd.none
          )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

renderSalaryData salaryData =  
    div [] [ text (salaryData.name ++ ": " ++ salaryData.averageSalary ) ]
renderDistrictOption district = 
    option [ value (String.fromInt district.code) ] [ text district.name ]


view : Model -> Html Msg
view model =
  div []
    [ select [ onInput UpdateDistrict]
        (List.map renderDistrictOption (model.districts))
    , div [] [ text model.districtCode ]
    , button [ onClick ApplyDistrict ] [ text "Apply" ]
    , ul []
     ( List.map renderSalaryData (model.salaryData) )
    ]

salaryDataDecoder : Decode.Decoder (List SalaryData)
salaryDataDecoder =
  Decode.list (Decode.map2 SalaryData
    (Decode.field "name" string)
    (Decode.field "averageSalary" string))

salaryQuery : String -> String
salaryQuery code = "{ teacherSalaries(codes: [" ++ code ++ "]) { name averageSalary } }"

requestSalaryData : String -> Http.Request (List SalaryData)
requestSalaryData code = 
  let 
    teacherSalaryDecoder = Decode.at [ "data", "teacherSalaries" ] <| salaryDataDecoder
  in 
    -- TODO: Refactor graphql link to common string
    Http.get ("http://localhost:4000/graphql?query=" ++ (salaryQuery code)) teacherSalaryDecoder


getTeacherSalary : String -> Cmd Msg
getTeacherSalary code =
  Http.send FetchTeacherSalary (requestSalaryData code)

-- GETTING DISTRICTS 
districtsDataDecoder : Decode.Decoder (List DistrictData)
districtsDataDecoder =
  Decode.list (Decode.map2 DistrictData
      (Decode.field "name" string)
      (Decode.field "code" int)
  )

districtsQuery : String
districtsQuery = "{ allDistricts { name code } }"

requestDistrictData = 
  let 
    districtDecoder = Decode.at ["data", "allDistricts" ] <| districtsDataDecoder 
  in 
        Http.get ("http://localhost:4000/graphql?query=" ++ districtsQuery) districtDecoder


getDistricts : Cmd Msg
getDistricts = 
    Http.send FetchDistricts (requestDistrictData)
