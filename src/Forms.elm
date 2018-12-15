import Browser
import Parser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" ""



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    -- , viewInput "password" "Password" model.password Password
    , viewInput "text" "Password" model.password Password
    -- , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewInput "text" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  checkPasswordEquality model.password model.passwordAgain
    |> checkPasswordLength
    |> verifyPasswordContainsUppercaseCharacters
    |> verifyPasswordContainsLowercaseCharacters
    |> verifyPasswordContainsNumbers
    |> outputHtmlDiv
    
type FormValidation
  = Success String
  | Failure String

outputHtmlDiv: FormValidation -> Html msg
outputHtmlDiv validation =
  case validation of
    Failure message ->
      div [ style "color" "red" ] [ text message ]
    Success _ ->
      div [ style "color" "green" ] [ text "OK" ]

checkPasswordEquality : String -> String -> FormValidation
checkPasswordEquality password passwordAgain =
  if password == passwordAgain then
    Success password
  else
    Failure "Passwords do not match!"

checkPasswordLength : FormValidation -> FormValidation
checkPasswordLength previousValidation =
  case previousValidation of
    Failure message -> Failure message
    Success password ->
      if String.length password < 8 then
        Failure "Password must be at least 8 characters"
      else
        Success password

verifyPasswordContainsRequiredCharacters : FormValidation -> String -> String -> FormValidation
verifyPasswordContainsRequiredCharacters previousValidation pattern error =
  case previousValidation of
    Failure message -> Failure message
    Success password ->
      let
        regex = Maybe.withDefault Regex.never <| Regex.fromString pattern
        contains = Regex.contains regex password
      in
        case contains of
          True -> Success password
          False -> Failure ("Password doesn't contain any " ++ error)

verifyPasswordContainsUppercaseCharacters : FormValidation -> FormValidation
verifyPasswordContainsUppercaseCharacters previousValidation =
  let
    pattern = ".*[A-Z]+.*"
    errorMessage = "upper-case characters"
  in
    verifyPasswordContainsRequiredCharacters previousValidation pattern errorMessage

verifyPasswordContainsLowercaseCharacters : FormValidation -> FormValidation
verifyPasswordContainsLowercaseCharacters previousValidation =
  let
    pattern = ".*[a-z]+.*"
    errorMessage = "lower-case characters"
  in
    verifyPasswordContainsRequiredCharacters previousValidation pattern errorMessage

verifyPasswordContainsNumbers : FormValidation -> FormValidation
verifyPasswordContainsNumbers previousValidation =
  let
    pattern = ".*[0-9]+.*"
    errorMessage = "numbers"
  in
    verifyPasswordContainsRequiredCharacters previousValidation pattern errorMessage