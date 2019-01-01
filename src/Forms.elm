module Main exposing (FormValidation(..), Model, Msg(..), checkPasswordEquality, checkPasswordLength, init, main, update, validateAgeIsInt, validatePassword, verifyPasswordContainsLowercaseCharacters, verifyPasswordContainsNumbers, verifyPasswordContainsRequiredCharacters, verifyPasswordContainsUppercaseCharacters, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Parser
import Regex
import String



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , age : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" "" ""



-- UPDATE


type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Age age ->
            { model | age = age }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        column [ height fill, width fill ]
            [-- passwordInput
            ]



-- passwordInput : Element msg
-- passwordInput =
-- row
--   [ width fill
--   -- , paddingXY 20 5
--   -- , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
--   -- , Border.color <| rgb255 200 200 200
--   ]
--   [ Input. ]
-- div []
--   [ viewInput "text" "Name" model.name Name
--   , viewInput "text" "Age" model.age Age
--   -- , viewInput "password" "Password" model.password Password
--   , viewInput "text" "Password" model.password Password
--   -- , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
--   , viewInput "text" "Re-enter Password" model.passwordAgain PasswordAgain
--   , input [type_ "button", value "Submit"] []
--   , validateInput model
--   ]
-- viewInput : String -> String -> String -> (String -> msg) -> Html msg
-- viewInput t p v toMsg =
--   input [ type_ t, placeholder p, value v, onInput toMsg ] []
-- validateInput : Model -> Html msg
-- validateInput model =
--   validatePassword model
--   |> validateAgeIsInt
--   |> outputHtmlDiv


validatePassword : Model -> FormValidation
validatePassword model =
    checkPasswordEquality model
        |> checkPasswordLength
        |> verifyPasswordContainsUppercaseCharacters
        |> verifyPasswordContainsLowercaseCharacters
        |> verifyPasswordContainsNumbers


type FormValidation
    = Success Model
    | Failure String



-- outputHtmlDiv: FormValidation -> Html msg
-- outputHtmlDiv validation =
--   case validation of
--     Failure message ->
--       div [ style "color" "red" ] [ text message ]
--     Success _ ->
--       div [ style "color" "green" ] [ text "OK" ]


checkPasswordEquality : Model -> FormValidation
checkPasswordEquality model =
    let
        password =
            model.password

        passwordAgain =
            model.passwordAgain
    in
    if password == passwordAgain then
        Success model

    else
        Failure "Passwords do not match!"


checkPasswordLength : FormValidation -> FormValidation
checkPasswordLength previousValidation =
    case previousValidation of
        Failure message ->
            Failure message

        Success model ->
            let
                password =
                    model.password
            in
            if String.length password < 8 then
                Failure "Password must be at least 8 characters"

            else
                Success model


verifyPasswordContainsRequiredCharacters : FormValidation -> String -> String -> FormValidation
verifyPasswordContainsRequiredCharacters previousValidation pattern error =
    case previousValidation of
        Failure message ->
            Failure message

        Success model ->
            let
                password =
                    model.password

                regex =
                    Maybe.withDefault Regex.never <| Regex.fromString pattern

                contains =
                    Regex.contains regex password
            in
            case contains of
                True ->
                    Success model

                False ->
                    Failure ("Password doesn't contain any " ++ error)


verifyPasswordContainsUppercaseCharacters : FormValidation -> FormValidation
verifyPasswordContainsUppercaseCharacters previousValidation =
    let
        pattern =
            ".*[A-Z]+.*"

        errorMessage =
            "upper-case characters"
    in
    verifyPasswordContainsRequiredCharacters previousValidation pattern errorMessage


verifyPasswordContainsLowercaseCharacters : FormValidation -> FormValidation
verifyPasswordContainsLowercaseCharacters previousValidation =
    let
        pattern =
            ".*[a-z]+.*"

        errorMessage =
            "lower-case characters"
    in
    verifyPasswordContainsRequiredCharacters previousValidation pattern errorMessage


verifyPasswordContainsNumbers : FormValidation -> FormValidation
verifyPasswordContainsNumbers previousValidation =
    let
        pattern =
            ".*[0-9]+.*"

        errorMessage =
            "numbers"
    in
    verifyPasswordContainsRequiredCharacters previousValidation pattern errorMessage


validateAgeIsInt : FormValidation -> FormValidation
validateAgeIsInt previousValidation =
    case previousValidation of
        Failure message ->
            Failure message

        Success model ->
            let
                age =
                    model.age

                cast =
                    String.toInt age
            in
            case cast of
                Just value ->
                    if value < 1 then
                        Failure "Age must be greater than 0"

                    else
                        Success model

                Nothing ->
                    Failure "Age is not a valid number"
