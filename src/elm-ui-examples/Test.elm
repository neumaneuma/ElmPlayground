module Main exposing (main)

import Html exposing (Html, button, div, fieldset, input, li, span, text, ul)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import RemoteData exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Types


type alias Model =
    { formField : FormField
    , repos : RemoteData String (List String)
    }


type Msg
    = Retrieve
    | RetrievedRepos (List String)
    | RetrievalError String
    | TypeInput String


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { formField = FormField "Repo keyword" "" [ Required ]
            , repos = NotAsked
            }
    in
    initialModel ! []



-- Views


view : Model -> Html Msg
view model =
    div
        []
        (List.append
            [ formFieldView model.formField ]
            [ button [ onClick Retrieve ] [ text "Search" ]
            , resultsView model
            ]
        )


formFieldView : FormField -> Html Msg
formFieldView formField =
    fieldset
        []
        [ span [ class "label" ] [ text formField.label ]
        , input
            [ type_ "text", value formField.value, onInput TypeInput ]
            []
        ]


resultsView : Model -> Html Msg
resultsView model =
    case model.repos of
        NotAsked ->
            div [] [ text "" ]

        Loading ->
            div [] [ text "Please wait.." ]

        Failure error ->
            div [] [ text error ]

        Success repos ->
            ul [] <| List.map (\repo -> li [] [ text repo ]) repos



-- Updates


noMsg : Model -> ( Model, Cmd Msg )
noMsg model =
    model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypeInput value ->
            { model | formField = updateFormField model.formField value }
                |> noMsg

        Retrieve ->
            model.formField
                |> invalidFormData
                |> validate
                |> Result.map (\validFormData -> { model | repos = Loading } ! [ fetchRepos validFormData ])
                |> Result.withDefault ({ model | repos = Failure "Invalid form" } ! [])

        RetrievalError error ->
            { model | repos = Failure error } ! []

        RetrievedRepos repos ->
            { model | repos = Success repos } ! []


updateFormField : FormField -> String -> FormField
updateFormField formField value =
    { formField | value = value }



-- HTTP


fetchRepos : FormData Valid -> Cmd Msg
fetchRepos formData =
    let
        keyword =
            formData
                |> fieldFromValidForm
                |> .value
    in
    Http.getString ("https://api.github.com/search/repositories?q=" ++ keyword)
        |> Http.send resultToMsg


resultToMsg : Result Http.Error String -> Msg
resultToMsg result =
    case Result.map (Decode.decodeString decodeResponse) result of
        Ok decodedResult ->
            case decodedResult of
                Ok repos ->
                    RetrievedRepos repos

                Err error ->
                    RetrievalError error

        Err _ ->
            RetrievalError "There was a problem retrieving github repositories"


decodeResponse : Decode.Decoder (List String)
decodeResponse =
    Decode.field "items" (Decode.list (Decode.field "html_url" Decode.string))



{-
   The code below this comment would go in a separate module so that
   certain types are opaque (see https://medium.com/@ckoster22/advanced-types-in-elm-opaque-types-ec5ec3b84ed2)

       FormData.elm

   module FormData exposing (Constraint(..), FormData, FormField, Invalid, Valid, fieldFromValidForm, invalidFormData, validate)

   Notice some constructor functions are not exposed through "(..)"
-}


type FormData a
    = FormData FormField -- A phantom type


type Valid
    = Valid


type Invalid
    = Invalid


type alias FormField =
    { label : String
    , value : String
    , constraints : List Constraint
    }


type Constraint
    = Required
    | NoSpaces -- etc


invalidFormData : FormField -> FormData Invalid
invalidFormData =
    -- Note that this function would be exposed
    FormData


validFormData : FormField -> FormData Valid
validFormData =
    -- Note that this function IS NOT exposed
    FormData


fieldFromValidForm : FormData Valid -> FormField
fieldFromValidForm formData =
    case formData of
        FormData formField ->
            formField


validate : FormData Invalid -> Result (FormData Invalid) (FormData Valid)
validate formData =
    case formData of
        FormData formField ->
            if isInvalid formField then
                Err formData
            else
                Ok <| validFormData formField


isInvalid : FormField -> Bool
isInvalid formField =
    List.any (failsConstraint formField.value) formField.constraints


failsConstraint : String -> Constraint -> Bool
failsConstraint value constraint =
    case constraint of
        Required ->
            value == ""

        NoSpaces ->
            String.contains " " value
