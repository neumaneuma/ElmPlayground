module Main exposing (main)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)

channelPanel : Element msg
channelPanel =
    row
        [ height <| fillPortion 1
        , width fill
        , Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        ]
        [ text "channels" ]


chatPanel : Element msg
chatPanel =
    row 
        [ height <| fillPortion 5
        , width fill
        ]
        [ text "chat" ]


main : Html msg
main =
    layout [] <|
        column
            [ height fill
            , width fill
            ]
            [ channelPanel
            , chatPanel
            ]
