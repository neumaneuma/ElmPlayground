module Main exposing (main)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)

channelPanel : List String -> String -> Element msg
channelPanel channels activeChannel =
    column
        [ height fill
        , width <| fillPortion 1
        , paddingXY 0 10
        , Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        ]
    <|
        List.map (createChannelRow activeChannel) channels

createChannelRow : String -> String -> Element msg
createChannelRow activeChannel channel =
    row (createChannelAttributes activeChannel channel) 
        [ text ("# " ++ channel) ]

createChannelAttributes : String -> String -> List (Attribute msg)
createChannelAttributes activeChannel channel =
    let
        activeChannelAttrs =
            [ Background.color <| rgb255 117 179 201, Font.bold ]

        channelAttrs =
            -- [ paddingXY 55 25, width fill, explain Debug.todo ]
            [ paddingXY 15 5, width fill ]
    in
        if channel == activeChannel then
            activeChannelAttrs ++ channelAttrs
        else
            channelAttrs

chatPanel : Element msg
chatPanel =
    column [ height fill, width <| fillPortion 5 ]
        [ text "chat" ]


main : Html msg
main =
    let 
        channels = ["general", "random", "elm-ui", "news", "official-announcements", "v18", "v19"]
        activeChannel = "elm-ui"
    in
        layout [] <|
            row [ height fill, width fill ]
                [ channelPanel channels activeChannel
                , chatPanel
                ]

