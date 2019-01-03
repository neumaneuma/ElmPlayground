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
    let
        activeChannelAttrs =
            [ Background.color <| rgb255 117 179 201, Font.bold ]

        channelAttrs =
            [ paddingXY 15 5, width fill ]

        channelEl channel =
            el
                (if channel == activeChannel then
                    activeChannelAttrs ++ channelAttrs
                 else
                    channelAttrs
                )
            <|
                text ("# " ++ channel)
    in
    column
        [ height fill
        , width <| fillPortion 1
        , paddingXY 0 10
        , Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        ]
    <|
        List.map channelEl channels


chatPanel : String -> Element msg
chatPanel activeChannel =
    let
        header = createHeader activeChannel
        messagePanel = column [] []
        footer = createFooter
    in
        column
            [ height fill
            , width <| fillPortion 5
            -- , explain Debug.todo
            ]
            <| [ header, messagePanel, footer ]

createHeader : String -> Element msg
createHeader activeChannel =
    row
        [ width fill
        -- , height fill -- if this was uncommented, then header would take up all available vertical blank space
        , paddingXY 20 5
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color <| rgb255 200 200 200
        ]
        [ row [] [text <| "#" ++ activeChannel]
        , Input.button
            [ padding 5
            , alignRight
            , Border.width 1
            , Border.rounded 3
            , Border.color <| rgb255 200 200 200
            ]
            { onPress = Nothing
            , label = text "Search"
            }
        ]

createFooter : Element msg
createFooter =
    let
        messageTextBoxContainer = createMessageTextBoxContainer
    in
        row
            [ alignBottom
            , padding 20
            , width fill
            -- , height fill
            -- , explain Debug.todo
            ]
            <| [ messageTextBoxContainer ]

createMessageTextBoxContainer : Element msg
createMessageTextBoxContainer =
    let
        plusSignButton = createPlusSignButton
        messageTextBox = createMessageTextbox
    in
        row
            [ spacingXY 2 0
            , width fill
            -- , height fill
            
            -- , centerX
            -- , centerY
            -- , alignRight
            , alignLeft
            -- , alignTop
            , alignBottom

            , Border.width 2
            , Border.rounded 4
            , Border.color <| rgb255 200 200 200
            -- , explain Debug.todo
            ]
            <| [ plusSignButton, messageTextBox ]

createPlusSignButton : Element msg
createPlusSignButton =
    row
        [ padding 15
        , Border.widthEach { right = 2, left = 0, top = 0, bottom = 0 }
        , Border.color <| rgb255 200 200 200
        , mouseOver [ Background.color <| rgb255 86 182 239 ]

        -- , centerX
        -- , centerY
        , alignLeft
        -- , alignRight
        -- , alignTop
        -- , alignBottom

        -- , explain Debug.todo
        ]
        [ text "+" ]

createMessageTextbox : Element msg
createMessageTextbox =
    row
        [ mouseOver [ Background.color <| rgb255 100 282 44 ]
        ]
        []

main : Html msg
main =
    let 
        channels = ["general", "random", "elm-ui", "news", "official-announcements", "v18", "v19"]
        activeChannel = "elm-ui"
    in
        layout [] <|
            row [ height fill, width fill ]
                [ channelPanel channels activeChannel
                , chatPanel activeChannel
                ]