module View exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import State exposing (..)


reqStateString : ReqState -> String
reqStateString r =
    case r of
        Pending ->
            "pending"

        InProxy ->
            "inProxy"

        Processed ->
            "processed"



-- , #9AF5A4, #BDA6F5


reqStateColor : ReqState -> String
reqStateColor r =
    case r of
        Pending ->
            "#F5BFCF"

        InProxy ->
            "#F5F2CB"

        Processed ->
            "#EAB3F5"


msgToString : Msg -> String
msgToString msg =
    case msg of
        HitProxy x ->
            String.fromInt x ++ " hitproxy"

        HitServer x ->
            String.fromInt x ++ " hitserver"

        Picked x ->
            String.fromInt x ++ " picked"


divAttrByState : ReqState -> List (Html.Attribute a)
divAttrByState s =
    let
        common =
            [ style "margin-left" "10px", style "padding" "10px", style "float" "left", style "width" "100px" ]
    in
    case s of
        Pending ->
            List.append common [ style "background-color" (reqStateColor s) ]

        InProxy ->
            List.append common [ style "background-color" (reqStateColor s) ]

        Processed ->
            List.append common [ style "background-color" (reqStateColor s) ]


reqsByState : ReqState -> Dict Int ReqState -> List Int
reqsByState s reqs =
    List.map Tuple.first <| List.filter (\t -> Tuple.second t == s) <| Dict.toList reqs


divByState : ReqState -> Dict Int ReqState -> List (Html Msg)
divByState s reqs =
    [ h2 [] [ text <| reqStateString s ]
    , div [] <|
        List.map
            (\id -> div [] [ button [ onClick (Picked id) ] [ text <| String.fromInt id ] ])
            (reqsByState s reqs)
    ]


nextMoveDiv : List Msg -> Html Msg
nextMoveDiv msgs =
    case msgs of
        [] ->
            div [] [ text "please, pick a message" ]

        _ ->
            div [] <| List.map (\msg -> button [ onClick msg ] [ text <| msgToString msg ]) msgs
