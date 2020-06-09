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


actionString : Action -> String
actionString msg =
    case msg of
        HitProxy x ->
            String.fromInt x ++ " hitproxy"

        HitServer x ->
            String.fromInt x ++ " hitserver"


divAttrByState : ReqState -> List (Html.Attribute a)
divAttrByState s =
    let
        common =
            [ style "margin-left" "10px"
            , style "padding" "10px"
            , style "float" "left"
            , style "width" "100px"
            ]
    in
    case s of
        Pending ->
            List.append common [ style "background-color" (reqStateColor s) ]

        InProxy ->
            List.append common [ style "background-color" (reqStateColor s) ]

        Processed ->
            List.append common [ style "background-color" (reqStateColor s) ]


reqsInState : ReqState -> Dict Int ReqState -> List Int
reqsInState s reqs =
    List.map Tuple.first <| List.filter (\t -> Tuple.second t == s) <| Dict.toList reqs


divByState : ReqState -> Dict Int ReqState -> Html Msg
divByState s reqs =
    div (divAttrByState s)
        [ h2 [] [ text <| reqStateString s ]
        , div [] <|
            List.map
                (\id -> div [] [ button [ onClick (Picked id) ] [ text <| String.fromInt id ] ])
                (reqsInState s reqs)
        ]


nextMoveDiv : List Action -> Html Msg
nextMoveDiv actions =
    div [ style "margin-bottom" "10px" ]
        [ h2 [] [ text "next move" ]
        , moveMsg actions
        ]


moveMsg : List Action -> Html Msg
moveMsg msgs =
    case msgs of
        [] ->
            div [] [ text "please, pick a message" ]

        _ ->
            div [] <| List.map (\action -> button [ onClick (Do action) ] [ text <| actionString action ]) msgs


errDiv : List String -> Html a
errDiv errs =
    div [ style "color" "red" ] <| List.map (\x -> text x) errs
