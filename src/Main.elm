module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (style)
--import Html.Events exposing (onClick)
import State exposing (..)
import View exposing (..)


type alias Model =
    { reqs : Dict Int ReqState
    , picked : Int
    }


initialModel : Model
initialModel =
    { reqs = Dict.fromList <| List.indexedMap Tuple.pair <| List.repeat 10 Pending
    , picked = -1
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        HitProxy req ->
            { model | reqs = setState model.reqs req InProxy }

        HitServer req ->
            { model | reqs = setState model.reqs req Processed }

        Picked p ->
            { model | picked = p }



activatedActions : ( Int, Maybe ReqState ) -> List Msg
activatedActions ( i, s ) =
    case s of
        Nothing ->
            []

        Just st ->
            case st of
                Pending ->
                    [ HitProxy i ]

                InProxy ->
                    [ HitServer i ]

                Processed ->
                    []


view : Model -> Html Msg
view model =
    div []
        [ div [ style "margin-bottom" "10px" ]
            [ h2 [] [ text "next move" ]
            , nextMoveDiv <| activatedActions <| ( model.picked, Dict.get model.picked model.reqs )
            ]
        , div (divAttrByState Pending) (divByState Pending model.reqs)
        , div (divAttrByState InProxy) (divByState InProxy model.reqs)
        , div (divAttrByState Processed) (divByState Processed model.reqs)
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
