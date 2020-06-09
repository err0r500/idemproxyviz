module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div)
import State exposing (..)
import View exposing (..)



-- Global State


type alias Model =
    { reqs : Dict Int ReqState
    , picked : Int
    , failedInvariants : List String
    }


initialModel : Model
initialModel =
    { reqs = Dict.fromList <| List.indexedMap Tuple.pair <| List.repeat 10 Pending
    , picked = -1
    , failedInvariants = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Do action ->
            case action of
                HitProxy req ->
                    { model | reqs = setState model.reqs req InProxy }

                HitServer req ->
                    { model | reqs = setState model.reqs req Processed }

        Picked p ->
            { model | picked = p }



-- Actions


activatedActions : Model -> List Action
activatedActions model =
    case Dict.get model.picked model.reqs of
        Nothing ->
            []

        Just st ->
            case st of
                Pending ->
                    [ HitProxy model.picked ]

                InProxy ->
                    [ HitServer model.picked ]

                Processed ->
                    []



-- Invariants


processedOnce : Model -> Maybe String
processedOnce m =
    if Dict.size (Dict.filter (\_ v -> v == Processed) m.reqs) > 1 then
        Just "more than one processed"

    else
        Nothing


invariantsCheck : Model -> List String
invariantsCheck m =
    List.filterMap identity [ processedOnce m ]



-- View


view : Model -> Html Msg
view model =
    div []
        [ nextMoveDiv <| activatedActions model
        , errDiv <| invariantsCheck model
        , divByState Pending model.reqs
        , divByState InProxy model.reqs
        , divByState Processed model.reqs
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
