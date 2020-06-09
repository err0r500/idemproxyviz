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
    { reqs = Dict.fromList <| List.indexedMap Tuple.pair <| List.repeat 4 Pending
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

                HitCache req ->
                    { model | reqs = setState model.reqs req FromCache }

        Picked p ->
            { model | picked = p }



-- Actions


activatedActions : Model -> List Action
activatedActions m =
    case Dict.get m.picked m.reqs of
        Nothing ->
            -- model.picked is not a req key
            []

        Just st ->
            case st of
                Pending ->
                    [ HitProxy m.picked ]

                InProxy ->
                    if Dict.size (reqsByState m.reqs Processed) == 0 then
                        [ HitServer m.picked ]

                    else if Dict.size (reqsByState m.reqs Processed) == 1 then
                        [ HitCache m.picked ]

                    else
                        []

                Processed ->
                    []

                FromCache ->
                    []



-- Invariants
-- the list of invariants that must be respected all the time


processOnlyOnce : Model -> Maybe String
processOnlyOnce m =
    if Dict.size (reqsByState m.reqs Processed) > 1 then
        Just "processOnlyOnce failed"

    else
        Nothing


invariantsCheck : Model -> List String
invariantsCheck m =
    List.filterMap identity [ processOnlyOnce m ]



-- View


view : Model -> Html Msg
view model =
    div []
        [ nextMoveDiv <| activatedActions model
        , errDiv <| invariantsCheck model
        , divByState Pending model.reqs
        , divByState InProxy model.reqs
        , divByState Processed model.reqs
        , divByState FromCache model.reqs
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
