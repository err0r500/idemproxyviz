module State exposing (..)

import Dict exposing (Dict)


type ReqState
    = Pending
    | InProxy
    | Processed


type Msg
    = Do Action
    | Picked Int


type Action
    = HitProxy Int
    | HitServer Int


updateReqState : ReqState -> Maybe ReqState -> Maybe ReqState
updateReqState r mR =
    case mR of
        Just _ ->
            Just r

        Nothing ->
            Nothing


setState : Dict Int ReqState -> Int -> ReqState -> Dict Int ReqState
setState reqs i state =
    Dict.update i (updateReqState state) reqs
