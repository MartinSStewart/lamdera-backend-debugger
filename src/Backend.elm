module Backend exposing (..)

import Array
import DebugApp
import Dict
import Html
import Lamdera exposing (ClientId, SessionId)
import Set
import Types exposing (..)


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    DebugApp.backend
        NoOpBackendMsg
        "Blah"
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { sessions = Dict.empty }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        ClientDisconnected _ clientId ->
            ( { model
                | sessions =
                    Dict.map
                        (\_ session -> { session | connections = Set.remove clientId session.connections })
                        model.sessions
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        LoadSessionRequest sessionName ->
            case Dict.get sessionName model.sessions of
                Just session ->
                    ( { model
                        | sessions =
                            Dict.insert
                                sessionName
                                { session | connections = Set.insert clientId session.connections }
                                model.sessions
                      }
                    , Lamdera.sendToFrontend clientId (LoadSessionResponse session)
                    )

                Nothing ->
                    let
                        session : DebugSession
                        session =
                            { initialModel = Nothing
                            , history = Array.empty
                            , connections = Set.singleton clientId
                            }
                    in
                    ( { model | sessions = Dict.insert sessionName session model.sessions }
                    , Lamdera.sendToFrontend clientId (LoadSessionResponse session)
                    )
