module Backend exposing (..)

import Array
import AssocList as Dict
import DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), PlainValue(..))
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import RPC
import Set
import Types exposing (..)


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( --{ sessions = Dict.empty }
      { sessions =
            Dict.fromList
                [ ( SessionName "test"
                  , { initialModel =
                        Just
                            (Expandable True
                                (ElmRecord
                                    [ ( "a", Plain (ElmNumber 5) )
                                    , ( "b", Plain (ElmString "c") )
                                    ]
                                )
                            )
                    , history =
                        [ BackendMsgEvent
                            { msg = Expandable True (ElmType "MyVariant" [ Plain (ElmNumber 5) ])
                            , newModel =
                                Expandable True
                                    (ElmRecord
                                        [ ( "a", Plain (ElmNumber 5) )
                                        , ( "b", Plain (ElmString "c") )
                                        ]
                                    )
                            }
                        , BackendMsgEvent
                            { msg = Expandable True (ElmType "MyVariant" [ Plain (ElmNumber 5) ])
                            , newModel =
                                Expandable True
                                    (ElmRecord
                                        [ ( "a", Plain (ElmNumber 5) )
                                        , ( "b", Plain (ElmString "c") )
                                        ]
                                    )
                            }
                        , BackendMsgEvent
                            { msg = Expandable True (ElmType "MyVariant" [ Plain (ElmNumber 5) ])
                            , newModel =
                                Expandable True
                                    (ElmRecord
                                        [ ( "a", Plain (ElmNumber 5) )
                                        , ( "b", Plain (ElmString "c") )
                                        ]
                                    )
                            }
                        ]
                            |> Array.fromList
                    , connections = Set.empty
                    }
                  )
                ]
      }
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

        ResetSessionRequest ->
            case getSessionByClientId clientId model of
                Just ( sessionName, session ) ->
                    let
                        model2 =
                            { model
                                | sessions =
                                    Dict.insert
                                        sessionName
                                        { session | history = Array.empty, initialModel = Nothing }
                                        model.sessions
                            }
                    in
                    ( model2
                    , RPC.broadcastToClients sessionName ResetSession model2
                    )

                Nothing ->
                    ( model, Cmd.none )


getSessionByClientId : ClientId -> BackendModel -> Maybe ( SessionName, DebugSession )
getSessionByClientId clientId model =
    Dict.toList model.sessions
        |> List.find (\( sessionName, session ) -> Set.member clientId session.connections)
