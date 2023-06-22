module Backend exposing (..)

import Array
import AssocList as Dict
import DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), PlainValue(..), SequenceType(..))
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
                  , { initialCmd = Just (Plain (ElmNumber 5))
                    , initialModel =
                        Just
                            (Expandable
                                (ElmRecord
                                    [ ( "a", Plain (ElmNumber 5) )
                                    , ( "blahblah"
                                      , Expandable
                                            (ElmRecord
                                                [ ( "a", Plain (ElmNumber 5) )
                                                , ( "b"
                                                  , Expandable
                                                        (ElmSequence SeqList
                                                            [ Plain (ElmNumber 4)
                                                            , Plain (ElmNumber 3)
                                                            , Plain (ElmNumber 4)
                                                            ]
                                                        )
                                                  )
                                                ]
                                            )
                                      )
                                    , ( "abc123"
                                      , Expandable
                                            (ElmDict
                                                [ ( Plain (ElmString "asdf123")
                                                  , Plain (ElmNumber 4)
                                                  )
                                                , ( Plain (ElmString "a")
                                                  , Plain (ElmNumber 4)
                                                  )
                                                ]
                                            )
                                      )
                                    ]
                                )
                            )
                    , history =
                        [ BackendMsgEvent
                            { msg =
                                Expandable
                                    (ElmType "Blah" [ Plain (ElmNumber 4), Plain (ElmNumber 4) ])
                            , cmd = Just (Plain (ElmNumber 5))
                            , newModel =
                                Expandable
                                    (ElmRecord
                                        [ ( "a", Plain (ElmNumber 4) )
                                        , ( "blahblah"
                                          , Expandable
                                                (ElmRecord
                                                    [ ( "a", Plain (ElmNumber 5) )
                                                    , ( "b"
                                                      , Expandable
                                                            (ElmSequence SeqList
                                                                [ Plain (ElmNumber 4)
                                                                , Plain (ElmNumber 4)
                                                                ]
                                                            )
                                                      )
                                                    ]
                                                )
                                          )
                                        , ( "abc123"
                                          , Expandable
                                                (ElmDict
                                                    [ ( Plain (ElmString "asdf123")
                                                      , Plain (ElmNumber 3)
                                                      )
                                                    , ( Plain (ElmString "b")
                                                      , Plain (ElmNumber 4)
                                                      )
                                                    ]
                                                )
                                          )
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
                            , initialCmd = Nothing
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
