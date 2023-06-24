module Backend exposing (..)

import Array
import AssocList as Dict
import AssocSet
import DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), PlainValue(..), SequenceType(..))
import Duration
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Quantity
import RPC
import Set
import Task
import Time
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
        , subscriptions = \_ -> Time.every (1000 * 60 * 60) HourlyCheck
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
                                                [ ( Expandable
                                                        (ElmRecord
                                                            [ ( "a", Plain (ElmString "asdf1") )
                                                            , ( "b", Plain (ElmString "asdf1") )
                                                            ]
                                                        )
                                                  , Plain (ElmNumber 3)
                                                  )
                                                , ( Expandable
                                                        (ElmRecord
                                                            [ ( "a", Plain (ElmString "asdf13") )
                                                            , ( "b", Plain (ElmString "asdf13") )
                                                            ]
                                                        )
                                                  , Plain (ElmNumber 3)
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
                                                    [ ( Expandable
                                                            (ElmRecord
                                                                [ ( "a", Plain (ElmString "asdf13") )
                                                                , ( "b", Plain (ElmString "asdf13") )
                                                                ]
                                                            )
                                                      , Plain (ElmNumber 3)
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
                    , settings = { filter = "", collapsedFields = AssocSet.empty }
                    , lastChange = Time.millisToPosix 1000000000000
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

        GotTime sessionId clientId toBackend time ->
            updateFromFrontendWithTime time sessionId clientId toBackend model

        HourlyCheck time ->
            ( { model
                | sessions =
                    Dict.filter
                        (\_ session ->
                            Duration.from session.lastChange time
                                |> Quantity.lessThan Duration.day
                        )
                        model.sessions
              }
            , Cmd.none
            )

        GotTimeForDataEndpoint sessionId dataType time ->
            RPC.dataEndpointWithTime time sessionId model dataType


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId toBackend model =
    ( model, Time.now |> Task.perform (GotTime sessionId clientId toBackend) )


updateFromFrontendWithTime : Time.Posix -> SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontendWithTime time sessionId clientId msg model =
    case msg of
        LoadSessionRequest sessionName ->
            case Dict.get sessionName model.sessions of
                Just session ->
                    ( { model
                        | sessions =
                            Dict.insert
                                sessionName
                                { session
                                    | connections = Set.insert clientId session.connections
                                    , lastChange = time
                                }
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
                            , settings = { filter = "", collapsedFields = AssocSet.empty }
                            , lastChange = time
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
                                        { session
                                            | history = Array.empty
                                            , initialModel = Nothing
                                            , initialCmd = Nothing
                                            , lastChange = time
                                        }
                                        model.sessions
                            }
                    in
                    ( model2
                    , RPC.broadcastToClients sessionName ResetSession model2
                    )

                Nothing ->
                    ( model, Cmd.none )

        SetSessionSettingsRequest settings ->
            case getSessionByClientId clientId model of
                Just ( sessionName, session ) ->
                    let
                        model2 =
                            { model
                                | sessions =
                                    Dict.insert
                                        sessionName
                                        { session | settings = settings, lastChange = time }
                                        model.sessions
                            }
                    in
                    ( model2, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


getSessionByClientId : ClientId -> BackendModel -> Maybe ( SessionName, DebugSession )
getSessionByClientId clientId model =
    Dict.toList model.sessions
        |> List.find (\( sessionName, session ) -> Set.member clientId session.connections)
