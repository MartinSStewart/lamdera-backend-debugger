module EffectDebugApp exposing (backend)

import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Http
import Effect.Lamdera
import Json.Encode


backend backendNoOp sessionName broadcast sendToFrontend { init, update, updateFromFrontend, subscriptions } =
    Effect.Lamdera.backend
        broadcast
        sendToFrontend
        { init =
            let
                ( model, cmd ) =
                    init
            in
            ( model
            , Command.batch
                [ cmd
                , sendToViewer
                    backendNoOp
                    (Init { sessionName = sessionName, model = Debug.toString model, cmd = Debug.toString cmd })
                ]
            )
        , update =
            \msg model ->
                let
                    ( newModel, cmd ) =
                        update msg model
                in
                ( newModel
                , Command.batch
                    [ cmd
                    , if backendNoOp == msg then
                        Command.none

                      else
                        sendToViewer
                            backendNoOp
                            (Update
                                { sessionName = sessionName
                                , msg = Debug.toString msg
                                , newModel = Debug.toString newModel
                                , cmd = Debug.toString cmd
                                }
                            )
                    ]
                )
        , updateFromFrontend =
            \sessionId clientId msg model ->
                let
                    ( newModel, cmd ) =
                        updateFromFrontend sessionId clientId msg model
                in
                ( newModel
                , Command.batch
                    [ cmd
                    , sendToViewer
                        backendNoOp
                        (UpdateFromFrontend
                            { sessionName = sessionName
                            , msg = Debug.toString msg
                            , newModel = Debug.toString newModel
                            , sessionId = Effect.Lamdera.sessionIdToString sessionId
                            , clientId = Effect.Lamdera.clientIdToString clientId
                            , cmd = Debug.toString cmd
                            }
                        )
                    ]
                )
        , subscriptions = subscriptions
        }


type DataType
    = Init { sessionName : String, model : String, cmd : String }
    | Update { sessionName : String, msg : String, newModel : String, cmd : String }
    | UpdateFromFrontend { sessionName : String, msg : String, newModel : String, sessionId : String, clientId : String, cmd : String }


sendToViewer : msg -> DataType -> Command BackendOnly toFrontend msg
sendToViewer backendNoOp data =
    Effect.Http.post
        { url = "https://backend-debugger.lamdera.app/_r/data"
        , body = Effect.Http.jsonBody (encodeDataType data)
        , expect = Effect.Http.expectWhatever (\_ -> backendNoOp)
        }


encodeDataType : DataType -> Json.Encode.Value
encodeDataType data =
    Json.Encode.list
        identity
        (case data of
            Init { sessionName, model, cmd } ->
                [ Json.Encode.int 0
                , Json.Encode.string sessionName
                , Json.Encode.string model
                , Json.Encode.string cmd
                ]

            Update { sessionName, msg, newModel, cmd } ->
                [ Json.Encode.int 1
                , Json.Encode.string sessionName
                , Json.Encode.string msg
                , Json.Encode.string newModel
                , Json.Encode.string cmd
                ]

            UpdateFromFrontend { sessionName, msg, newModel, sessionId, clientId, cmd } ->
                [ Json.Encode.int 2
                , Json.Encode.string sessionName
                , Json.Encode.string msg
                , Json.Encode.string newModel
                , Json.Encode.string sessionId
                , Json.Encode.string clientId
                , Json.Encode.string cmd
                ]
        )
