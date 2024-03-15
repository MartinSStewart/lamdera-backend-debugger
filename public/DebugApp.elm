module DebugApp exposing (backend)

import Http
import Json.Encode
import Lamdera exposing (ClientId, SessionId)
import Task
import Time


backend :
    backendMsg
    -> String
    ->
        { init : ( backendModel, Cmd backendMsg )
        , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
        , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
        , subscriptions : backendModel -> Sub backendMsg
        }
    ->
        { init : ( backendModel, Cmd backendMsg )
        , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
        , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
        , subscriptions : backendModel -> Sub backendMsg
        }
backend backendNoOp sessionName { init, update, updateFromFrontend, subscriptions } =
    { init =
        let
            ( model, cmd ) =
                init
        in
        ( model
        , Cmd.batch
            [ cmd
            , sendToViewer
                backendNoOp
                (Init { sessionName = sessionName, model = Debug.toString model })
            ]
        )
    , update =
        \msg model ->
            let
                ( newModel, cmd ) =
                    update msg model
            in
            ( newModel
            , Cmd.batch
                [ cmd
                , if backendNoOp == msg then
                    Cmd.none

                  else
                    sendToViewer
                        backendNoOp
                        (Update
                            { sessionName = sessionName
                            , msg = Debug.toString msg
                            , newModel = Debug.toString newModel
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
            , Cmd.batch
                [ cmd
                , sendToViewer
                    backendNoOp
                    (UpdateFromFrontend
                        { sessionName = sessionName
                        , msg = Debug.toString msg
                        , newModel = Debug.toString newModel
                        , sessionId = sessionId
                        , clientId = clientId
                        }
                    )
                ]
            )
    , subscriptions = subscriptions
    }


type DataType
    = Init { sessionName : String, model : String }
    | Update { sessionName : String, msg : String, newModel : String }
    | UpdateFromFrontend { sessionName : String, msg : String, newModel : String, sessionId : String, clientId : String }


sendToViewer : msg -> DataType -> Cmd msg
sendToViewer backendNoOp data =
    Time.now
        |> Task.andThen
            (\time ->
                Http.task
                    { method = "POST"
                    , headers = []
                    , url = "http://localhost:8001/https://backend-debugger.lamdera.app/_r/data"
                    , body = Http.jsonBody (encodeDataType time data)
                    , resolver = Http.bytesResolver (\_ -> Ok ())
                    , timeout = Just 10000
                    }
            )
        |> Task.attempt (\_ -> backendNoOp)


encodeTime : Time.Posix -> Json.Encode.Value
encodeTime time =
    Time.posixToMillis time |> Json.Encode.int


encodeDataType : Time.Posix -> DataType -> Json.Encode.Value
encodeDataType time data =
    Json.Encode.list
        identity
        (case data of
            Init { sessionName, model } ->
                [ Json.Encode.int 0
                , Json.Encode.string sessionName
                , Json.Encode.string model
                , Json.Encode.null
                , encodeTime time
                ]

            Update { sessionName, msg, newModel } ->
                [ Json.Encode.int 1
                , Json.Encode.string sessionName
                , Json.Encode.string msg
                , Json.Encode.string newModel
                , Json.Encode.null
                , encodeTime time
                ]

            UpdateFromFrontend { sessionName, msg, newModel, sessionId, clientId } ->
                [ Json.Encode.int 2
                , Json.Encode.string sessionName
                , Json.Encode.string msg
                , Json.Encode.string newModel
                , Json.Encode.string sessionId
                , Json.Encode.string clientId
                , Json.Encode.null
                , encodeTime time
                ]
        )
