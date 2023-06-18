module DebugApp exposing (backend)

import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Lamdera exposing (ClientId, SessionId)


backend :
    { init : ( backendModel, Cmd backendMsg )
    , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
    , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
    , subscriptions : backendModel -> Sub backendMsg
    }
    ->
        { init : ( backendModel, Cmd (Msg backendMsg) )
        , update : Msg backendMsg -> backendModel -> ( backendModel, Cmd (Msg backendMsg) )
        , updateFromFrontend : SessionId -> ClientId -> Msg toBackend -> backendModel -> ( backendModel, Cmd (Msg backendMsg) )
        , subscriptions : backendModel -> Sub backendMsg
        }
backend { init, update, updateFromFrontend, subscriptions } =
    { init =
        let
            ( model, cmd ) =
                init
        in
        ( model, Cmd.batch [ Cmd.map UserMsg cmd, sendToViewer (Init (Debug.toString model)) ] )
    , update =
        \msg model ->
            case msg of
                NoOp ->
                    ( model, Cmd.none )

                UserMsg userMsg ->
                    let
                        ( newModel, cmd ) =
                            update userMsg model
                    in
                    ( newModel
                    , Cmd.batch
                        [ Cmd.map UserMsg cmd
                        , sendToViewer
                            (Update
                                { msg = Debug.toString userMsg
                                , newModel = Debug.toString model
                                }
                            )
                        ]
                    )
    , updateFromFrontend =
        \sessionId clientId msg model ->
            case msg of
                NoOp ->
                    ( model, Cmd.none )

                UserMsg userMsg ->
                    let
                        ( newModel, cmd ) =
                            updateFromFrontend sessionId clientId userMsg model
                    in
                    ( newModel
                    , Cmd.batch
                        [ Cmd.map UserMsg cmd
                        , sendToViewer
                            (UpdateFromFrontend
                                { msg = Debug.toString userMsg
                                , newModel = Debug.toString model
                                , sessionId = sessionId
                                , clientId = clientId
                                }
                            )
                        ]
                    )
    , subscriptions = subscriptions
    }


type Msg msg
    = UserMsg msg
    | NoOp


type DataType
    = Init String
    | Update Update_
    | UpdateFromFrontend UpdateFromFrontend_


type alias Update_ =
    { msg : String, newModel : String }


type alias UpdateFromFrontend_ =
    { msg : String, newModel : String, sessionId : String, clientId : String }


sendToViewer : DataType -> Cmd (Msg msg)
sendToViewer data =
    Http.post
        { url = "https://backend-debugger.lamdera.app/r/data"
        , body = Http.jsonBody (encodeDataType data)
        , expect = Http.expectWhatever (\_ -> NoOp)
        }


encodeDataType : DataType -> Json.Encode.Value
encodeDataType data =
    Json.Encode.list
        identity
        (case data of
            Init model ->
                [ Json.Encode.int 0
                , Json.Encode.string model
                ]

            Update { msg, newModel } ->
                [ Json.Encode.int 1
                , Json.Encode.string msg
                , Json.Encode.string newModel
                ]

            UpdateFromFrontend { msg, newModel, sessionId, clientId } ->
                [ Json.Encode.int 2
                , Json.Encode.string msg
                , Json.Encode.string newModel
                , Json.Encode.string sessionId
                , Json.Encode.string clientId
                ]
        )


decodeDataType : Decoder DataType
decodeDataType =
    Json.Decode.index 0 Json.Decode.int
        |> Json.Decode.andThen
            (\dataType ->
                case dataType of
                    2 ->
                        Json.Decode.map4
                            UpdateFromFrontend_
                            Json.Decode.string
                            Json.Decode.string
                            Json.Decode.string
                            Json.Decode.string
                            |> Json.Decode.map UpdateFromFrontend

                    1 ->
                        Json.Decode.map2
                            Update_
                            Json.Decode.string
                            Json.Decode.string
                            |> Json.Decode.map Update

                    0 ->
                        Json.Decode.map Init Json.Decode.string

                    _ ->
                        Json.Decode.fail "Invalid data type"
            )
