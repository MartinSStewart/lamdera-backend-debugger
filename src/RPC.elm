module RPC exposing (..)

import Array
import AssocList as Dict
import AssocSet
import DebugParser
import DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), SequenceType(..))
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Lamdera exposing (SessionId)
import Lamdera.Wire3 as Wire3
import LamderaRPC
import Set
import Task exposing (Task)
import Types exposing (BackendModel, BackendMsg(..), DataType(..), DebugSession, Event(..), Init_, SessionName(..), ToFrontend(..), UpdateFromFrontend_, Update_)


decodeElmValue : Decoder ElmValue
decodeElmValue =
    Json.Decode.string
        |> Json.Decode.andThen
            (\text ->
                case DebugParser.parse ("0: " ++ text) of
                    Ok ok ->
                        Json.Decode.succeed (refineElmValue ok.value)

                    Err error ->
                        Json.Decode.fail error
            )


decodeSessionName =
    Json.Decode.map SessionName Json.Decode.string


decodeDataType : Decoder DataType
decodeDataType =
    Json.Decode.index 0 Json.Decode.int
        |> Json.Decode.andThen
            (\dataType ->
                case dataType of
                    2 ->
                        Json.Decode.map6
                            UpdateFromFrontend_
                            (Json.Decode.index 1 decodeSessionName)
                            (Json.Decode.index 2 decodeElmValue)
                            (Json.Decode.index 3 decodeElmValue)
                            (Json.Decode.index 4 Json.Decode.string)
                            (Json.Decode.index 5 Json.Decode.string)
                            (Json.Decode.index 6 (Json.Decode.nullable decodeElmValue))
                            |> Json.Decode.map UpdateFromFrontend

                    1 ->
                        Json.Decode.map4
                            Update_
                            (Json.Decode.index 1 decodeSessionName)
                            (Json.Decode.index 2 decodeElmValue)
                            (Json.Decode.index 3 decodeElmValue)
                            (Json.Decode.index 4 (Json.Decode.nullable decodeElmValue))
                            |> Json.Decode.map Update

                    0 ->
                        Json.Decode.map3
                            Init_
                            (Json.Decode.index 1 decodeSessionName)
                            (Json.Decode.index 2 decodeElmValue)
                            (Json.Decode.index 3 (Json.Decode.nullable decodeElmValue))
                            |> Json.Decode.map Init

                    _ ->
                        Json.Decode.fail "Invalid data type"
            )


broadcastToClients : SessionName -> ToFrontend -> BackendModel -> Cmd BackendMsg
broadcastToClients sessionName toFrontend model =
    case Dict.get sessionName model.sessions of
        Just session ->
            Set.toList session.connections
                |> List.map (\clientId -> Lamdera.sendToFrontend clientId toFrontend)
                |> Cmd.batch

        Nothing ->
            Cmd.none


dataEndpoint :
    SessionId
    -> BackendModel
    -> Json.Decode.Value
    -> ( Result Http.Error Json.Decode.Value, BackendModel, Cmd BackendMsg )
dataEndpoint _ model request =
    case Json.Decode.decodeValue decodeDataType request of
        Ok dataType ->
            case dataType of
                UpdateFromFrontend data ->
                    updateSession
                        dataType
                        data.sessionName
                        (\session ->
                            { session
                                | history =
                                    Array.push
                                        (ToBackendEvent
                                            { sessionId = data.sessionId
                                            , clientId = data.clientId
                                            , msg = data.msg
                                            , newModel = data.newModel
                                            , cmd = data.maybeCmd
                                            }
                                        )
                                        session.history
                            }
                        )
                        model

                Init data ->
                    updateSession
                        dataType
                        data.sessionName
                        (\session ->
                            { session
                                | initialModel = Just data.model
                                , history = Array.empty
                            }
                        )
                        model

                Update data ->
                    updateSession
                        dataType
                        data.sessionName
                        (\session ->
                            { session
                                | history =
                                    Array.push
                                        (BackendMsgEvent
                                            { msg = data.msg
                                            , newModel = data.newModel
                                            , cmd = data.maybeCmd
                                            }
                                        )
                                        session.history
                            }
                        )
                        model

        Err error ->
            let
                errorText =
                    "Failed to decode webhook: "
                        ++ Json.Decode.errorToString error
            in
            ( Err (Http.BadBody errorText), model, Cmd.none )


refineElmValue : ElmValue -> ElmValue
refineElmValue value =
    case value of
        Plain plain ->
            value

        Expandable expandableValue ->
            (case expandableValue of
                ElmSequence sequenceType elmValues ->
                    List.map refineElmValue elmValues |> ElmSequence sequenceType

                ElmType variant elmValues ->
                    case ( variant, elmValues ) of
                        ( "D", [ Expandable (ElmSequence SeqList list) ] ) ->
                            List.filterMap
                                (\a ->
                                    case a of
                                        Expandable (ElmSequence SeqTuple [ key, value2 ]) ->
                                            Just ( refineElmValue key, refineElmValue value2 )

                                        _ ->
                                            Nothing
                                )
                                list
                                |> ElmDict

                        _ ->
                            ElmType variant (List.map refineElmValue elmValues)

                ElmRecord fields ->
                    List.map (\( field, value2 ) -> ( field, refineElmValue value2 )) fields |> ElmRecord

                ElmDict list ->
                    List.map (\( key, value2 ) -> ( refineElmValue key, refineElmValue value2 )) list
                        |> ElmDict
            )
                |> Expandable


updateSession :
    DataType
    -> SessionName
    -> (DebugSession -> DebugSession)
    -> BackendModel
    -> ( Result error Json.Encode.Value, BackendModel, Cmd BackendMsg )
updateSession dataType sessionName func model =
    ( Ok Json.Encode.null
    , { model
        | sessions =
            Dict.update
                sessionName
                (\maybeSession ->
                    Maybe.withDefault
                        { initialModel = Nothing
                        , initialCmd = Nothing
                        , history = Array.empty
                        , connections = Set.empty
                        , settings = { filter = "", collapsedFields = AssocSet.empty }
                        }
                        maybeSession
                        |> func
                        |> Just
                )
                model.sessions
      }
    , broadcastToClients sessionName (SessionUpdate dataType) model
    )



-- Things that should be auto-generated in future


requestDataEndpoint : String -> Task Http.Error String
requestDataEndpoint value =
    LamderaRPC.asTask Wire3.encodeString Wire3.decodeString value "dataEndpoint"


lamdera_handleEndpoints :
    LamderaRPC.RPCArgs
    -> BackendModel
    -> ( LamderaRPC.RPCResult, BackendModel, Cmd BackendMsg )
lamdera_handleEndpoints args model =
    case args.endpoint of
        "data" ->
            LamderaRPC.handleEndpointJson dataEndpoint args model

        _ ->
            ( LamderaRPC.ResultFailure <| Http.BadBody <| "Unknown endpoint " ++ args.endpoint, model, Cmd.none )
