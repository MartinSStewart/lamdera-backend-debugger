module Types exposing (..)

import Array exposing (Array)
import AssocList
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import DebugParser.ElmValue exposing (ElmValue)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Url exposing (Url)


type FrontendModel
    = LoadingSession LoadingData
    | LoadedSession LoadedData


type alias LoadingData =
    { key : Key, sessionName : SessionName }


type alias LoadedData =
    { key : Key
    , sessionName : SessionName
    , initialModel : Maybe ElmValue
    , history : Array Event
    , selected : Int
    }


type alias BackendModel =
    { sessions : AssocList.Dict SessionName DebugSession
    }


type SessionName
    = SessionName String


type DataType
    = Init Init_
    | Update Update_
    | UpdateFromFrontend UpdateFromFrontend_


type alias Init_ =
    { sessionName : SessionName, model : ElmValue }


type alias Update_ =
    { sessionName : SessionName, msg : ElmValue, newModel : ElmValue }


type alias UpdateFromFrontend_ =
    { sessionName : SessionName, msg : ElmValue, newModel : ElmValue, sessionId : String, clientId : String }


type alias DebugSession =
    { initialModel : Maybe ElmValue
    , history : Array Event
    , connections : Set ClientId
    }


type Event
    = BackendMsgEvent BackendMsgEvent_
    | ToBackendEvent ToBackendEvent_


type alias BackendMsgEvent_ =
    { msg : ElmValue, newModel : ElmValue }


type alias ToBackendEvent_ =
    { msg : ElmValue, newModel : ElmValue, sessionId : String, clientId : String }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedEvent Int
    | PressedResetSession
    | GotRandomSessionName SessionName


type ToBackend
    = LoadSessionRequest SessionName
    | ResetSessionRequest


type BackendMsg
    = NoOpBackendMsg
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = LoadSessionResponse DebugSession
    | SessionUpdate DataType
    | ResetSession
