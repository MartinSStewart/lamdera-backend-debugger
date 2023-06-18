module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Url exposing (Url)


type FrontendModel
    = LoadingSession LoadingData
    | LoadedSession LoadedData
    | HomePage { key : Key }


type alias LoadingData =
    { key : Key, sessionName : SessionName }


type alias LoadedData =
    { key : Key
    , sessionName : SessionName
    , initialModel : Maybe String
    , history : Array Event
    , selected : Int
    }


type alias BackendModel =
    { sessions : Dict SessionName DebugSession
    }


type alias SessionName =
    String


type DataType
    = Init Init_
    | Update Update_
    | UpdateFromFrontend UpdateFromFrontend_


type alias Init_ =
    { sessionName : SessionName, model : String }


type alias Update_ =
    { sessionName : SessionName, msg : String, newModel : String }


type alias UpdateFromFrontend_ =
    { sessionName : SessionName, msg : String, newModel : String, sessionId : String, clientId : String }


type alias DebugSession =
    { initialModel : Maybe String
    , history : Array Event
    , connections : Set ClientId
    }


type Event
    = BackendMsgEvent BackendMsgEvent_
    | ToBackendEvent ToBackendEvent_


type alias BackendMsgEvent_ =
    { msg : String, newModel : String }


type alias ToBackendEvent_ =
    { msg : String, newModel : String, sessionId : String, clientId : String }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedEvent Int


type ToBackend
    = LoadSessionRequest String


type BackendMsg
    = NoOpBackendMsg
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = LoadSessionResponse DebugSession
    | SessionUpdate DataType
