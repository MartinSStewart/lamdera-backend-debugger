module Evergreen.V4.Types exposing (..)

import Array
import AssocList
import Browser
import Browser.Navigation
import DebugParser.ElmValue
import Lamdera
import Set
import Url


type SessionName
    = SessionName String


type alias LoadingData =
    { key : Browser.Navigation.Key
    , sessionName : SessionName
    }


type alias BackendMsgEvent_ =
    { msg : DebugParser.ElmValue.ElmValue
    , newModel : DebugParser.ElmValue.ElmValue
    }


type alias ToBackendEvent_ =
    { msg : DebugParser.ElmValue.ElmValue
    , newModel : DebugParser.ElmValue.ElmValue
    , sessionId : String
    , clientId : String
    }


type Event
    = BackendMsgEvent BackendMsgEvent_
    | ToBackendEvent ToBackendEvent_


type alias LoadedData =
    { key : Browser.Navigation.Key
    , sessionName : SessionName
    , initialModel : Maybe DebugParser.ElmValue.ElmValue
    , history : Array.Array Event
    , selected : Int
    }


type FrontendModel
    = LoadingSession LoadingData
    | LoadedSession LoadedData
    | HomePage
        { key : Browser.Navigation.Key
        }


type alias DebugSession =
    { initialModel : Maybe DebugParser.ElmValue.ElmValue
    , history : Array.Array Event
    , connections : Set.Set Lamdera.ClientId
    }


type alias BackendModel =
    { sessions : AssocList.Dict SessionName DebugSession
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PressedEvent Int
    | PressedResetSession


type ToBackend
    = LoadSessionRequest SessionName
    | ResetSessionRequest


type BackendMsg
    = NoOpBackendMsg
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type alias Init_ =
    { sessionName : SessionName
    , model : DebugParser.ElmValue.ElmValue
    }


type alias Update_ =
    { sessionName : SessionName
    , msg : DebugParser.ElmValue.ElmValue
    , newModel : DebugParser.ElmValue.ElmValue
    }


type alias UpdateFromFrontend_ =
    { sessionName : SessionName
    , msg : DebugParser.ElmValue.ElmValue
    , newModel : DebugParser.ElmValue.ElmValue
    , sessionId : String
    , clientId : String
    }


type DataType
    = Init Init_
    | Update Update_
    | UpdateFromFrontend UpdateFromFrontend_


type ToFrontend
    = LoadSessionResponse DebugSession
    | SessionUpdate DataType
    | ResetSession
