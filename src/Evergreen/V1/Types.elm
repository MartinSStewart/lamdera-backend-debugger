module Evergreen.V1.Types exposing (..)

import Array
import Browser
import Browser.Navigation
import Dict
import Lamdera
import Set
import Url


type alias SessionName =
    String


type alias LoadingData =
    { key : Browser.Navigation.Key
    , sessionName : SessionName
    }


type alias BackendMsgEvent_ =
    { msg : String
    , newModel : String
    }


type alias ToBackendEvent_ =
    { msg : String
    , newModel : String
    , sessionId : String
    , clientId : String
    }


type Event
    = BackendMsgEvent BackendMsgEvent_
    | ToBackendEvent ToBackendEvent_


type alias LoadedData =
    { key : Browser.Navigation.Key
    , sessionName : SessionName
    , initialModel : Maybe String
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
    { initialModel : Maybe String
    , history : Array.Array Event
    , connections : Set.Set Lamdera.ClientId
    }


type alias BackendModel =
    { sessions : Dict.Dict SessionName DebugSession
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PressedEvent Int
    | PressedResetSession


type ToBackend
    = LoadSessionRequest String
    | ResetSessionRequest


type BackendMsg
    = NoOpBackendMsg
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type alias Init_ =
    { sessionName : SessionName
    , model : String
    }


type alias Update_ =
    { sessionName : SessionName
    , msg : String
    , newModel : String
    }


type alias UpdateFromFrontend_ =
    { sessionName : SessionName
    , msg : String
    , newModel : String
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
