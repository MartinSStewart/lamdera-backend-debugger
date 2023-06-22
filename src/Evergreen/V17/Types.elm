module Evergreen.V17.Types exposing (..)

import Array
import AssocList
import Browser
import Browser.Navigation
import Evergreen.V17.DebugParser.ElmValue
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
    { msg : Evergreen.V17.DebugParser.ElmValue.ElmValue
    , newModel : Evergreen.V17.DebugParser.ElmValue.ElmValue
    }


type alias ToBackendEvent_ =
    { msg : Evergreen.V17.DebugParser.ElmValue.ElmValue
    , newModel : Evergreen.V17.DebugParser.ElmValue.ElmValue
    , sessionId : String
    , clientId : String
    }


type Event
    = BackendMsgEvent BackendMsgEvent_
    | ToBackendEvent ToBackendEvent_


type alias LoadedData =
    { key : Browser.Navigation.Key
    , sessionName : SessionName
    , initialModel : Maybe Evergreen.V17.DebugParser.ElmValue.ElmValue
    , history : Array.Array Event
    , selected : Int
    , filter : String
    }


type FrontendModel
    = LoadingSession LoadingData
    | LoadedSession LoadedData


type alias DebugSession =
    { initialModel : Maybe Evergreen.V17.DebugParser.ElmValue.ElmValue
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
    | GotRandomSessionName SessionName
    | TypedVariantFilter String


type ToBackend
    = LoadSessionRequest SessionName
    | ResetSessionRequest


type BackendMsg
    = NoOpBackendMsg
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type alias Init_ =
    { sessionName : SessionName
    , model : Evergreen.V17.DebugParser.ElmValue.ElmValue
    }


type alias Update_ =
    { sessionName : SessionName
    , msg : Evergreen.V17.DebugParser.ElmValue.ElmValue
    , newModel : Evergreen.V17.DebugParser.ElmValue.ElmValue
    }


type alias UpdateFromFrontend_ =
    { sessionName : SessionName
    , msg : Evergreen.V17.DebugParser.ElmValue.ElmValue
    , newModel : Evergreen.V17.DebugParser.ElmValue.ElmValue
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
