module Evergreen.V37.Types exposing (..)

import Array
import Browser
import Browser.Navigation
import Evergreen.V37.DebugParser.ElmValue
import Lamdera
import SeqDict
import SeqSet
import Set
import Time
import Url


type SessionName
    = SessionName String


type alias LoadingData =
    { key : Browser.Navigation.Key
    , sessionName : SessionName
    }


type alias BackendMsgEvent_ =
    { msg : Evergreen.V37.DebugParser.ElmValue.ElmValue
    , newModel : Evergreen.V37.DebugParser.ElmValue.ElmValue
    , cmd : Maybe Evergreen.V37.DebugParser.ElmValue.ElmValue
    , time : Time.Posix
    }


type alias ToBackendEvent_ =
    { msg : Evergreen.V37.DebugParser.ElmValue.ElmValue
    , newModel : Evergreen.V37.DebugParser.ElmValue.ElmValue
    , sessionId : String
    , clientId : String
    , cmd : Maybe Evergreen.V37.DebugParser.ElmValue.ElmValue
    , time : Time.Posix
    }


type Event
    = BackendMsgEvent BackendMsgEvent_
    | ToBackendEvent ToBackendEvent_


type PathNode
    = FieldNode String
    | VariantNode String
    | SequenceNode Int
    | DictNode Evergreen.V37.DebugParser.ElmValue.ElmValue


type alias DebugSessionSettings =
    { filter : String
    , collapsedFields : SeqSet.SeqSet (List PathNode)
    }


type alias LoadedData =
    { key : Browser.Navigation.Key
    , sessionName : SessionName
    , initialModel : Maybe Evergreen.V37.DebugParser.ElmValue.ElmValue
    , initialCmd : Maybe Evergreen.V37.DebugParser.ElmValue.ElmValue
    , history : Array.Array Event
    , selected : Int
    , indexOffset : Int
    , settings : DebugSessionSettings
    , debounceCounter : Int
    , isUsingProgramTest : Bool
    }


type FrontendModel
    = LoadingSession LoadingData
    | LoadedSession LoadedData


type alias DebugSession =
    { initialModel : Maybe Evergreen.V37.DebugParser.ElmValue.ElmValue
    , initialCmd : Maybe Evergreen.V37.DebugParser.ElmValue.ElmValue
    , history : Array.Array Event
    , connections : Set.Set Lamdera.ClientId
    , settings : DebugSessionSettings
    , lastChange : Time.Posix
    }


type alias BackendModel =
    { sessions : SeqDict.SeqDict SessionName DebugSession
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PressedEvent Int
    | PressedResetSession
    | GotRandomSessionName SessionName
    | TypedVariantFilter String
    | PressedCollapseField (List PathNode)
    | PressedExpandField (List PathNode)
    | DebounceFinished Int
    | ScrolledToBottom
    | ToggledIsUsingProgramTest Bool


type ToBackend
    = LoadSessionRequest SessionName
    | ResetSessionRequest
    | SetSessionSettingsRequest DebugSessionSettings


type alias Init_ =
    { sessionName : SessionName
    , model : Evergreen.V37.DebugParser.ElmValue.ElmValue
    , maybeCmd : Maybe Evergreen.V37.DebugParser.ElmValue.ElmValue
    , time : Maybe Time.Posix
    }


type alias Update_ =
    { sessionName : SessionName
    , msg : Evergreen.V37.DebugParser.ElmValue.ElmValue
    , newModel : Evergreen.V37.DebugParser.ElmValue.ElmValue
    , maybeCmd : Maybe Evergreen.V37.DebugParser.ElmValue.ElmValue
    , time : Maybe Time.Posix
    }


type alias UpdateFromFrontend_ =
    { sessionName : SessionName
    , msg : Evergreen.V37.DebugParser.ElmValue.ElmValue
    , newModel : Evergreen.V37.DebugParser.ElmValue.ElmValue
    , sessionId : String
    , clientId : String
    , maybeCmd : Maybe Evergreen.V37.DebugParser.ElmValue.ElmValue
    , time : Maybe Time.Posix
    }


type DataType
    = Init Init_
    | Update Update_
    | UpdateFromFrontend UpdateFromFrontend_


type BackendMsg
    = NoOpBackendMsg
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId
    | GotTime Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix
    | HourlyCheck Time.Posix
    | GotTimeForDataEndpoint Lamdera.SessionId DataType Time.Posix


type ToFrontend
    = LoadSessionResponse DebugSession
    | SessionUpdate DataType Time.Posix
    | ResetSession
