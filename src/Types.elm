module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import DebugParser.ElmValue exposing (ElmValue)
import Lamdera exposing (ClientId, SessionId)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Set exposing (Set)
import Time
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
    , initialCmd : Maybe ElmValue
    , history : Array Event
    , selected : Int
    , indexOffset : Int
    , settings : DebugSessionSettings
    , debounceCounter : Int
    , isUsingProgramTest : Bool
    }


type PathNode
    = FieldNode String
    | VariantNode String
    | SequenceNode Int
    | DictNode ElmValue


type alias BackendModel =
    { sessions : SeqDict SessionName DebugSession
    }


type SessionName
    = SessionName String


type DataType
    = Init Init_
    | Update Update_
    | UpdateFromFrontend UpdateFromFrontend_


type alias Init_ =
    { sessionName : SessionName
    , model : ElmValue
    , maybeCmd : Maybe ElmValue
    , time : Maybe Time.Posix
    }


type alias Update_ =
    { sessionName : SessionName
    , msg : ElmValue
    , newModel : ElmValue
    , maybeCmd : Maybe ElmValue
    , time : Maybe Time.Posix
    }


type alias UpdateFromFrontend_ =
    { sessionName : SessionName
    , msg : ElmValue
    , newModel : ElmValue
    , sessionId : String
    , clientId : String
    , maybeCmd : Maybe ElmValue
    , time : Maybe Time.Posix
    }


type alias DebugSession =
    { initialModel : Maybe ElmValue
    , initialCmd : Maybe ElmValue
    , history : Array Event
    , connections : Set ClientId
    , settings : DebugSessionSettings
    , lastChange : Time.Posix
    }


type alias DebugSessionSettings =
    { filter : String
    , collapsedFields : SeqSet (List PathNode)
    }


type Event
    = BackendMsgEvent BackendMsgEvent_
    | ToBackendEvent ToBackendEvent_


type alias BackendMsgEvent_ =
    { msg : ElmValue
    , newModel : ElmValue
    , cmd : Maybe ElmValue
    , time : Time.Posix
    }


type alias ToBackendEvent_ =
    { msg : ElmValue
    , newModel : ElmValue
    , sessionId : String
    , clientId : String
    , cmd : Maybe ElmValue
    , time : Time.Posix
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
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


type BackendMsg
    = NoOpBackendMsg
    | ClientDisconnected SessionId ClientId
    | GotTime SessionId ClientId ToBackend Time.Posix
    | HourlyCheck Time.Posix
    | GotTimeForDataEndpoint SessionId DataType Time.Posix


type ToFrontend
    = LoadSessionResponse DebugSession
    | SessionUpdate DataType Time.Posix
    | ResetSession
