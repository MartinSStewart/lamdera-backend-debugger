module Types exposing (..)

import Array exposing (Array)
import AssocList
import AssocSet
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import DebugParser.ElmValue exposing (ElmValue)
import Dict exposing (Dict)
import Json.Decode
import Lamdera exposing (ClientId, SessionId)
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
    , settings : DebugSessionSettings
    , debounceCounter : Int
    }


type PathNode
    = FieldNode String
    | VariantNode String
    | SequenceNode Int
    | DictNode ElmValue


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
    { sessionName : SessionName, model : ElmValue, maybeCmd : Maybe ElmValue }


type alias Update_ =
    { sessionName : SessionName, msg : ElmValue, newModel : ElmValue, maybeCmd : Maybe ElmValue }


type alias UpdateFromFrontend_ =
    { sessionName : SessionName
    , msg : ElmValue
    , newModel : ElmValue
    , sessionId : String
    , clientId : String
    , maybeCmd : Maybe ElmValue
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
    , collapsedFields : AssocSet.Set (List PathNode)
    }


type Event
    = BackendMsgEvent BackendMsgEvent_
    | ToBackendEvent ToBackendEvent_


type alias BackendMsgEvent_ =
    { msg : ElmValue, newModel : ElmValue, cmd : Maybe ElmValue }


type alias ToBackendEvent_ =
    { msg : ElmValue, newModel : ElmValue, sessionId : String, clientId : String, cmd : Maybe ElmValue }


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
    | SessionUpdate DataType
    | ResetSession
