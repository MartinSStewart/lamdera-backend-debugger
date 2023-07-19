module Evergreen.Migrate.V31 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Array
import AssocSet
import Evergreen.V29.DebugParser.ElmValue
import Evergreen.V29.Types
import Evergreen.V31.DebugParser.ElmValue
import Evergreen.V31.Types
import Lamdera.Migrations exposing (..)
import Maybe


frontendModel : Evergreen.V29.Types.FrontendModel -> ModelMigration Evergreen.V31.Types.FrontendModel Evergreen.V31.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V29.Types.BackendModel -> ModelMigration Evergreen.V31.Types.BackendModel Evergreen.V31.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V29.Types.FrontendMsg -> MsgMigration Evergreen.V31.Types.FrontendMsg Evergreen.V31.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V29.Types.ToBackend -> MsgMigration Evergreen.V31.Types.ToBackend Evergreen.V31.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V29.Types.BackendMsg -> MsgMigration Evergreen.V31.Types.BackendMsg Evergreen.V31.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V29.Types.ToFrontend -> MsgMigration Evergreen.V31.Types.ToFrontend Evergreen.V31.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_AssocSet_Set : (a_old -> a_new) -> AssocSet.Set a_old -> AssocSet.Set a_new
migrate_AssocSet_Set migrate_a old =
    old |> AssocSet.map migrate_a


migrate_DebugParser_ElmValue_ElmValue : Evergreen.V29.DebugParser.ElmValue.ElmValue -> Evergreen.V31.DebugParser.ElmValue.ElmValue
migrate_DebugParser_ElmValue_ElmValue old =
    case old of
        Evergreen.V29.DebugParser.ElmValue.Plain p0 ->
            Evergreen.V31.DebugParser.ElmValue.Plain (p0 |> migrate_DebugParser_ElmValue_PlainValue)

        Evergreen.V29.DebugParser.ElmValue.Expandable p0 ->
            Evergreen.V31.DebugParser.ElmValue.Expandable (p0 |> migrate_DebugParser_ElmValue_ExpandableValue)


migrate_DebugParser_ElmValue_ExpandableValue : Evergreen.V29.DebugParser.ElmValue.ExpandableValue -> Evergreen.V31.DebugParser.ElmValue.ExpandableValue
migrate_DebugParser_ElmValue_ExpandableValue old =
    case old of
        Evergreen.V29.DebugParser.ElmValue.ElmSequence p0 p1 ->
            Evergreen.V31.DebugParser.ElmValue.ElmSequence (p0 |> migrate_DebugParser_ElmValue_SequenceType)
                (p1 |> List.map migrate_DebugParser_ElmValue_ElmValue)

        Evergreen.V29.DebugParser.ElmValue.ElmType p0 p1 ->
            Evergreen.V31.DebugParser.ElmValue.ElmType p0
                (p1 |> List.map migrate_DebugParser_ElmValue_ElmValue)

        Evergreen.V29.DebugParser.ElmValue.ElmRecord p0 ->
            Evergreen.V31.DebugParser.ElmValue.ElmRecord (p0 |> List.map (Tuple.mapSecond migrate_DebugParser_ElmValue_ElmValue))

        Evergreen.V29.DebugParser.ElmValue.ElmDict p0 ->
            Evergreen.V31.DebugParser.ElmValue.ElmDict (p0 |> List.map (Tuple.mapBoth migrate_DebugParser_ElmValue_ElmValue migrate_DebugParser_ElmValue_ElmValue))


migrate_DebugParser_ElmValue_PlainValue : Evergreen.V29.DebugParser.ElmValue.PlainValue -> Evergreen.V31.DebugParser.ElmValue.PlainValue
migrate_DebugParser_ElmValue_PlainValue old =
    case old of
        Evergreen.V29.DebugParser.ElmValue.ElmString p0 ->
            Evergreen.V31.DebugParser.ElmValue.ElmString p0

        Evergreen.V29.DebugParser.ElmValue.ElmChar p0 ->
            Evergreen.V31.DebugParser.ElmValue.ElmChar p0

        Evergreen.V29.DebugParser.ElmValue.ElmNumber p0 ->
            Evergreen.V31.DebugParser.ElmValue.ElmNumber p0

        Evergreen.V29.DebugParser.ElmValue.ElmBool p0 ->
            Evergreen.V31.DebugParser.ElmValue.ElmBool p0

        Evergreen.V29.DebugParser.ElmValue.ElmFunction ->
            Evergreen.V31.DebugParser.ElmValue.ElmFunction

        Evergreen.V29.DebugParser.ElmValue.ElmInternals ->
            Evergreen.V31.DebugParser.ElmValue.ElmInternals

        Evergreen.V29.DebugParser.ElmValue.ElmUnit ->
            Evergreen.V31.DebugParser.ElmValue.ElmUnit

        Evergreen.V29.DebugParser.ElmValue.ElmFile p0 ->
            Evergreen.V31.DebugParser.ElmValue.ElmFile p0

        Evergreen.V29.DebugParser.ElmValue.ElmBytes p0 ->
            Evergreen.V31.DebugParser.ElmValue.ElmBytes p0


migrate_DebugParser_ElmValue_SequenceType : Evergreen.V29.DebugParser.ElmValue.SequenceType -> Evergreen.V31.DebugParser.ElmValue.SequenceType
migrate_DebugParser_ElmValue_SequenceType old =
    case old of
        Evergreen.V29.DebugParser.ElmValue.SeqSet ->
            Evergreen.V31.DebugParser.ElmValue.SeqSet

        Evergreen.V29.DebugParser.ElmValue.SeqList ->
            Evergreen.V31.DebugParser.ElmValue.SeqList

        Evergreen.V29.DebugParser.ElmValue.SeqArray ->
            Evergreen.V31.DebugParser.ElmValue.SeqArray

        Evergreen.V29.DebugParser.ElmValue.SeqTuple ->
            Evergreen.V31.DebugParser.ElmValue.SeqTuple


migrate_Types_BackendMsgEvent_ : Evergreen.V29.Types.BackendMsgEvent_ -> Evergreen.V31.Types.BackendMsgEvent_
migrate_Types_BackendMsgEvent_ old =
    { msg = old.msg |> migrate_DebugParser_ElmValue_ElmValue
    , newModel = old.newModel |> migrate_DebugParser_ElmValue_ElmValue
    , cmd = old.cmd |> Maybe.map migrate_DebugParser_ElmValue_ElmValue
    }


migrate_Types_DebugSessionSettings : Evergreen.V29.Types.DebugSessionSettings -> Evergreen.V31.Types.DebugSessionSettings
migrate_Types_DebugSessionSettings old =
    { filter = old.filter
    , collapsedFields = old.collapsedFields |> migrate_AssocSet_Set (List.map migrate_Types_PathNode)
    }


migrate_Types_Event : Evergreen.V29.Types.Event -> Evergreen.V31.Types.Event
migrate_Types_Event old =
    case old of
        Evergreen.V29.Types.BackendMsgEvent p0 ->
            Evergreen.V31.Types.BackendMsgEvent (p0 |> migrate_Types_BackendMsgEvent_)

        Evergreen.V29.Types.ToBackendEvent p0 ->
            Evergreen.V31.Types.ToBackendEvent (p0 |> migrate_Types_ToBackendEvent_)


migrate_Types_FrontendModel : Evergreen.V29.Types.FrontendModel -> Evergreen.V31.Types.FrontendModel
migrate_Types_FrontendModel old =
    case old of
        Evergreen.V29.Types.LoadingSession p0 ->
            Evergreen.V31.Types.LoadingSession (p0 |> migrate_Types_LoadingData)

        Evergreen.V29.Types.LoadedSession p0 ->
            Evergreen.V31.Types.LoadedSession (p0 |> migrate_Types_LoadedData)


migrate_Types_LoadedData : Evergreen.V29.Types.LoadedData -> Evergreen.V31.Types.LoadedData
migrate_Types_LoadedData old =
    { key = old.key
    , sessionName = old.sessionName |> migrate_Types_SessionName
    , initialModel = old.initialModel |> Maybe.map migrate_DebugParser_ElmValue_ElmValue
    , initialCmd = old.initialCmd |> Maybe.map migrate_DebugParser_ElmValue_ElmValue
    , history = old.history |> Array.map migrate_Types_Event
    , selected = old.selected
    , indexOffset = 0
    , settings = old.settings |> migrate_Types_DebugSessionSettings
    , debounceCounter = old.debounceCounter
    }


migrate_Types_LoadingData : Evergreen.V29.Types.LoadingData -> Evergreen.V31.Types.LoadingData
migrate_Types_LoadingData old =
    { key = old.key
    , sessionName = old.sessionName |> migrate_Types_SessionName
    }


migrate_Types_PathNode : Evergreen.V29.Types.PathNode -> Evergreen.V31.Types.PathNode
migrate_Types_PathNode old =
    case old of
        Evergreen.V29.Types.FieldNode p0 ->
            Evergreen.V31.Types.FieldNode p0

        Evergreen.V29.Types.VariantNode p0 ->
            Evergreen.V31.Types.VariantNode p0

        Evergreen.V29.Types.SequenceNode p0 ->
            Evergreen.V31.Types.SequenceNode p0

        Evergreen.V29.Types.DictNode p0 ->
            Evergreen.V31.Types.DictNode (p0 |> migrate_DebugParser_ElmValue_ElmValue)


migrate_Types_SessionName : Evergreen.V29.Types.SessionName -> Evergreen.V31.Types.SessionName
migrate_Types_SessionName old =
    case old of
        Evergreen.V29.Types.SessionName p0 ->
            Evergreen.V31.Types.SessionName p0


migrate_Types_ToBackendEvent_ : Evergreen.V29.Types.ToBackendEvent_ -> Evergreen.V31.Types.ToBackendEvent_
migrate_Types_ToBackendEvent_ old =
    { msg = old.msg |> migrate_DebugParser_ElmValue_ElmValue
    , newModel = old.newModel |> migrate_DebugParser_ElmValue_ElmValue
    , sessionId = old.sessionId
    , clientId = old.clientId
    , cmd = old.cmd |> Maybe.map migrate_DebugParser_ElmValue_ElmValue
    }