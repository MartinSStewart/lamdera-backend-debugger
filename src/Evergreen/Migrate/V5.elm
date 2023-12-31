module Evergreen.Migrate.V5 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.com/docs/evergreen> for more info.

-}

import Array
import Evergreen.V4.Types
import Evergreen.V5.Types
import Lamdera.Migrations exposing (..)


frontendModel : Evergreen.V4.Types.FrontendModel -> ModelMigration Evergreen.V5.Types.FrontendModel Evergreen.V5.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Evergreen.V4.Types.BackendModel -> ModelMigration Evergreen.V5.Types.BackendModel Evergreen.V5.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V4.Types.FrontendMsg -> MsgMigration Evergreen.V5.Types.FrontendMsg Evergreen.V5.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V4.Types.ToBackend -> MsgMigration Evergreen.V5.Types.ToBackend Evergreen.V5.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V4.Types.BackendMsg -> MsgMigration Evergreen.V5.Types.BackendMsg Evergreen.V5.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V4.Types.ToFrontend -> MsgMigration Evergreen.V5.Types.ToFrontend Evergreen.V5.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Types_BackendMsgEvent_ : Evergreen.V4.Types.BackendMsgEvent_ -> Evergreen.V5.Types.BackendMsgEvent_
migrate_Types_BackendMsgEvent_ old =
    old


migrate_Types_Event : Evergreen.V4.Types.Event -> Evergreen.V5.Types.Event
migrate_Types_Event old =
    case old of
        Evergreen.V4.Types.BackendMsgEvent p0 ->
            Evergreen.V5.Types.BackendMsgEvent (p0 |> migrate_Types_BackendMsgEvent_)

        Evergreen.V4.Types.ToBackendEvent p0 ->
            Evergreen.V5.Types.ToBackendEvent (p0 |> migrate_Types_ToBackendEvent_)


migrate_Types_LoadedData : Evergreen.V4.Types.LoadedData -> Evergreen.V5.Types.LoadedData
migrate_Types_LoadedData old =
    { key = old.key
    , sessionName = old.sessionName |> migrate_Types_SessionName
    , initialModel = old.initialModel
    , history = old.history |> Array.map migrate_Types_Event
    , selected = old.selected
    }


migrate_Types_LoadingData : Evergreen.V4.Types.LoadingData -> Evergreen.V5.Types.LoadingData
migrate_Types_LoadingData old =
    { key = old.key
    , sessionName = old.sessionName |> migrate_Types_SessionName
    }


migrate_Types_SessionName : Evergreen.V4.Types.SessionName -> Evergreen.V5.Types.SessionName
migrate_Types_SessionName old =
    case old of
        Evergreen.V4.Types.SessionName p0 ->
            Evergreen.V5.Types.SessionName p0


migrate_Types_ToBackendEvent_ : Evergreen.V4.Types.ToBackendEvent_ -> Evergreen.V5.Types.ToBackendEvent_
migrate_Types_ToBackendEvent_ old =
    old
