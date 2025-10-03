module Frontend exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation
import DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..))
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Env
import Html
import Html.Attributes
import Lamdera
import Process
import Random
import SeqDict
import SeqSet exposing (SeqSet)
import Set exposing (Set)
import Sha256
import SyntaxHighlight
import Task
import Time
import Types exposing (..)
import Url
import Url.Parser


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    case Url.Parser.parse Url.Parser.string url of
        Just sessionName ->
            ( LoadingSession
                { key = key
                , sessionName = SessionName sessionName
                }
            , Lamdera.sendToBackend (LoadSessionRequest (SessionName sessionName))
            )

        Nothing ->
            ( LoadingSession { key = key, sessionName = SessionName "" }
            , Random.generate
                GotRandomSessionName
                (Random.map
                    (\int -> String.fromInt int |> Sha256.sha224 |> String.left 16 |> SessionName)
                    (Random.int Random.minInt Random.maxInt)
                )
            )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case model of
        LoadingSession loading ->
            case msg of
                GotRandomSessionName sessionName ->
                    ( LoadingSession { loading | sessionName = sessionName }
                    , Cmd.batch
                        [ Lamdera.sendToBackend (LoadSessionRequest sessionName)
                        , Browser.Navigation.replaceUrl loading.key ("/" ++ sessionNameToString sessionName)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        LoadedSession loaded ->
            let
                ( newLoaded, cmd ) =
                    updateLoaded msg loaded
            in
            ( LoadedSession newLoaded, cmd )


updateLoaded : FrontendMsg -> LoadedData -> ( LoadedData, Cmd FrontendMsg )
updateLoaded msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        PressedEvent index ->
            ( { model | selected = index }, Cmd.none )

        PressedResetSession ->
            ( model, Lamdera.sendToBackend ResetSessionRequest )

        GotRandomSessionName _ ->
            ( model, Cmd.none )

        TypedVariantFilter filter ->
            updateSettings (\settings -> { settings | filter = filter }) model

        PressedCollapseField path ->
            updateSettings
                (\settings -> { settings | collapsedFields = SeqSet.insert path settings.collapsedFields })
                model

        PressedExpandField path ->
            updateSettings
                (\settings -> { settings | collapsedFields = SeqSet.remove path settings.collapsedFields })
                model

        DebounceFinished counter ->
            if counter == model.debounceCounter then
                ( model, Lamdera.sendToBackend (SetSessionSettingsRequest model.settings) )

            else
                ( model, Cmd.none )

        ScrolledToBottom ->
            ( model, Cmd.none )

        ToggledIsUsingProgramTest bool ->
            ( { model | isUsingProgramTest = bool }, Cmd.none )


updateSettings : (DebugSessionSettings -> DebugSessionSettings) -> LoadedData -> ( LoadedData, Cmd FrontendMsg )
updateSettings func model =
    ( { model | settings = func model.settings, debounceCounter = model.debounceCounter + 1 }
    , Process.sleep 1000 |> Task.perform (\() -> DebounceFinished (model.debounceCounter + 1))
    )


sessionNameToString : SessionName -> String
sessionNameToString (SessionName sessionName) =
    sessionName


historyContainerId : String
historyContainerId =
    "historyId"


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        LoadSessionResponse debugSession ->
            case model of
                LoadingSession loading ->
                    ( LoadedSession
                        { key = loading.key
                        , sessionName = loading.sessionName
                        , initialModel = debugSession.initialModel
                        , initialCmd = debugSession.initialCmd
                        , history = debugSession.history
                        , selected = Array.length debugSession.history - 1
                        , indexOffset = 0
                        , settings = debugSession.settings
                        , debounceCounter = 0
                        , isUsingProgramTest = False
                        }
                    , Browser.Dom.setViewportOf historyContainerId 0 99999999 |> Task.attempt (\_ -> ScrolledToBottom)
                    )

                _ ->
                    ( model, Cmd.none )

        SessionUpdate dataType time ->
            case model of
                LoadedSession loaded ->
                    let
                        historyLength =
                            Array.length loaded.history

                        msgsToDrop =
                            historyLength - 1000 |> max 0
                    in
                    case dataType of
                        Init init_ ->
                            ( LoadedSession
                                { loaded
                                    | initialModel = Just init_.model
                                    , history = Array.empty
                                    , selected = 0
                                }
                            , Cmd.none
                            )

                        Update update_ ->
                            let
                                selectedLatest =
                                    loaded.selected == Array.length loaded.history - 1
                            in
                            ( LoadedSession
                                { loaded
                                    | history =
                                        arrayPushSorted
                                            (BackendMsgEvent
                                                { msg = update_.msg
                                                , newModel = update_.newModel
                                                , cmd = update_.maybeCmd
                                                , time = Maybe.withDefault time update_.time
                                                }
                                            )
                                            loaded.history
                                            |> (\a -> Array.slice msgsToDrop (historyLength + 1) a)
                                    , indexOffset = loaded.indexOffset + msgsToDrop
                                    , selected =
                                        if selectedLatest then
                                            loaded.selected + 1 - msgsToDrop

                                        else
                                            loaded.selected - msgsToDrop
                                }
                            , if selectedLatest then
                                Browser.Dom.setViewportOf historyContainerId 0 99999999 |> Task.attempt (\_ -> ScrolledToBottom)

                              else
                                Cmd.none
                            )

                        UpdateFromFrontend update_ ->
                            ( LoadedSession
                                { loaded
                                    | history =
                                        arrayPushSorted
                                            (ToBackendEvent
                                                { msg = update_.msg
                                                , newModel = update_.newModel
                                                , sessionId = update_.sessionId
                                                , clientId = update_.clientId
                                                , cmd = update_.maybeCmd
                                                , time = Maybe.withDefault time update_.time
                                                }
                                            )
                                            loaded.history
                                            |> (\a -> Array.slice msgsToDrop (historyLength + 1) a)
                                    , indexOffset = loaded.indexOffset + msgsToDrop
                                    , selected =
                                        if loaded.selected == Array.length loaded.history - 1 then
                                            loaded.selected + 1 - msgsToDrop

                                        else
                                            loaded.selected - msgsToDrop
                                }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        ResetSession ->
            case model of
                LoadedSession loaded ->
                    ( LoadedSession { loaded | history = Array.empty, initialModel = Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


eventTime : Event -> Time.Posix
eventTime event =
    case event of
        BackendMsgEvent a ->
            a.time

        ToBackendEvent a ->
            a.time


arrayPushSorted : Event -> Array Event -> Array Event
arrayPushSorted item array =
    Array.toList array
        |> (::) item
        |> List.sortBy (\event -> eventTime event |> Time.posixToMillis)
        |> Array.fromList


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Debugger"
    , body =
        [ Html.node "style" [] [ Html.text css ]
        , Element.layout
            [ Element.Font.size 16
            , Element.inFront
                (Element.el
                    [ Element.alignBottom
                    , Element.alignRight
                    , Element.padding 8
                    , Element.Font.size 20
                    , Element.Background.color (Element.rgb 1 1 1)
                    ]
                    (link "https://github.com/MartinSStewart/lamdera-backend-debugger" "View source code")
                )
            ]
            (case model of
                LoadingSession _ ->
                    Element.none

                LoadedSession loaded ->
                    loadedSessionView loaded
            )
        ]
    }


link : String -> String -> Element msg
link url text =
    Element.newTabLink
        [ Element.Font.color (Element.rgb 0.1 0.3 0.9) ]
        { url = url, label = Element.text text }


eventIsHidden : Set String -> ElmValue -> Bool
eventIsHidden hiddenVariants elmValue =
    case elmValue of
        Plain _ ->
            False

        Expandable expandableValue ->
            case expandableValue of
                ElmSequence _ elmValues ->
                    List.any (eventIsHidden hiddenVariants) elmValues

                ElmType variant elmValues ->
                    Set.member variant hiddenVariants || List.any (eventIsHidden hiddenVariants) elmValues

                ElmRecord list ->
                    List.any (\( _, value ) -> eventIsHidden hiddenVariants value) list

                ElmDict list ->
                    List.any
                        (\( key, value ) ->
                            eventIsHidden hiddenVariants key
                                || eventIsHidden hiddenVariants value
                        )
                        list


checkIcon checked =
    Element.el
        [ Element.htmlAttribute (Html.Attributes.class "focusable")
        , Element.width
            (Element.px 18)
        , Element.height (Element.px 18)
        , Element.Font.color (Element.rgb 1 1 1)
        , Element.centerY
        , Element.Font.size 9
        , Element.Font.center
        , Element.Border.rounded 4
        , Element.Border.color <|
            if checked then
                Element.rgb (59 / 255) (153 / 255) (252 / 255)

            else
                Element.rgb (211 / 255) (211 / 255) (211 / 255)
        , Element.Border.shadow
            { offset = ( 0, 0 )
            , blur = 1
            , size = 1
            , color =
                if checked then
                    Element.rgba (238 / 255) (238 / 255) (238 / 255) 0

                else
                    Element.rgb (238 / 255) (238 / 255) (238 / 255)
            }
        , Element.Background.color <|
            if checked then
                Element.rgb (59 / 255) (153 / 255) (252 / 255)

            else
                Element.rgb 1 1 1
        , Element.Border.width <|
            if checked then
                0

            else
                1
        , Element.inFront
            (Element.el
                [ Element.Border.color (Element.rgb 1 1 1)
                , Element.height (Element.px 6)
                , Element.width (Element.px 9)
                , Element.rotate (degrees -45)
                , Element.centerX
                , Element.centerY
                , Element.moveUp 1
                , Element.transparent (not checked)
                , Element.Border.widthEach
                    { top = 0
                    , left = 2
                    , bottom = 2
                    , right = 0
                    }
                ]
                Element.none
            )
        ]
        Element.none


instructionView : Bool -> SessionName -> Element FrontendMsg
instructionView isUsingProgramTest sessionName =
    Element.column
        [ Element.spacing 32
        , Element.centerX
        , Element.centerY
        , Element.width (Element.px 600)
        , Element.Font.size 20
        ]
        [ Element.el
            [ Element.Background.color (Element.rgb255 250 250 240)
            , Element.Border.width 1
            , Element.Border.color (Element.rgb255 220 220 200)
            , Element.width Element.fill
            ]
            (Element.Input.checkbox
                [ Element.padding 8 ]
                { onChange = ToggledIsUsingProgramTest
                , icon = checkIcon
                , checked = isUsingProgramTest
                , label = Element.Input.labelRight [] (Element.text "Check this if you're using lamdera/program-test")
                }
            )

        --, Element.el [ Element.width Element.fill, Element.Border.width 1 ] Element.none
        , Element.column
            [ Element.spacing 24, Element.width Element.fill ]
            (if isUsingProgramTest then
                [ Element.paragraph
                    []
                    [ Element.text "To get started copy "
                    , link (Env.domain ++ "/EffectDebugApp.elm") "EffectDebugApp.elm"
                    , Element.text " into your src folder."
                    ]
                , Element.column
                    [ Element.spacing 8, Element.width Element.fill ]
                    [ Element.paragraph [] [ Element.text "Then replace" ]
                    , code SyntaxHighlight.elm codeBeforeProgramTest
                    , Element.text "with"
                    , code SyntaxHighlight.elm (codeAfterProgramTest sessionName)
                    ]
                ]

             else
                [ Element.paragraph
                    []
                    [ Element.text "To get started copy "
                    , link (Env.domain ++ "/DebugApp.elm") "DebugApp.elm"
                    , Element.text " into your src folder."
                    ]
                , Element.column
                    [ Element.spacing 8, Element.width Element.fill ]
                    [ Element.paragraph [] [ Element.text "Then replace" ]
                    , code SyntaxHighlight.elm codeBefore
                    , Element.text "with"
                    , code SyntaxHighlight.elm (codeAfter sessionName)
                    ]
                ]
            )
        ]


loadedSessionView : LoadedData -> Element FrontendMsg
loadedSessionView model =
    if Array.isEmpty model.history && model.initialModel == Nothing then
        instructionView model.isUsingProgramTest model.sessionName

    else
        let
            filter : Set String
            filter =
                String.split "," model.settings.filter |> List.map String.trim |> Set.fromList

            historyLength =
                Array.length model.history
        in
        Element.row
            [ Element.width Element.fill
            , Element.height (Element.minimum 0 Element.fill)
            , Element.padding 8
            , Element.scrollbars
            ]
            [ Element.column
                [ Element.alignTop
                , Element.spacing 8
                , Element.width (Element.px 380)
                , Element.scrollbars
                , Element.height (Element.minimum 0 Element.fill)
                ]
                [ Element.row
                    [ Element.spacing 4, Element.width Element.fill ]
                    [ Element.Input.button
                        [ Element.Background.color (Element.rgb 1 0.4 0.2), Element.padding 8, Element.height Element.fill ]
                        { onPress = Just PressedResetSession
                        , label = Element.text "Reset"
                        }
                    , Element.Input.text
                        [ Element.padding 4, Element.width Element.fill, Element.spacing 0 ]
                        { text = model.settings.filter
                        , onChange = TypedVariantFilter
                        , placeholder = Nothing
                        , label =
                            Element.Input.labelAbove
                                []
                                (Element.paragraph
                                    [ Element.Font.size 14 ]
                                    [ Element.text "Comma delimited list of variants to hide" ]
                                )
                        }
                    ]
                , Element.column
                    [ Element.Background.color (Element.rgb 0.9 0.9 0.9)
                    , Element.width Element.fill
                    , Element.height (Element.minimum 0 Element.fill)
                    , Element.scrollbars
                    , Html.Attributes.id historyContainerId |> Element.htmlAttribute
                    ]
                    (List.indexedMap Tuple.pair (Array.toList model.history)
                        |> List.filterMap
                            (\( index, event ) ->
                                if
                                    eventIsHidden filter (eventMsg event)
                                        && (index /= model.selected)
                                        && (index /= historyLength - 1)
                                then
                                    Nothing

                                else
                                    Just (eventView model index event)
                            )
                    )
                ]
            , -- I don't understand why but this extra column is necessary for scrolling to work
              Element.column
                [ Element.width Element.fill
                , Element.height (Element.minimum 0 Element.fill)
                , Element.scrollbars
                ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.height (Element.minimum 0 Element.fill)
                    , Element.padding 8
                    , Element.spacing 16
                    , Element.scrollbars
                    ]
                    [ case Array.get model.selected model.history of
                        Just event ->
                            Element.column
                                [ Element.Font.family [ Element.Font.monospace ], Element.spacing 4 ]
                                [ Element.el
                                    [ Element.Font.bold ]
                                    (case event of
                                        ToBackendEvent _ ->
                                            Element.text "ToBackend"

                                        BackendMsgEvent _ ->
                                            Element.text "BackendMsg"
                                    )
                                , treeView (eventMsg event)
                                ]

                        Nothing ->
                            Element.none
                    , case getCmd model.selected model of
                        Just cmd ->
                            Element.column
                                [ Element.width Element.fill
                                , Element.Font.family [ Element.Font.monospace ]
                                , Element.spacing 4
                                ]
                                [ Element.el [ Element.Font.bold ] (Element.text "Cmd")
                                , treeView cmd
                                ]

                        Nothing ->
                            Element.none
                    , case ( getModel (model.selected - 1) model, getModel model.selected model ) of
                        ( Just previousEvent, Just event ) ->
                            Element.column
                                [ Element.width Element.fill
                                , Element.Font.family [ Element.Font.monospace ]
                                , Element.spacing 4

                                --, Element.height (Element.minimum 0 Element.fill)
                                --, Element.scrollbars
                                ]
                                [ Element.el [ Element.Font.bold ] (Element.text "New model")
                                , treeViewDiff [] model.settings.collapsedFields previousEvent event
                                ]

                        ( Nothing, Just event ) ->
                            Element.column
                                [ Element.width Element.fill
                                , Element.Font.family [ Element.Font.monospace ]
                                , Element.spacing 4
                                ]
                                [ Element.el [ Element.Font.bold ] (Element.text "New model (initial model missing, no diff available)")
                                , treeView event
                                ]

                        _ ->
                            Element.none
                    ]
                ]
            ]


newColor =
    Element.Background.color (Element.rgb 0.8 1 0.8)


oldColor =
    Element.Background.color (Element.rgb 1 0.8 0.8)


numberText number =
    Element.el [ Element.Font.color (Element.rgb 0.6 0.3 0.4) ] (Element.text (String.fromFloat number))


stringText text =
    Element.el [ Element.Font.color (Element.rgb 0.2 0.5 0.2) ] (Element.text ("\"" ++ text ++ "\""))


charText char =
    Element.el [ Element.Font.color (Element.rgb 0.2 0.4 0.2) ] (Element.text ("'" ++ String.fromChar char ++ "'"))


emptyDict =
    Element.el [ Element.Font.color (Element.rgb 0.4 0.4 0.4) ] (Element.text "<empty dict>")


collapsedValue : ElmValue -> Element msg
collapsedValue value =
    Element.el
        [ Element.Font.color (Element.rgb 0.4 0.4 0.4) ]
        (Element.text
            (case value of
                Plain _ ->
                    "<primitive>"

                Expandable expandableValue ->
                    case expandableValue of
                        ElmSequence sequenceType elmValues ->
                            case sequenceType of
                                DebugParser.ElmValue.SeqSet ->
                                    "<set, " ++ items (List.length elmValues)

                                DebugParser.ElmValue.SeqList ->
                                    "<list, " ++ items (List.length elmValues)

                                DebugParser.ElmValue.SeqArray ->
                                    "<array, " ++ items (List.length elmValues)

                                DebugParser.ElmValue.SeqTuple ->
                                    "<tuple " ++ String.fromInt (List.length elmValues) ++ ">"

                        ElmType variant _ ->
                            "<" ++ variant ++ ">"

                        ElmRecord _ ->
                            "<record>"

                        ElmDict list ->
                            "<dict, " ++ items (List.length list)
            )
        )


items : Int -> String
items count =
    if count == 1 then
        "1 item>"

    else
        String.fromInt count ++ " items>"


variantText variant =
    Element.el [ Element.Font.color (Element.rgb 0.4 0.3 0.8) ] (Element.text variant)


plainValueToString : DebugParser.ElmValue.PlainValue -> Element msg
plainValueToString value =
    case value of
        DebugParser.ElmValue.ElmString string ->
            stringText string

        DebugParser.ElmValue.ElmChar char ->
            charText char

        DebugParser.ElmValue.ElmNumber float ->
            numberText float

        DebugParser.ElmValue.ElmBool bool ->
            if bool then
                Element.text "True"

            else
                Element.text "False"

        DebugParser.ElmValue.ElmFunction ->
            Element.text "<function>"

        DebugParser.ElmValue.ElmInternals ->
            Element.text "<internal>"

        DebugParser.ElmValue.ElmUnit ->
            Element.text "()"

        DebugParser.ElmValue.ElmFile string ->
            Element.text ("<file named " ++ string ++ ">")

        DebugParser.ElmValue.ElmBytes int ->
            Element.text ("<" ++ String.fromInt int ++ " bytes>")


singleLineView : ElmValue -> Element msg
singleLineView value =
    case value of
        Plain plainValue ->
            plainValueToString plainValue

        Expandable expandableValue ->
            case expandableValue of
                ElmSequence sequenceType _ ->
                    let
                        ( startChar, endChar ) =
                            sequenceStartEnd sequenceType
                    in
                    startChar ++ " ... " ++ endChar |> Element.text

                ElmType variant elmValues ->
                    Element.row [ Element.spacing 8 ] (variantText variant :: List.map singleLineView elmValues)

                ElmRecord _ ->
                    Element.text "{ ... }"

                ElmDict _ ->
                    Element.text "{{ ... }}"


sequenceStartEnd : DebugParser.ElmValue.SequenceType -> ( String, String )
sequenceStartEnd sequenceType =
    case sequenceType of
        DebugParser.ElmValue.SeqSet ->
            ( "{|", "|}" )

        DebugParser.ElmValue.SeqList ->
            ( "[", "]" )

        DebugParser.ElmValue.SeqArray ->
            ( "[|", "|]" )

        DebugParser.ElmValue.SeqTuple ->
            ( "(", ")" )


isSingleLine : ElmValue -> Bool
isSingleLine elmValue =
    case elmValue of
        Plain _ ->
            True

        Expandable expandable ->
            case expandable of
                ElmSequence _ elmValues ->
                    List.isEmpty elmValues

                ElmType _ elmValues ->
                    List.isEmpty elmValues

                ElmRecord _ ->
                    False

                ElmDict dict ->
                    List.isEmpty dict


indexedMap2 : (Int -> a -> b -> c) -> List a -> List b -> List c
indexedMap2 func listA listB =
    List.map2 Tuple.pair listA listB
        |> List.indexedMap (\index ( a, b ) -> func index a b)


treeViewDiff : List PathNode -> SeqSet (List PathNode) -> ElmValue -> ElmValue -> Element FrontendMsg
treeViewDiff currentPath collapsedFields oldValue value =
    case ( oldValue, value ) of
        ( Plain oldPlainValue, Plain plainValue ) ->
            if plainValue == oldPlainValue then
                plainValueToString plainValue

            else
                Element.column
                    []
                    [ plainValueToString oldPlainValue |> Element.el [ oldColor ]
                    , plainValueToString plainValue |> Element.el [ newColor ]
                    ]

        ( Expandable oldExpandableValue, Expandable expandableValue ) ->
            case ( oldExpandableValue, expandableValue ) of
                ( ElmSequence _ oldElmValues, ElmSequence sequenceType elmValues ) ->
                    if List.isEmpty oldElmValues && List.isEmpty elmValues then
                        let
                            ( startChar, endChar ) =
                                sequenceStartEnd sequenceType
                        in
                        Element.text (startChar ++ endChar)

                    else
                        let
                            ( startChar, endChar ) =
                                sequenceStartEnd sequenceType

                            lengthDiff =
                                List.length elmValues - List.length oldElmValues

                            newItems =
                                if lengthDiff > 0 then
                                    List.reverse elmValues
                                        |> List.take lengthDiff
                                        |> List.reverse
                                        |> List.map (\a -> Element.el [ newColor ] (treeView a))

                                else
                                    List.reverse oldElmValues
                                        |> List.take -lengthDiff
                                        |> List.reverse
                                        |> List.map (\a -> Element.el [ oldColor ] (treeView a))

                            pairedItems =
                                indexedMap2
                                    (\index old new ->
                                        treeViewDiff (SequenceNode index :: currentPath) collapsedFields old new
                                    )
                                    oldElmValues
                                    elmValues
                        in
                        Element.column
                            []
                            (List.indexedMap
                                (\index a ->
                                    if index == 0 then
                                        Element.row []
                                            [ Element.el
                                                [ Element.alignTop ]
                                                (Element.text (String.padRight 2 ' ' startChar))
                                            , a
                                            ]

                                    else
                                        Element.row []
                                            [ Element.el
                                                [ Element.alignTop ]
                                                (Element.text ", ")
                                            , a
                                            ]
                                )
                                (pairedItems ++ newItems)
                                ++ [ Element.text endChar ]
                            )

                ( ElmType oldVariant oldElmValues, ElmType variant elmValues ) ->
                    if oldVariant == variant then
                        case ( oldElmValues, elmValues ) of
                            ( [ oldSingle ], [ single ] ) ->
                                if isSingleLine single then
                                    Element.row
                                        []
                                        [ Element.el [ Element.alignTop ] (variantText variant)
                                        , Element.text " "
                                        , treeViewDiff
                                            (VariantNode variant :: currentPath)
                                            collapsedFields
                                            oldSingle
                                            single
                                        ]

                                else
                                    Element.column
                                        []
                                        [ variantText variant
                                        , Element.el
                                            [ tabAmount ]
                                            (treeViewDiff
                                                (VariantNode variant :: currentPath)
                                                collapsedFields
                                                oldSingle
                                                single
                                            )
                                        ]

                            _ ->
                                Element.column
                                    []
                                    [ variantText variant
                                    , Element.column
                                        [ tabAmount ]
                                        (List.map2
                                            (treeViewDiff (VariantNode variant :: currentPath) collapsedFields)
                                            oldElmValues
                                            elmValues
                                        )
                                    ]

                    else
                        Element.column
                            []
                            [ Element.el [ oldColor ] (treeView oldValue)
                            , Element.el [ newColor ] (treeView value)
                            ]

                ( ElmRecord oldRecord, ElmRecord record ) ->
                    Element.column
                        []
                        (List.map2
                            (\( _, oldElmValue ) ( fieldName, elmValue ) ->
                                let
                                    nextPath =
                                        FieldNode fieldName :: currentPath

                                    isCollapsed =
                                        SeqSet.member nextPath collapsedFields
                                in
                                if isSingleLine elmValue then
                                    Element.row []
                                        [ Element.el [ Element.alignTop ] (Element.text (fieldName ++ ": "))
                                        , treeViewDiff nextPath collapsedFields oldElmValue elmValue
                                        ]

                                else if isCollapsed then
                                    Element.row []
                                        [ Element.Input.button
                                            [ Element.alignTop ]
                                            { onPress = PressedExpandField nextPath |> Just
                                            , label = Element.text (fieldName ++ ": ")
                                            }
                                        , if oldElmValue == elmValue then
                                            collapsedValue elmValue

                                          else
                                            Element.el [ newColor ] (collapsedValue elmValue)
                                        ]

                                else
                                    Element.column []
                                        [ Element.Input.button
                                            []
                                            { onPress = PressedCollapseField nextPath |> Just
                                            , label = Element.text (fieldName ++ ": ")
                                            }
                                        , Element.el
                                            [ tabAmount ]
                                            (treeViewDiff nextPath collapsedFields oldElmValue elmValue)
                                        ]
                            )
                            oldRecord
                            record
                        )

                ( ElmDict oldDict, ElmDict dict ) ->
                    if List.isEmpty oldDict && List.isEmpty dict then
                        emptyDict

                    else
                        let
                            oldDict2 =
                                SeqDict.fromList oldDict

                            dict2 =
                                SeqDict.fromList dict

                            merge =
                                SeqDict.merge
                                    (\key old state ->
                                        Element.column
                                            [ oldColor ]
                                            [ dictKey key
                                            , Element.el [ tabAmount ] (treeView old)
                                            ]
                                            :: state
                                    )
                                    (\key old new state ->
                                        Element.column
                                            []
                                            [ dictKey key
                                            , Element.el
                                                [ tabAmount ]
                                                (treeViewDiff (DictNode key :: currentPath) collapsedFields old new)
                                            ]
                                            :: state
                                    )
                                    (\key new state ->
                                        Element.column
                                            [ newColor ]
                                            [ dictKey key
                                            , Element.el [ tabAmount ] (treeView new)
                                            ]
                                            :: state
                                    )
                                    oldDict2
                                    dict2
                                    []
                        in
                        Element.column [] merge

                _ ->
                    Element.text "Error, old and new types don't match"

        _ ->
            Element.text "Error, old and new types don't match"


tabAmount : Element.Attribute msg
tabAmount =
    Element.paddingEach { left = 24, right = 0, top = 0, bottom = 0 }


treeView : ElmValue -> Element msg
treeView value =
    case value of
        Plain plainValue ->
            plainValueToString plainValue

        Expandable expandableValue ->
            case expandableValue of
                DebugParser.ElmValue.ElmSequence sequenceType elmValues ->
                    if List.isEmpty elmValues then
                        let
                            ( startChar, endChar ) =
                                sequenceStartEnd sequenceType
                        in
                        Element.text (startChar ++ endChar)

                    else
                        let
                            ( startChar, endChar ) =
                                sequenceStartEnd sequenceType
                        in
                        Element.column
                            []
                            (List.indexedMap
                                (\index new ->
                                    if index == 0 then
                                        Element.row []
                                            [ Element.el
                                                [ Element.alignTop ]
                                                (Element.text (String.padRight 2 ' ' startChar))
                                            , treeView new
                                            ]

                                    else
                                        Element.row []
                                            [ Element.el
                                                [ Element.alignTop ]
                                                (Element.text ", ")
                                            , treeView new
                                            ]
                                )
                                elmValues
                                ++ [ Element.text endChar ]
                            )

                DebugParser.ElmValue.ElmType variant elmValues ->
                    case elmValues of
                        [ single ] ->
                            if isSingleLine single then
                                Element.row []
                                    [ Element.el [ Element.alignTop ] (variantText variant)
                                    , Element.text " "
                                    , treeView single
                                    ]

                            else
                                Element.column
                                    []
                                    [ variantText variant
                                    , Element.column [ tabAmount ] (List.map treeView elmValues)
                                    ]

                        _ ->
                            Element.column
                                []
                                [ variantText variant
                                , Element.column [ tabAmount ] (List.map treeView elmValues)
                                ]

                DebugParser.ElmValue.ElmRecord fields ->
                    Element.column
                        []
                        (List.map
                            (\( fieldName, elmValue ) ->
                                if isSingleLine elmValue then
                                    Element.row
                                        []
                                        [ Element.el [ Element.alignTop ] (Element.text (fieldName ++ ": "))
                                        , treeView elmValue
                                        ]

                                else
                                    Element.column
                                        []
                                        [ Element.text (fieldName ++ ": ")
                                        , Element.el [ tabAmount ] (treeView elmValue)
                                        ]
                            )
                            fields
                        )

                DebugParser.ElmValue.ElmDict dict ->
                    if List.isEmpty dict then
                        emptyDict

                    else
                        Element.column
                            []
                            (List.map
                                (\( key, value2 ) ->
                                    Element.column
                                        []
                                        [ dictKey key
                                        , Element.el [ tabAmount ] (treeView value2)
                                        ]
                                )
                                dict
                            )


dictKey : ElmValue -> Element msg
dictKey elmValue =
    let
        row () =
            Element.row [] [ treeView elmValue, Element.text ":" ]

        column () =
            Element.row
                [ Element.spacing 8 ]
                [ treeView elmValue
                , Element.el
                    [ Element.Border.widthEach { left = 2, right = 0, top = 0, bottom = 0 }
                    , Element.height Element.fill
                    , Element.Border.color (Element.rgb 0.4 0.4 0.4)
                    , Element.Font.color (Element.rgb 0.4 0.4 0.4)
                    ]
                    (Element.el [ Element.centerY ] (Element.text "(key)"))
                ]
    in
    case elmValue of
        Plain _ ->
            row ()

        Expandable expandable ->
            case expandable of
                ElmSequence _ _ ->
                    column ()

                ElmType _ elmValues ->
                    case elmValues of
                        [] ->
                            row ()

                        [ Plain _ ] ->
                            row ()

                        _ ->
                            column ()

                ElmRecord _ ->
                    column ()

                ElmDict _ ->
                    column ()


getModel :
    Int
    -> { a | initialModel : Maybe ElmValue, selected : Int, history : Array Event }
    -> Maybe ElmValue
getModel index model =
    if index < 0 then
        model.initialModel

    else
        Array.get index model.history |> Maybe.map eventNewModel


getCmd :
    Int
    -> { a | initialCmd : Maybe ElmValue, selected : Int, history : Array Event }
    -> Maybe ElmValue
getCmd index model =
    if index < 0 then
        model.initialCmd

    else
        Array.get index model.history |> Maybe.andThen eventCmd


eventMsg : Event -> ElmValue
eventMsg event =
    case event of
        BackendMsgEvent { msg } ->
            msg

        ToBackendEvent { msg } ->
            msg


eventNewModel : Event -> ElmValue
eventNewModel event =
    case event of
        BackendMsgEvent { newModel } ->
            newModel

        ToBackendEvent { newModel } ->
            newModel


eventCmd : Event -> Maybe ElmValue
eventCmd event =
    case event of
        BackendMsgEvent { cmd } ->
            cmd

        ToBackendEvent { cmd } ->
            cmd


eventView : LoadedData -> Int -> Event -> Element FrontendMsg
eventView model index event =
    let
        index2 =
            index + model.indexOffset + 1 |> String.fromInt
    in
    Element.Input.button
        ([ Element.paddingXY 4 0
         , Element.height (Element.px 28)
         , Element.Font.size 14
         , Element.width Element.fill
         ]
            ++ (if model.selected == index then
                    [ Element.Background.color (Element.rgb 0.7 0.9 0.7) ]

                else
                    []
               )
        )
        (case event of
            BackendMsgEvent { msg } ->
                { onPress = Just (PressedEvent index)
                , label = Element.row [] [ Element.text (index2 ++ ". "), singleLineView msg ]
                }

            ToBackendEvent { msg } ->
                { onPress = Just (PressedEvent index)
                , label = Element.row [] [ Element.text (index2 ++ ". "), singleLineView msg ]
                }
        )


ellipsis : String -> String
ellipsis text =
    if String.length text > 40 then
        String.left 38 text ++ "..."

    else
        text


code codeType text =
    case codeType text of
        Ok ok ->
            Html.div
                [ Html.Attributes.style "line-height" "22px" ]
                [ SyntaxHighlight.toInlineHtml ok ]
                |> Element.html
                |> Element.el
                    [ Element.Background.color (Element.rgb 0.95 0.95 0.95)
                    , Element.paddingXY 16 8
                    , Element.Border.rounded 4
                    , Element.Font.size 16
                    , Element.width Element.fill
                    ]

        Err _ ->
            Element.none


css =
    """
.elmsh {
    background: transparent;
    color: #24292e;
}

.elmsh-hl {
    background: #fffbdd;
}

.elmsh-add {
    background: #eaffea;
}

.elmsh-del {
    background: #ffecec;
}

.elmsh-strong {
    font-weight: bold;
}

.elmsh-emphasis {
    font-style: italic;
}

.elmsh1 {
    color: #969896;
}
.elmsh2 {
    color: #df5000;
}

.elmsh3 {
    color: #d73a49;
}

.elmsh4 {
    color: #0086b3;
}

.elmsh5 {
    color: #63a35c;
}

.elmsh6 {
    color: #005cc5;
}

.elmsh7 {
    color: #795da3;
}"""


codeBefore : String
codeBefore =
    """Lamdera.backend
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }
"""


codeAfter : SessionName -> String
codeAfter sessionName =
    """import DebugApp

DebugApp.backend
    NoOpBackendMsg
    \"""" ++ sessionNameToString sessionName ++ """"
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }
"""


codeBeforeProgramTest : String
codeBeforeProgramTest =
    """Effect.Lamdera.backend
    Lamdera.broadcast
    Lamdera.sendToFrontend
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }
"""


codeAfterProgramTest : SessionName -> String
codeAfterProgramTest sessionName =
    """import EffectDebugApp

EffectDebugApp.backend
    NoOpBackendMsg
    \"""" ++ sessionNameToString sessionName ++ """"
    Lamdera.broadcast
    Lamdera.sendToFrontend
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }
"""
