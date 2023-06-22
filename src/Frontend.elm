module Frontend exposing (..)

import Array exposing (Array)
import AssocList
import Browser exposing (UrlRequest(..))
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
import Random
import Sha256
import SyntaxHighlight
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
                    (Random.int
                        -(2 ^ 53 - 1)
                        (2 ^ 53 - 1)
                    )
                )
            )


getNavKey : FrontendModel -> Browser.Navigation.Key
getNavKey model =
    case model of
        LoadingSession loadingData ->
            loadingData.key

        LoadedSession loadedData ->
            loadedData.key


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl (getNavKey model) (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        PressedEvent index ->
            case model of
                LoadedSession loaded ->
                    ( LoadedSession { loaded | selected = index }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PressedResetSession ->
            ( model, Lamdera.sendToBackend ResetSessionRequest )

        GotRandomSessionName sessionName ->
            case model of
                LoadingSession loading ->
                    ( LoadingSession { loading | sessionName = sessionName }
                    , Cmd.batch
                        [ Lamdera.sendToBackend (LoadSessionRequest sessionName)
                        , Browser.Navigation.replaceUrl loading.key ("/" ++ sessionNameToString sessionName)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )


sessionNameToString : SessionName -> String
sessionNameToString (SessionName sessionName) =
    sessionName


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
                        , history = debugSession.history
                        , selected = Array.length debugSession.history - 1
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SessionUpdate dataType ->
            ( case model of
                LoadedSession loaded ->
                    case dataType of
                        Init init_ ->
                            LoadedSession { loaded | initialModel = Just init_.model, history = Array.empty }

                        Update update_ ->
                            LoadedSession
                                { loaded
                                    | history =
                                        Array.push
                                            (BackendMsgEvent { msg = update_.msg, newModel = update_.newModel })
                                            loaded.history
                                }

                        UpdateFromFrontend update_ ->
                            LoadedSession
                                { loaded
                                    | history =
                                        Array.push
                                            (ToBackendEvent
                                                { msg = update_.msg
                                                , newModel = update_.newModel
                                                , sessionId = update_.sessionId
                                                , clientId = update_.clientId
                                                }
                                            )
                                            loaded.history
                                }

                _ ->
                    model
            , Cmd.none
            )

        ResetSession ->
            case model of
                LoadedSession loaded ->
                    ( LoadedSession { loaded | history = Array.empty, initialModel = Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Debugger"
    , body =
        [ Html.node "style" [] [ Html.text css ]
        , Element.layout
            [ Element.Font.size 16 ]
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


loadedSessionView : LoadedData -> Element FrontendMsg
loadedSessionView model =
    if Array.isEmpty model.history && model.initialModel == Nothing then
        Element.column
            [ Element.spacing 24
            , Element.centerX
            , Element.centerY
            , Element.width (Element.px 600)
            , Element.Font.size 20
            ]
            [ Element.paragraph
                []
                [ Element.text "To get started copy "
                , link (Env.domain ++ "/DebugApp.elm") "DebugApp.elm"
                , Element.text " into your src folder (or "
                , link (Env.domain ++ "/EffectDebugApp.elm") "EffectDebugApp.elm"
                , Element.text " if you are using "
                , link "https://github.com/lamdera/program-test" "lamdera/program-test"
                , Element.text ")."
                ]
            , Element.column
                [ Element.spacing 8, Element.width Element.fill ]
                [ Element.paragraph [] [ Element.text "Then replace" ]
                , code SyntaxHighlight.elm codeBefore
                , Element.text "with"
                , code SyntaxHighlight.elm (codeAfter model.sessionName)
                ]
            ]

    else
        Element.row
            [ Element.width Element.fill, Element.height Element.fill ]
            [ Element.column
                [ Element.alignTop ]
                [ Element.Input.button
                    [ Element.Background.color (Element.rgb 1 0.4 0.2), Element.padding 8 ]
                    { onPress = Just PressedResetSession
                    , label = Element.text "Reset"
                    }
                , Element.column
                    [ Element.Background.color (Element.rgb 0.9 0.9 0.9) ]
                    (List.indexedMap
                        (eventView model.selected)
                        (Array.toList model.history)
                    )
                ]
            , Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 8
                , Element.spacing 16
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
                , case ( getModel (model.selected - 1) model, getModel model.selected model ) of
                    ( Just previousEvent, Just event ) ->
                        Element.column
                            [ Element.width Element.fill
                            , Element.Font.family [ Element.Font.monospace ]
                            , Element.spacing 4
                            ]
                            [ Element.el [ Element.Font.bold ] (Element.text "New model")
                            , treeViewDiff previousEvent event
                            ]

                    _ ->
                        Element.none
                ]
            ]


newColor =
    Element.Background.color (Element.rgb 0.7 1 0.7)


oldColor =
    Element.Background.color (Element.rgb 1 0.7 0.7)


plainValueToString value =
    case value of
        DebugParser.ElmValue.ElmString string ->
            "\"" ++ string ++ "\""

        DebugParser.ElmValue.ElmChar char ->
            "'" ++ String.fromChar char ++ "'"

        DebugParser.ElmValue.ElmNumber float ->
            String.fromFloat float

        DebugParser.ElmValue.ElmBool bool ->
            if bool then
                "True"

            else
                "False"

        DebugParser.ElmValue.ElmFunction ->
            "<function>"

        DebugParser.ElmValue.ElmInternals ->
            "<internal>"

        DebugParser.ElmValue.ElmUnit ->
            "()"

        DebugParser.ElmValue.ElmFile string ->
            "<file named " ++ string ++ ">"

        DebugParser.ElmValue.ElmBytes int ->
            "<" ++ String.fromInt int ++ " bytes>"


singleLineView : ElmValue -> String
singleLineView value =
    case value of
        Plain plainValue ->
            plainValueToString plainValue

        Expandable bool expandableValue ->
            case expandableValue of
                ElmSequence sequenceType elmValues ->
                    let
                        ( startChar, endChar ) =
                            sequenceStartEnd sequenceType
                    in
                    startChar ++ " ... " ++ endChar

                ElmType variant elmValues ->
                    variant ++ " " ++ String.join " " (List.map singleLineView elmValues)

                ElmRecord list ->
                    "{ ... }"

                ElmDict list ->
                    "{{ ... }}"


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


indexedMap2 : (Int -> a -> b -> c) -> List a -> List b -> List c
indexedMap2 func listA listB =
    List.map2 Tuple.pair listA listB
        |> List.indexedMap (\index ( a, b ) -> func index a b)


isSingleLine : ElmValue -> Bool
isSingleLine elmValue =
    case elmValue of
        Plain _ ->
            True

        Expandable _ expandable ->
            case expandable of
                ElmSequence _ elmValues ->
                    List.isEmpty elmValues

                ElmType _ elmValues ->
                    List.isEmpty elmValues

                ElmRecord list ->
                    False

                ElmDict dict ->
                    List.isEmpty dict


treeViewDiff : ElmValue -> ElmValue -> Element msg
treeViewDiff oldValue value =
    case ( oldValue, value ) of
        ( Plain oldPlainValue, Plain plainValue ) ->
            if plainValue == oldPlainValue then
                plainValueToString plainValue |> Element.text

            else
                Element.column
                    []
                    [ plainValueToString oldPlainValue |> Element.text |> Element.el [ oldColor ]
                    , plainValueToString plainValue |> Element.text |> Element.el [ newColor ]
                    ]

        ( Expandable _ oldExpandableValue, Expandable _ expandableValue ) ->
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
                                List.map2
                                    (\old new ->
                                        treeViewDiff old new
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
                                        [ Element.el [ Element.alignTop ] (Element.text (variant ++ " "))
                                        , treeViewDiff oldSingle single
                                        ]

                                else
                                    Element.column
                                        []
                                        [ Element.text variant
                                        , Element.el
                                            [ Element.moveRight 16 ]
                                            (treeViewDiff oldSingle single)
                                        ]

                            _ ->
                                Element.column
                                    []
                                    [ Element.text variant
                                    , Element.column
                                        [ Element.moveRight 16 ]
                                        (List.map2 treeViewDiff oldElmValues elmValues)
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
                                if isSingleLine elmValue then
                                    Element.row []
                                        [ Element.el [ Element.alignTop ] (Element.text (fieldName ++ ": "))
                                        , treeViewDiff oldElmValue elmValue
                                        ]

                                else
                                    Element.column []
                                        [ Element.text (fieldName ++ ": ")
                                        , Element.el
                                            [ Element.moveRight 16 ]
                                            (treeViewDiff oldElmValue elmValue)
                                        ]
                            )
                            oldRecord
                            record
                        )

                ( ElmDict oldDict, ElmDict dict ) ->
                    let
                        oldDict2 =
                            AssocList.fromList oldDict

                        dict2 =
                            AssocList.fromList dict

                        merge =
                            AssocList.merge
                                (\key old state ->
                                    Element.column
                                        [ oldColor ]
                                        [ Element.row [] [ treeView key, Element.text "; " ]
                                        , Element.el [ Element.moveRight 16 ] (treeView old)
                                        ]
                                        :: state
                                )
                                (\key old new state ->
                                    Element.column
                                        []
                                        [ Element.row [] [ treeView key, Element.text "; " ]
                                        , Element.el [ Element.moveRight 16 ] (treeViewDiff old new)
                                        ]
                                        :: state
                                )
                                (\key new state ->
                                    Element.column
                                        [ newColor ]
                                        [ Element.row [] [ treeView key, Element.text "; " ]
                                        , Element.el [ Element.moveRight 16 ] (treeView new)
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


treeView : ElmValue -> Element msg
treeView value =
    case value of
        Plain plainValue ->
            plainValueToString plainValue |> Element.text

        Expandable bool expandableValue ->
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
                                    [ Element.el [ Element.alignTop ] (Element.text (variant ++ " "))
                                    , treeView single
                                    ]

                            else
                                Element.column
                                    []
                                    [ Element.text variant
                                    , Element.column [ Element.moveRight 16 ] (List.map treeView elmValues)
                                    ]

                        _ ->
                            Element.column
                                []
                                [ Element.text variant
                                , Element.column [ Element.moveRight 16 ] (List.map treeView elmValues)
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
                                        , Element.el [ Element.moveRight 16 ] (treeView elmValue)
                                        ]
                            )
                            fields
                        )

                DebugParser.ElmValue.ElmDict dict ->
                    Element.column
                        []
                        (List.map
                            (\( key, value2 ) ->
                                Element.column
                                    []
                                    [ Element.row [ Element.alignTop ] [ treeView key, Element.text "; " ]
                                    , Element.el [ Element.moveRight 16 ] (treeView value2)
                                    ]
                            )
                            dict
                        )


getModel :
    Int
    -> { a | initialModel : Maybe ElmValue, selected : Int, history : Array Event }
    -> Maybe ElmValue
getModel index model =
    if index == -1 then
        model.initialModel

    else
        Array.get index model.history |> Maybe.map eventNewModel


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


eventView : Int -> Int -> Event -> Element FrontendMsg
eventView selected index event =
    Element.Input.button
        ([ Element.padding 4, Element.width (Element.px 380) ]
            ++ (if selected == index then
                    [ Element.Background.color (Element.rgb 0.7 0.9 0.7) ]

                else
                    []
               )
        )
        (case event of
            BackendMsgEvent { msg } ->
                { onPress = Just (PressedEvent index)
                , label = Element.text (String.fromInt index ++ ". " ++ ellipsis (singleLineView msg))
                }

            ToBackendEvent { msg } ->
                { onPress = Just (PressedEvent index)
                , label = Element.text (String.fromInt index ++ ". " ++ ellipsis (singleLineView msg))
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
    """DebugApp.backend
    NoOpBackendMsg
    \"""" ++ sessionNameToString sessionName ++ """"
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }
"""
