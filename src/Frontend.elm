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
import Set exposing (Set)
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

        UrlChanged url ->
            ( model, Cmd.none )

        PressedEvent index ->
            ( { model | selected = index }, Cmd.none )

        PressedResetSession ->
            ( model, Lamdera.sendToBackend ResetSessionRequest )

        GotRandomSessionName _ ->
            ( model, Cmd.none )

        TypedVariantFilter filter ->
            ( { model | filter = filter }, Cmd.none )


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
                        , filter = ""
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


eventIsHidden : Set String -> ElmValue -> Bool
eventIsHidden hiddenVariants elmValue =
    case Debug.log "a" elmValue of
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


instructionView : SessionName -> Element msg
instructionView sessionName =
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
            , code SyntaxHighlight.elm (codeAfter sessionName)
            ]
        ]


loadedSessionView : LoadedData -> Element FrontendMsg
loadedSessionView model =
    let
        filter : Set String
        filter =
            String.split "," model.filter |> List.map String.trim |> Set.fromList
    in
    if Array.isEmpty model.history && model.initialModel == Nothing then
        instructionView model.sessionName

    else
        Element.row
            [ Element.width Element.fill, Element.height Element.fill, Element.padding 8 ]
            [ Element.column
                [ Element.alignTop, Element.spacing 8, Element.width Element.fill ]
                [ Element.row
                    [ Element.spacing 4, Element.width Element.fill ]
                    [ Element.Input.button
                        [ Element.Background.color (Element.rgb 1 0.4 0.2), Element.padding 8, Element.height Element.fill ]
                        { onPress = Just PressedResetSession
                        , label = Element.text "Reset"
                        }
                    , Element.Input.text
                        [ Element.padding 4, Element.width Element.fill, Element.spacing 0 ]
                        { text = model.filter
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
                    [ Element.Background.color (Element.rgb 0.9 0.9 0.9) ]
                    (List.indexedMap Tuple.pair (Array.toList model.history)
                        |> List.filterMap
                            (\( index, event ) ->
                                if eventIsHidden filter (eventMsg event) then
                                    Nothing

                                else
                                    Just (eventView model.selected index event)
                            )
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
    Element.el [ Element.Font.color (Element.rgb 0.4 0.4 0.4) ] (Element.text "<Empty dict>")


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
                ElmSequence sequenceType elmValues ->
                    let
                        ( startChar, endChar ) =
                            sequenceStartEnd sequenceType
                    in
                    startChar ++ " ... " ++ endChar |> Element.text

                ElmType variant elmValues ->
                    Element.row [ Element.spacing 10 ] (variantText variant :: List.map singleLineView elmValues)

                ElmRecord list ->
                    Element.text "{ ... }"

                ElmDict list ->
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

                ElmRecord list ->
                    False

                ElmDict dict ->
                    List.isEmpty dict


treeViewDiff : ElmValue -> ElmValue -> Element msg
treeViewDiff oldValue value =
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
                                        [ Element.el [ Element.alignTop ] (variantText variant)
                                        , Element.text " "
                                        , treeViewDiff oldSingle single
                                        ]

                                else
                                    Element.column
                                        []
                                        [ variantText variant
                                        , Element.el
                                            [ tabAmount ]
                                            (treeViewDiff oldSingle single)
                                        ]

                            _ ->
                                Element.column
                                    []
                                    [ variantText variant
                                    , Element.column
                                        [ tabAmount ]
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
                                            [ tabAmount ]
                                            (treeViewDiff oldElmValue elmValue)
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
                                AssocList.fromList oldDict

                            dict2 =
                                AssocList.fromList dict

                            merge =
                                AssocList.merge
                                    (\key old state ->
                                        Element.column
                                            [ oldColor ]
                                            [ Element.row [] [ treeView key, Element.text ": " ]
                                            , Element.el [ tabAmount ] (treeView old)
                                            ]
                                            :: state
                                    )
                                    (\key old new state ->
                                        Element.column
                                            []
                                            [ Element.row [] [ treeView key, Element.text ": " ]
                                            , Element.el [ tabAmount ] (treeViewDiff old new)
                                            ]
                                            :: state
                                    )
                                    (\key new state ->
                                        Element.column
                                            [ newColor ]
                                            [ Element.row [] [ treeView key, Element.text ": " ]
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


tabAmount =
    Element.moveRight 24


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
                                        [ Element.row [ Element.alignTop ] [ treeView key, Element.text ": " ]
                                        , Element.el [ tabAmount ] (treeView value2)
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
        ([ Element.paddingXY 4 0
         , Element.width (Element.px 380)
         , Element.height (Element.px 28)
         , Element.Font.size 14
         , Element.clip
         ]
            ++ (if selected == index then
                    [ Element.Background.color (Element.rgb 0.7 0.9 0.7) ]

                else
                    []
               )
        )
        (case event of
            BackendMsgEvent { msg } ->
                { onPress = Just (PressedEvent index)
                , label = Element.row [] [ Element.text (String.fromInt index ++ ". "), singleLineView msg ]
                }

            ToBackendEvent { msg } ->
                { onPress = Just (PressedEvent index)
                , label = Element.row [] [ Element.text (String.fromInt index ++ ". "), singleLineView msg ]
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
