module Frontend exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import DebugParser.ElmValue exposing (ElmValue(..))
import DebugToJson
import Diff
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Env
import Html
import Html.Attributes
import Json.Encode
import Lamdera
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


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
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
            ( HomePage { key = key }
            , Cmd.none
            )


getNavKey : FrontendModel -> Nav.Key
getNavKey model =
    case model of
        LoadingSession loadingData ->
            loadingData.key

        LoadedSession loadedData ->
            loadedData.key

        HomePage record ->
            record.key


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl (getNavKey model) (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
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

                HomePage homepage ->
                    homepageView homepage
            )
        ]
    }


{-| Pretty print output from Debug.toString to JSON
-}
prettyPrint : String -> String
prettyPrint d =
    case DebugToJson.toJson d of
        Ok val ->
            Json.Encode.encode 2 val

        Err _ ->
            d


loadedSessionView : LoadedData -> Element FrontendMsg
loadedSessionView model =
    if Array.isEmpty model.history && model.initialModel == Nothing then
        Element.el [ Element.centerX, Element.centerY, Element.Font.size 20 ] (Element.text "No debug data yet")

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
                            []
                            [ Element.el
                                [ Element.Font.bold ]
                                (case event of
                                    ToBackendEvent _ ->
                                        Element.text "ToBackend"

                                    BackendMsgEvent _ ->
                                        Element.text "BackendMsg"
                                )
                            , treeView 0 (eventMsg event)
                            ]

                    Nothing ->
                        Element.none
                , case ( getModel (model.selected - 1) model, getModel model.selected model ) of
                    ( Just previousEvent, Just event ) ->
                        Element.column
                            [ Element.width Element.fill ]
                            [ Element.el [ Element.Font.bold ] (Element.text "New model")
                            , treeView 0 event
                            ]

                    _ ->
                        Element.none
                ]
            ]


treeView : Int -> ElmValue -> Element msg
treeView indentation value =
    case value of
        Plain plainValue ->
            case plainValue of
                DebugParser.ElmValue.ElmString string ->
                    "\"" ++ string ++ "\"" |> Element.text

                DebugParser.ElmValue.ElmChar char ->
                    "'" ++ String.fromChar char ++ "'" |> Element.text

                DebugParser.ElmValue.ElmNumber float ->
                    Element.text (String.fromFloat float)

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
                    "<file named " ++ string ++ ">" |> Element.text

                DebugParser.ElmValue.ElmBytes int ->
                    "<" ++ String.fromInt int ++ " bytes>" |> Element.text

        Expandable bool expandableValue ->
            case expandableValue of
                DebugParser.ElmValue.ElmSequence sequenceType elmValues ->
                    Element.column
                        [ Element.moveRight (toFloat indentation * 16) ]
                        (List.map (treeView (indentation + 1)) elmValues)

                DebugParser.ElmValue.ElmType string elmValues ->
                    Element.column
                        [ Element.moveRight (toFloat indentation * 16) ]
                        (List.map (treeView (indentation + 1)) elmValues)

                DebugParser.ElmValue.ElmRecord list ->
                    Element.column
                        [ Element.moveRight (toFloat indentation * 16) ]
                        (List.map (\( _, elmValue ) -> treeView (indentation + 1) elmValue) list)

                DebugParser.ElmValue.ElmDict list ->
                    Element.column
                        [ Element.moveRight (toFloat indentation * 16) ]
                        (List.map (\( _, elmValue ) -> treeView (indentation + 1) elmValue) list)


diffView : String -> String -> Element msg
diffView previousEvent event =
    Diff.diffLines previousEvent event
        |> List.map
            (\line ->
                case line of
                    Diff.Added text ->
                        Html.div
                            [ Html.Attributes.style "background-color" "rgba(100, 255, 100, 0.3)"
                            , Html.Attributes.style "padding" "2px"
                            ]
                            [ Html.text text ]

                    Diff.Removed text ->
                        Html.div
                            [ Html.Attributes.style "background-color" "rgba(255, 100, 100, 0.3)"
                            , Html.Attributes.style "padding" "2px"
                            ]
                            [ Html.text text ]

                    Diff.NoChange text ->
                        Html.div [ Html.Attributes.style "padding" "2px" ] [ Html.text text ]
            )
        |> Html.pre [ Html.Attributes.style "margin" "0" ]
        |> Element.html
        |> Element.el [ Element.width Element.fill ]


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
                , label = Element.text (String.fromInt index ++ ". ")
                }

            ToBackendEvent { msg } ->
                { onPress = Just (PressedEvent index)
                , label = Element.text (String.fromInt index ++ ". ")
                }
        )


ellipsis : String -> String
ellipsis text =
    if String.length text > 40 then
        String.left 38 text ++ "..."

    else
        text


homepageView : a -> Element msg
homepageView model =
    Element.column
        [ Element.spacing 8
        , Element.centerX
        , Element.centerY
        , Element.width (Element.px 600)
        , Element.Font.size 20
        ]
        [ Element.paragraph
            []
            [ Element.text "To get started copy "
            , Element.newTabLink
                [ Element.Font.color (Element.rgb 0.1 0.3 0.9) ]
                { url = Env.domain ++ "/DebugApp.elm", label = Element.text "this file" }
            , Element.text " into your src folder as DebugApp.elm, then replace "
            ]
        , code SyntaxHighlight.elm codeBefore
        , Element.text "with"
        , code SyntaxHighlight.elm codeAfter
        , Element.paragraph
            []
            [ Element.text "and finally navigate to "
            ]
        , code SyntaxHighlight.noLang (Env.domain ++ "/<name of your debug session>")
        , Element.paragraph [] [ Element.text "to view the debug history." ]
        ]


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


codeAfter : String
codeAfter =
    """DebugApp.backend
    NoOpBackendMsg
    "<name of your debug session>"
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }
"""
