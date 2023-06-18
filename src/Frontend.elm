module Frontend exposing (..)

import Array
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Env
import Html
import Html.Attributes
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
                , sessionName = sessionName
                }
            , Lamdera.sendToBackend (LoadSessionRequest sessionName)
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
                        , selected = 0
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


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Debugger"
    , body =
        [ Html.node "style" [] [ Html.text css ]
        , Element.layout
            []
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


loadedSessionView : LoadedData -> Element FrontendMsg
loadedSessionView model =
    if Array.isEmpty model.history && model.initialModel == Nothing then
        Element.el [ Element.centerX, Element.centerY ] (Element.text "No debug data yet")

    else
        Element.row
            [ Element.width Element.fill ]
            [ Element.column
                [ Element.Background.color (Element.rgb 0.9 0.9 0.9) ]
                (List.indexedMap
                    eventView
                    (Array.toList model.history)
                )
            ]


eventView : Int -> Event -> Element FrontendMsg
eventView index event =
    case event of
        BackendMsgEvent { msg } ->
            Element.Input.button
                []
                { onPress = Just (PressedEvent index)
                , label = Element.text msg
                }

        ToBackendEvent { msg } ->
            Element.Input.button
                []
                { onPress = Just (PressedEvent index)
                , label = Element.text msg
                }


homepageView model =
    Element.column
        [ Element.spacing 8, Element.centerX, Element.centerY, Element.width (Element.px 600) ]
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
