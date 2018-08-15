port module Main exposing (main)

import Dom.Scroll
import Html exposing (..)
import Html.Attributes exposing (id, type_, value, src, class, style, disabled)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Http
import Json.Decode
import Json.Encode
import Random
import Task


-- ports


port startVoiceTyping : () -> Cmd a


port stopVoiceTyping : () -> Cmd a


port switchLanguage : String -> Cmd a


port voiceTyping : (String -> msg) -> Sub msg


port updateVoiceTypingStatus : (Bool -> msg) -> Sub msg



-- data


type Language
    = En
    | Zh_HK
    | Zh_TW
    | Zh_CN


languageString : Language -> String
languageString lang =
    case lang of
        En ->
            "en"

        Zh_HK ->
            "zh-HK"

        Zh_TW ->
            "zh-TW"

        Zh_CN ->
            "zh-CN"


languageDisplayText : Language -> String
languageDisplayText lang =
    case lang of
        En ->
            "English"

        Zh_HK ->
            "中文(香港)"

        Zh_TW ->
            "中文(台灣)"

        Zh_CN ->
            "中文(大陸)"


i18nText : Language -> String -> String
i18nText lang text =
    case ( text, lang ) of
        ( "more", Zh_HK ) ->
            "更多資料"

        ( "more", Zh_TW ) ->
            "更多資料"

        ( "more", Zh_CN ) ->
            "更多资料"

        ( "Unknown error...", Zh_HK ) ->
            "好似有D狀況出現左，我都唔知咩野事..."

        ( "Unknown error...", Zh_TW ) ->
            "有一點狀況出現了，但我不知道發生了什麽問題..."

        _ ->
            text


type alias ButtonItem =
    { text : String
    , responseAction : String
    , responseValue : String
    }


type alias ListItem =
    { thumbnail : Maybe String
    , description : String
    , responseAction : String
    , responseValue : String
    }


type alias CardMessageItem =
    { title : String
    , text : String
    , thumbnail : Maybe String
    , buttons : List ButtonItem
    }


type Message
    = TextMessage String
    | ListItemMessage String (List ListItem)
    | CardMessage CardMessageItem


fromDialogFlowV1FulfillmentWebDataAttachment : DialogFlowV1FulfillmentWebDataAttachment -> List Message
fromDialogFlowV1FulfillmentWebDataAttachment attachment =
    case attachment of
        WebDataListAttachment listAtt ->
            [ ListItemMessage listAtt.title <|
                List.map
                    (\item ->
                        { thumbnail = item.thumbnailSrc
                        , description = item.text
                        , responseAction = item.responseEvent
                        , responseValue = item.responseValue
                        }
                    )
                    listAtt.items
            ]

        WebDataCardAttachment cardAtt ->
            [ CardMessage
                { title = cardAtt.title
                , text = cardAtt.text
                , thumbnail = cardAtt.thumbnailSrc
                , buttons =
                    cardAtt.buttons
                        |> List.map
                            (\bu ->
                                ButtonItem
                                    bu.title
                                    bu.responseEvent
                                    (Maybe.withDefault "" bu.responseValue)
                            )
                }
            ]


fromDialogFlowV2Response : DialogFlowV2Response -> List Message
fromDialogFlowV2Response response =
    let
        webdata =
            response.queryResult.webhookPayload
                |> Maybe.andThen (\d -> d.web)
    in
        case webdata of
            Just web ->
                TextMessage web.text
                    :: fromDialogFlowV1FulfillmentWebDataAttachment web.attachment

            Nothing ->
                TextMessage response.queryResult.fulfillmentText :: []


type ConversationMessage
    = ConversationMessage String Message


type alias DialogFlowV2Response =
    { responseId : String
    , queryResult : DialogFlowV2Result
    , status : Maybe DialogFlowV2Status
    }


type alias DialogFlowV2Result =
    { queryText : String
    , action : String
    , fulfillmentText : String
    , fulfillmentMessages : List DialogFlowV2FulfillmentMessages
    , webhookPayload : Maybe DialogFlowV1FulfillmentData
    , intentDetectionConfidence : Float
    }


type alias DialogFlowV2Status =
    { message : String }


type alias DialogFlowV2FulfillmentMessages =
    { text : DialogFlowV2FulfillmentMessageText
    }


type alias DialogFlowV2FulfillmentMessageText =
    { text : List String
    }


type alias DialogFlowV1FulfillmentData =
    { web : Maybe DialogFlowV1FulfillmentWebData }


type alias DialogFlowV1FulfillmentWebData =
    { text : String
    , attachment : DialogFlowV1FulfillmentWebDataAttachment
    }


type DialogFlowV1FulfillmentWebDataAttachment
    = WebDataListAttachment DialogFlowV1FulfillmentWebDataListAttachment
    | WebDataCardAttachment DialogFlowV1FulfillmentWebDataCardAttachment


type alias DialogFlowV1FulfillmentWebDataListAttachment =
    { type_ : String
    , title : String
    , items : List DialogFlowV1FulfillmentWebDataListAttachmentItem
    }


type alias DialogFlowV1FulfillmentWebDataListAttachmentItem =
    { text : String
    , responseEvent : String
    , responseValue : String
    , thumbnailSrc : Maybe String
    }


type alias DialogFlowV1FulfillmentWebDataCardAttachment =
    { type_ : String
    , title : String
    , text : String
    , thumbnailSrc : Maybe String
    , buttons : List DialogFlowV1FulfillmentWebDataCardAttachmentButton
    }


type alias DialogFlowV1FulfillmentWebDataCardAttachmentButton =
    { title : String
    , responseEvent : String
    , responseValue : Maybe String
    }



-- decoder


dialogFlowV2ResponseDecoder : Json.Decode.Decoder DialogFlowV2Response
dialogFlowV2ResponseDecoder =
    Json.Decode.map3 DialogFlowV2Response
        (Json.Decode.field "responseId" Json.Decode.string)
        (Json.Decode.field "queryResult" dialogFlowV2ResultDecoder)
        (Json.Decode.maybe <| Json.Decode.field "webhookStatus" dialogFlowV2StatusDecoder)


dialogFlowV2ResultDecoder : Json.Decode.Decoder DialogFlowV2Result
dialogFlowV2ResultDecoder =
    Json.Decode.map6 DialogFlowV2Result
        (Json.Decode.field "queryText" Json.Decode.string)
        (Json.Decode.field "action" Json.Decode.string)
        (Json.Decode.field "fulfillmentText" Json.Decode.string)
        (Json.Decode.field "fulfillmentMessages" <|
            Json.Decode.list <|
                Json.Decode.map DialogFlowV2FulfillmentMessages
                    (Json.Decode.field "text" dialogFlowV2FulfillmentMessageTextDecoder)
        )
        (Json.Decode.maybe <| Json.Decode.field "webhookPayload" dialogFlowV1FulfillmentDataDecoder)
        (Json.Decode.field "intentDetectionConfidence" Json.Decode.float)


dialogFlowV2FulfillmentMessageTextDecoder : Json.Decode.Decoder DialogFlowV2FulfillmentMessageText
dialogFlowV2FulfillmentMessageTextDecoder =
    Json.Decode.map DialogFlowV2FulfillmentMessageText
        (Json.Decode.field "text" <|
            Json.Decode.list <|
                Json.Decode.string
        )


dialogFlowV1FulfillmentDataDecoder : Json.Decode.Decoder DialogFlowV1FulfillmentData
dialogFlowV1FulfillmentDataDecoder =
    Json.Decode.map DialogFlowV1FulfillmentData
        (Json.Decode.maybe <| Json.Decode.field "web" dialogFlowV1FulfillmentWebDataDecoder)


dialogFlowV1FulfillmentWebDataDecoder : Json.Decode.Decoder DialogFlowV1FulfillmentWebData
dialogFlowV1FulfillmentWebDataDecoder =
    Json.Decode.map2 DialogFlowV1FulfillmentWebData
        (Json.Decode.field "text" Json.Decode.string)
        (Json.Decode.field "attachment" dialogFlowV1FulfillmentWebDataAttachmentDecoder)


dialogFlowV1FulfillmentWebDataAttachmentDecoder : Json.Decode.Decoder DialogFlowV1FulfillmentWebDataAttachment
dialogFlowV1FulfillmentWebDataAttachmentDecoder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\attType ->
                case attType of
                    "LIST" ->
                        Json.Decode.map
                            WebDataListAttachment
                            dialogFlowV1FulfillmentWebDataListAttachmentDecoder

                    "CARD" ->
                        Json.Decode.map
                            WebDataCardAttachment
                            dialogFlowV1FulfillmentWebDataCardAttachmentDecoder

                    _ ->
                        Json.Decode.fail "Invalid attachment type"
            )


dialogFlowV1FulfillmentWebDataListAttachmentDecoder : Json.Decode.Decoder DialogFlowV1FulfillmentWebDataListAttachment
dialogFlowV1FulfillmentWebDataListAttachmentDecoder =
    Json.Decode.map3 DialogFlowV1FulfillmentWebDataListAttachment
        (Json.Decode.field "type" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "items" <|
            Json.Decode.list dialogFlowV1FulfillmentWebDataListAttachmentItemDecoder
        )


dialogFlowV1FulfillmentWebDataListAttachmentItemDecoder : Json.Decode.Decoder DialogFlowV1FulfillmentWebDataListAttachmentItem
dialogFlowV1FulfillmentWebDataListAttachmentItemDecoder =
    Json.Decode.map4 DialogFlowV1FulfillmentWebDataListAttachmentItem
        (Json.Decode.field "text" Json.Decode.string)
        (Json.Decode.field "responseEvent" Json.Decode.string)
        (Json.Decode.field "responseValue" Json.Decode.string)
        (Json.Decode.maybe <| Json.Decode.field "thumbnailSrc" Json.Decode.string)


dialogFlowV1FulfillmentWebDataCardAttachmentDecoder : Json.Decode.Decoder DialogFlowV1FulfillmentWebDataCardAttachment
dialogFlowV1FulfillmentWebDataCardAttachmentDecoder =
    Json.Decode.map5 DialogFlowV1FulfillmentWebDataCardAttachment
        (Json.Decode.field "type" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "text" Json.Decode.string)
        (Json.Decode.maybe <| Json.Decode.field "thumbnailSrc" Json.Decode.string)
        (Json.Decode.field "buttons" <|
            Json.Decode.list dialogFlowV1FulfillmentWebDataCardAttachmentButtonDecoder
        )


dialogFlowV1FulfillmentWebDataCardAttachmentButtonDecoder : Json.Decode.Decoder DialogFlowV1FulfillmentWebDataCardAttachmentButton
dialogFlowV1FulfillmentWebDataCardAttachmentButtonDecoder =
    Json.Decode.map3 DialogFlowV1FulfillmentWebDataCardAttachmentButton
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "responseEvent" Json.Decode.string)
        (Json.Decode.maybe <| Json.Decode.field "responseValue" Json.Decode.string)


dialogFlowV2StatusDecoder : Json.Decode.Decoder DialogFlowV2Status
dialogFlowV2StatusDecoder =
    Json.Decode.map DialogFlowV2Status
        (Json.Decode.field "message" Json.Decode.string)



-- model


type alias Model =
    { accessToken : String
    , hasVoiceTyping : Bool
    , voiceTypingEnabled : Bool
    , sessionID : String
    , language : Language
    , inputText : String
    , conversation : List ConversationMessage
    }


type alias Flags =
    { hasSpeechRecognition : Bool
    , accessToken : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags.accessToken flags.hasSpeechRecognition False "" En "" []
    , Random.int 1 1000
        |> Random.map ((++) "SESSION_" << toString)
        |> Random.generate SetSessionID
    )



-- update


type Msg
    = SetSessionID String
    | NoOp
    | TypingInput String
    | VoiceTypingInput String
    | InputBoxKeyDown Int
    | ToggleVoiceTyping
    | UpdateVoiceTypingStatus Bool
    | SwitchLanguage Language
    | SendResponseAction String String
    | DialogFlowResponse (Result Http.Error DialogFlowV2Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSessionID sessionID ->
            ( { model | sessionID = sessionID }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        TypingInput inputText ->
            ( { model | inputText = inputText }, Cmd.none )

        VoiceTypingInput inputText ->
            ( { model | inputText = model.inputText ++ inputText }, Cmd.none )

        InputBoxKeyDown code ->
            -- when user hit enter
            if code == 13 && (not (String.isEmpty model.inputText)) then
                let
                    userMessage =
                        ConversationMessage "Me" (TextMessage model.inputText)
                in
                    ( { model
                        | inputText = ""
                        , conversation = model.conversation ++ [ userMessage ]
                      }
                    , sendMessage
                        model.accessToken
                        (languageString model.language)
                        model.sessionID
                        model.inputText
                        |> Http.send DialogFlowResponse
                    )
            else
                ( model, Cmd.none )

        ToggleVoiceTyping ->
            let
                toEnable =
                    not model.voiceTypingEnabled

                cmd =
                    if toEnable then
                        startVoiceTyping ()
                    else
                        stopVoiceTyping ()
            in
                ( model, cmd )

        UpdateVoiceTypingStatus enabled ->
            ( { model | voiceTypingEnabled = enabled }, Cmd.none )

        SwitchLanguage lang ->
            ( { model | language = lang }
            , switchLanguage <| languageString lang
            )

        SendResponseAction action value ->
            ( model
            , sendSelection model.accessToken (languageString model.language) model.sessionID ( action, value )
                |> Http.send DialogFlowResponse
            )

        DialogFlowResponse (Err error) ->
            let
                responseText =
                    (i18nText model.language "Unknown error...") ++ (errorHandler error)

                botMessage =
                    ConversationMessage "Bot" (TextMessage responseText)
            in
                ( { model | conversation = model.conversation ++ [ botMessage ] }
                , Task.attempt (always NoOp) <| Dom.Scroll.toBottom "conversationBox"
                )

        DialogFlowResponse (Ok response) ->
            let
                botMessages =
                    fromDialogFlowV2Response response
                        |> List.map (ConversationMessage "Bot")
            in
                ( { model | conversation = model.conversation ++ botMessages }
                , Task.attempt (always NoOp) <| Dom.Scroll.toBottom "conversationBox"
                )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ voiceTyping VoiceTypingInput
        , updateVoiceTypingStatus UpdateVoiceTypingStatus
        ]



-- view


view : Model -> Html Msg
view model =
    div [ class "w-100 w-50-l mw7-l center bg-light-gray pa3 flex flex-column justify-end vh-100" ]
        [ languageBar model.language
        , conversationBox model.language model.conversation
        , inputBox model.hasVoiceTyping model.voiceTypingEnabled model.inputText
        ]


languageBar : Language -> Html Msg
languageBar lang =
    div [ class "bg-black-30 tc" ]
        [ languageButton En (lang == En)
        , languageButton Zh_HK (lang == Zh_HK)
        , languageButton Zh_TW (lang == Zh_TW)
        , languageButton Zh_CN (lang == Zh_CN)
        ]


languageButton : Language -> Bool -> Html Msg
languageButton lang active =
    let
        buttonClass =
            if active then
                "button-reset bn pointer w-20 tc lh-solid br2 ma2 pa2 dib white bg-black"
            else
                "button-reset bn pointer w-20 tc lh-solid br2 ma2 pa2 dib black bg-grey"
    in
        button
            [ class buttonClass
            , onClick <| SwitchLanguage lang
            ]
            [ text <| languageDisplayText lang ]


inputBox : Bool -> Bool -> String -> Html Msg
inputBox hasVoiceTyping voiceTypingEnabled inputText =
    let
        onKeyDown action =
            on "keydown" (Json.Decode.map action keyCode)

        voiceControlStyleClass =
            case ( hasVoiceTyping, voiceTypingEnabled ) of
                ( True, True ) ->
                    "button-reset pa3 tc bn pointer lh-solid w-20 br2 br--left bg-red black"

                ( True, False ) ->
                    "button-reset pa3 tc bn pointer lh-solid w-20 br2 br--left bg-silver black"

                _ ->
                    "button-reset pa3 tc bn pointer lh-solid w-20 br2 br--left bg-near-white gray"

        sendControlStyleClass =
            "button-reset pa3 tc bn pointer lh-solid w-20 br2 br--right bg-black-80 white"

        inputBoxStyleClass =
            "f6 f5-l input-reset bn black-80 bg-white pa3 lh-solid w-60"

        voiceIcon =
            if hasVoiceTyping then
                i [ class "fa fa-microphone" ] []
            else
                i [ class "fa fa-microphone-slash" ] []
    in
        div []
            [ button
                [ class voiceControlStyleClass
                , onClick ToggleVoiceTyping
                , disabled <| not hasVoiceTyping
                ]
                [ voiceIcon ]
            , input
                [ type_ "text"
                , class inputBoxStyleClass
                , onInput TypingInput
                , onKeyDown InputBoxKeyDown
                , value inputText
                ]
                []
            , button
                [ class sendControlStyleClass
                , onClick <| InputBoxKeyDown 13
                ]
                [ i [ class "fa fa-arrow-right" ] [] ]
            ]


conversationBox : Language -> List ConversationMessage -> Html Msg
conversationBox lang messages =
    div
        [ id "conversationBox"
        , class "h-100 overflow-auto"
        ]
    <|
        List.map (conversationMessageItem lang) messages


conversationMessageItem : Language -> ConversationMessage -> Html Msg
conversationMessageItem lang (ConversationMessage user message) =
    let
        messageBox =
            case message of
                TextMessage textMessage ->
                    textMessageBox textMessage

                ListItemMessage title items ->
                    listItemMessageBox lang title items

                CardMessage item ->
                    cardMessageBox item

        boxStyle =
            case user of
                "Me" ->
                    "fr br4 bg-silver pv2 ph3"

                _ ->
                    "fl br4 bg-moon-gray pv2 ph3"
    in
        div [ class "fl w-100 db mv2" ]
            [ div [ class boxStyle ] [ messageBox ]
            ]


textMessageBox : String -> Html Msg
textMessageBox textMessage =
    div [] [ text textMessage ]


listItemMessageBox : Language -> String -> List ListItem -> Html Msg
listItemMessageBox lang title items =
    div [] <|
        div [] [ text title ]
            :: List.map (listItem lang) items


listItem : Language -> ListItem -> Html Msg
listItem lang item =
    let
        thumbnail =
            case item.thumbnail of
                Just data ->
                    img
                        [ src data
                        , class "ba b--black-10 db br2 w2 w3-ns h2 h3-ns"
                        ]
                        []

                Nothing ->
                    div [ class "tc" ] [ i [ class "fa fa-cutlery fa-4x" ] [] ]
    in
        div [ class "dt w-100 bb b--black-05 pb2 mt2" ]
            [ div [ class "dtc w2 w3-ns v-mid" ] [ thumbnail ]
            , div [ class "dtc v-top pl3" ]
                [ div [ class "db" ] [ text item.description ]
                , button
                    [ onClick <| SendResponseAction item.responseAction item.responseValue
                    , class inlineButtonStyle
                    ]
                    [ text <| i18nText lang "more" ]
                ]
            ]


cardMessageBox : CardMessageItem -> Html Msg
cardMessageBox item =
    let
        thumbnail =
            case item.thumbnail of
                Just data ->
                    img
                        [ src data
                        , class "ba b--black-10 db br2 w2 w3-ns h2 h3-ns"
                        ]
                        []

                Nothing ->
                    div [] []
    in
        div [] <|
            [ div [ class "fr" ] [ thumbnail ]
            , h4 [] [ text item.title ]
            , div [] [ text item.text ]
            , div [ class "pt2" ] <| List.map cardMessageButton item.buttons
            ]


cardMessageButton : ButtonItem -> Html Msg
cardMessageButton item =
    button
        [ onClick <| SendResponseAction item.responseAction item.responseValue
        , class inlineButtonStyle
        ]
        [ text item.text ]


inlineButtonStyle : String
inlineButtonStyle =
    "f6 dib button-reset ma2 pa2 tc bn pointer br2 bg-light-gray"



-- requests


sendMessage : String -> String -> String -> String -> Http.Request DialogFlowV2Response
sendMessage accessToken lang sessionID message =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
        , url = "https://dialogflow.googleapis.com/v2beta1/projects/restaurantdemo-8f0a0/agent/sessions/" ++ sessionID ++ ":detectIntent"
        , body =
            Http.stringBody "application/json; charset=utf-8" <|
                Json.Encode.encode 0 <|
                    Json.Encode.object
                        [ ( "queryInput"
                          , Json.Encode.object
                                [ ( "text"
                                  , Json.Encode.object
                                        [ ( "text", Json.Encode.string message )
                                        , ( "languageCode", Json.Encode.string lang )
                                        ]
                                  )
                                ]
                          )
                        , ( "session", Json.Encode.string sessionID )
                        ]
        , expect = Http.expectJson dialogFlowV2ResponseDecoder
        , timeout = Nothing
        , withCredentials = False
        }


sendSelection : String -> String -> String -> ( String, String ) -> Http.Request DialogFlowV2Response
sendSelection accessToken lang sessionID ( responseEvent, responseValue ) =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
        , url = "https://dialogflow.googleapis.com/v2beta1/projects/restaurantdemo-8f0a0/agent/sessions/" ++ sessionID ++ ":detectIntent"
        , body =
            Http.stringBody "application/json; charset=utf-8" <|
                Json.Encode.encode 0 <|
                    Json.Encode.object
                        [ ( "queryInput"
                          , Json.Encode.object
                                [ ( "event"
                                  , Json.Encode.object
                                        [ ( "name", Json.Encode.string responseEvent )
                                        , ( "parameters"
                                          , Json.Encode.object
                                                [ ( "id"
                                                  , Json.Encode.string responseValue
                                                  )
                                                ]
                                          )
                                        , ( "language_code", Json.Encode.string lang )
                                        ]
                                  )
                                ]
                          )
                        , ( "session", Json.Encode.string sessionID )
                        ]
        , expect = Http.expectJson dialogFlowV2ResponseDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- error handling


errorHandler : Http.Error -> String
errorHandler error =
    case error of
        Http.BadUrl url ->
            "Bad URL"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus res ->
            "Bad Status: " ++ ((toString res.status.code) ++ "::" ++ res.status.message)

        Http.BadPayload payload res ->
            "Bad Data: " ++ payload

        Http.Timeout ->
            "Timeout"



-- main


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
