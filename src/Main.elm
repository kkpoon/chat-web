module Main exposing (main)

import Dom.Scroll
import Html exposing (..)
import Html.Attributes exposing (id, type_, value, src, class, style)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Http
import Json.Decode
import Json.Encode
import Random
import Task


-- data


accessToken : String
accessToken =
    ""


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
    , thumbnailSrc : Maybe String
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
                , thumbnailSrc = cardAtt.thumbnailSrc
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


fromDialogFlowV1Response : DialogFlowV1Response -> List Message
fromDialogFlowV1Response response =
    let
        fulfillment =
            response.result.fulfillment

        webdata =
            fulfillment.data
                |> Maybe.andThen (\d -> d.web)
    in
        case webdata of
            Just web ->
                TextMessage web.text
                    :: fromDialogFlowV1FulfillmentWebDataAttachment web.attachment

            Nothing ->
                case fulfillment.displayText of
                    Just message ->
                        [ TextMessage message ]

                    Nothing ->
                        [ TextMessage fulfillment.speech ]


type ConversationMessage
    = ConversationMessage String Message


type alias DialogFlowV1Response =
    { id : String
    , timestamp : String
    , lang : String
    , result : DialogFlowV1Result
    , status : DialogFlowV1Status
    , sessionId : String
    }


type alias DialogFlowV1Result =
    { source : String
    , resolvedQuery : String
    , action : String
    , actionIncomplete : Bool
    , fulfillment : DialogFlowV1Fulfillment
    , score : Float
    }


type alias DialogFlowV1Status =
    { code : Int
    , errorType : String
    , webhookTimedOut : Bool
    }


type alias DialogFlowV1Fulfillment =
    { speech : String
    , displayText : Maybe String
    , messages : List DialogFlowV1FulfillmentMessage
    , data : Maybe DialogFlowV1FulfillmentData
    }


type alias DialogFlowV1FulfillmentMessage =
    { type_ : Int
    , speech : String
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


dialogFlowV1ResponseDecoder : Json.Decode.Decoder DialogFlowV1Response
dialogFlowV1ResponseDecoder =
    Json.Decode.map6 DialogFlowV1Response
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "timestamp" Json.Decode.string)
        (Json.Decode.field "lang" Json.Decode.string)
        (Json.Decode.field "result" dialogFlowV1ResultDecoder)
        (Json.Decode.field "status" dialogFlowV1StatusDecoder)
        (Json.Decode.field "sessionId" Json.Decode.string)


dialogFlowV1ResultDecoder : Json.Decode.Decoder DialogFlowV1Result
dialogFlowV1ResultDecoder =
    Json.Decode.map6 DialogFlowV1Result
        (Json.Decode.field "source" Json.Decode.string)
        (Json.Decode.field "resolvedQuery" Json.Decode.string)
        (Json.Decode.field "action" Json.Decode.string)
        (Json.Decode.field "actionIncomplete" Json.Decode.bool)
        (Json.Decode.field "fulfillment" dialogFlowV1FulfillmentDecoder)
        (Json.Decode.field "score" Json.Decode.float)


dialogFlowV1FulfillmentDecoder : Json.Decode.Decoder DialogFlowV1Fulfillment
dialogFlowV1FulfillmentDecoder =
    Json.Decode.map4 DialogFlowV1Fulfillment
        (Json.Decode.field "speech" Json.Decode.string)
        (Json.Decode.maybe <| Json.Decode.field "displayText" Json.Decode.string)
        (Json.Decode.field "messages" <|
            Json.Decode.list <|
                Json.Decode.map2 DialogFlowV1FulfillmentMessage
                    (Json.Decode.field "type" Json.Decode.int)
                    (Json.Decode.field "speech" Json.Decode.string)
        )
        (Json.Decode.maybe <| Json.Decode.field "data" dialogFlowV1FulfillmentDataDecoder)


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


dialogFlowV1StatusDecoder : Json.Decode.Decoder DialogFlowV1Status
dialogFlowV1StatusDecoder =
    Json.Decode.map3 DialogFlowV1Status
        (Json.Decode.field "code" Json.Decode.int)
        (Json.Decode.field "errorType" Json.Decode.string)
        (Json.Decode.field "webhookTimedOut" Json.Decode.bool)



-- model


type alias Model =
    { sessionID : String
    , inputText : String
    , conversation : List ConversationMessage
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" []
    , Random.int 1 1000
        |> Random.map ((++) "SESSION_" << toString)
        |> Random.generate SetSessionID
    )



-- update


type Msg
    = SetSessionID String
    | NoOp
    | TypingInput String
    | InputBoxKeyDown Int
    | SendResponseAction String String
    | DialogFlowResponse (Result Http.Error DialogFlowV1Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSessionID sessionID ->
            ( { model | sessionID = sessionID }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        TypingInput inputText ->
            ( { model | inputText = inputText }, Cmd.none )

        InputBoxKeyDown code ->
            -- when user hit enter
            if code == 13 then
                let
                    userMessage =
                        ConversationMessage "Me" (TextMessage model.inputText)
                in
                    ( { model
                        | inputText = ""
                        , conversation = model.conversation ++ [ userMessage ]
                      }
                    , sendMessage accessToken "zh-HK" model.sessionID model.inputText
                        |> Http.send DialogFlowResponse
                    )
            else
                ( model, Cmd.none )

        SendResponseAction action value ->
            ( model
            , sendSelection accessToken "zh-HK" model.sessionID ( action, value )
                |> Http.send DialogFlowResponse
            )

        DialogFlowResponse (Err error) ->
            let
                responseText =
                    "好似有D狀況出現左，我都唔知咩野事..." ++ (errorHandler error)

                botMessage =
                    ConversationMessage "Bot" (TextMessage responseText)
            in
                ( { model | conversation = model.conversation ++ [ botMessage ] }
                , Task.attempt (always NoOp) <| Dom.Scroll.toBottom "conversationBox"
                )

        DialogFlowResponse (Ok response) ->
            let
                botMessages =
                    fromDialogFlowV1Response response
                        |> List.map (ConversationMessage "Bot")
            in
                ( { model | conversation = model.conversation ++ botMessages }
                , Task.attempt (always NoOp) <| Dom.Scroll.toBottom "conversationBox"
                )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


view : Model -> Html Msg
view model =
    div [ class "w-50 mw7 center bg-light-gray pa3 flex flex-column justify-end vh-100" ]
        [ conversationBox model.conversation
        , inputBox model.inputText
        ]


inputBox : String -> Html Msg
inputBox inputText =
    let
        onKeyDown action =
            on "keydown" (Json.Decode.map action keyCode)

        voiceControlStyleClass =
            "button-reset pa3 tc bn pointer lh-solid w-20 br2 br--left bg-gray white"

        sendControlStyleClass =
            "button-reset pa3 tc bn pointer lh-solid w-20 br2 br--right bg-black-80 white"

        inputBoxStyleClass =
            "f6 f5-l input-reset bn black-80 bg-white pa3 lh-solid w-60"
    in
        div []
            [ button [ class voiceControlStyleClass ]
                [ i [ class "fa fa-microphone" ] [] ]
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
                [ text "Send" ]
            ]


conversationBox : List ConversationMessage -> Html Msg
conversationBox messages =
    div
        [ id "conversationBox"
        , class "h-100 overflow-auto"
        ]
    <|
        List.map conversationMessageItem messages


conversationMessageItem : ConversationMessage -> Html Msg
conversationMessageItem (ConversationMessage user message) =
    let
        messageBox =
            case message of
                TextMessage textMessage ->
                    textMessageBox textMessage

                ListItemMessage title items ->
                    listItemMessageBox title items

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


listItemMessageBox : String -> List ListItem -> Html Msg
listItemMessageBox title items =
    div [] <|
        div [] [ text title ]
            :: List.map listItem items


listItem : ListItem -> Html Msg
listItem item =
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
                    [ text "更多資料" ]
                ]
            ]


cardMessageBox : CardMessageItem -> Html Msg
cardMessageBox item =
    div [] <|
        [ div [] [ text item.title ]
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


sendMessage : String -> String -> String -> String -> Http.Request DialogFlowV1Response
sendMessage accessToken lang sessionID message =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
        , url = "https://api.dialogflow.com/v1/query?v=20170712"
        , body =
            Http.stringBody "application/json; charset=utf-8" <|
                Json.Encode.encode 0 <|
                    Json.Encode.object
                        [ ( "query", Json.Encode.string message )
                        , ( "lang", Json.Encode.string lang )
                        , ( "sessionId", Json.Encode.string sessionID )
                        ]
        , expect = Http.expectJson dialogFlowV1ResponseDecoder
        , timeout = Nothing
        , withCredentials = False
        }


sendSelection : String -> String -> String -> ( String, String ) -> Http.Request DialogFlowV1Response
sendSelection accessToken lang sessionID ( responseEvent, responseValue ) =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
        , url = "https://api.dialogflow.com/v1/query?v=20170712"
        , body =
            Http.stringBody "application/json; charset=utf-8" <|
                Json.Encode.encode 0 <|
                    Json.Encode.object
                        [ ( "event"
                          , Json.Encode.object
                                [ ( "name", Json.Encode.string responseEvent )
                                , ( "data"
                                  , Json.Encode.object
                                        [ ( "id"
                                          , Json.Encode.string responseValue
                                          )
                                        ]
                                  )
                                ]
                          )
                        , ( "lang", Json.Encode.string lang )
                        , ( "sessionId", Json.Encode.string sessionID )
                        ]
        , expect = Http.expectJson dialogFlowV1ResponseDecoder
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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
