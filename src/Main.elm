module Main exposing (main)

import Dom.Scroll
import Html exposing (..)
import Html.Attributes exposing (id, type_, value, src, class)
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
    , responseValue : String
    }


type alias ListItem =
    { thumbnail : Maybe String
    , description : String
    , responseAction : String
    , responseValue : String
    }


type Message
    = TextMessage String
    | ListItemMessage String (List ListItem)


fromDialogFlowV1Response : DialogFlowV1Response -> List Message
fromDialogFlowV1Response response =
    let
        fulfillment =
            response.result.fulfillment

        attachment =
            fulfillment.data
                |> Maybe.andThen (\d -> d.web)
    in
        case attachment of
            Just web ->
                [ TextMessage web.text
                , ListItemMessage web.attachment.title <|
                    List.map
                        (\item ->
                            { thumbnail = item.thumbnailSrc
                            , description = item.text
                            , responseAction = item.responseEvent
                            , responseValue = item.responseValue
                            }
                        )
                        web.attachment.items
                ]

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


type alias DialogFlowV1FulfillmentWebDataAttachment =
    { type_ : String
    , title : String
    , items : List DialogFlowV1FulfillmentWebDataAttachmentItem
    }


type alias DialogFlowV1FulfillmentWebDataAttachmentItem =
    { text : String
    , responseEvent : String
    , responseValue : String
    , thumbnailSrc : Maybe String
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
        (Json.Decode.maybe <|
            Json.Decode.field "data" <|
                Json.Decode.map DialogFlowV1FulfillmentData <|
                    Json.Decode.maybe
                        (Json.Decode.field "web" dialogFlowV1FulfillmentWebDataDecoder)
        )


dialogFlowV1FulfillmentWebDataDecoder : Json.Decode.Decoder DialogFlowV1FulfillmentWebData
dialogFlowV1FulfillmentWebDataDecoder =
    Json.Decode.map2 DialogFlowV1FulfillmentWebData
        (Json.Decode.field "text" Json.Decode.string)
        (Json.Decode.field "attachment" dialogFlowV1FulfillmentWebDataAttachmentDecoder)


dialogFlowV1FulfillmentWebDataAttachmentDecoder : Json.Decode.Decoder DialogFlowV1FulfillmentWebDataAttachment
dialogFlowV1FulfillmentWebDataAttachmentDecoder =
    Json.Decode.map3 DialogFlowV1FulfillmentWebDataAttachment
        (Json.Decode.field "type" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "items" <|
            Json.Decode.list dialogFlowV1FulfillmentWebDataAttachmentItemDecoder
        )


dialogFlowV1FulfillmentWebDataAttachmentItemDecoder : Json.Decode.Decoder DialogFlowV1FulfillmentWebDataAttachmentItem
dialogFlowV1FulfillmentWebDataAttachmentItemDecoder =
    Json.Decode.map4 DialogFlowV1FulfillmentWebDataAttachmentItem
        (Json.Decode.field "text" Json.Decode.string)
        (Json.Decode.field "responseEvent" Json.Decode.string)
        (Json.Decode.field "responseValue" Json.Decode.string)
        (Json.Decode.maybe <| Json.Decode.field "thumbnailSrc" Json.Decode.string)


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
    | SelectListItem String String
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

        SelectListItem action value ->
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
    div [ class "w-50 mw7 center bg-gray pa3 flex flex-column justify-end vh-100" ]
        [ conversationBox model.conversation
        , inputBox model.inputText
        ]


inputBox : String -> Html Msg
inputBox inputText =
    let
        onKeyDown action =
            on "keydown" (Json.Decode.map action keyCode)

        voiceControlStyleClass =
            "button-reset pa3 tc bn pointer lh-solid w-20 br2 br--left"

        sendControlStyleClass =
            "button-reset pa3 tc bn pointer lh-solid w-20 br2 br--right bg-black-80 white"

        inputBoxStyleClass =
            "f6 f5-l input-reset bn black-80 bg-white pa3 lh-solid w-60"
    in
        div []
            [ button [ class voiceControlStyleClass ] [ text "Voice" ]
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
    in
        div []
            [ div [] [ text user ]
            , div [] [ messageBox ]
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
    div []
        [ img [ src <| Maybe.withDefault "" item.thumbnail ] []
        , div [] [ text item.description ]
        , button
            [ onClick <| SelectListItem item.responseAction item.responseValue
            ]
            [ text "更多資料" ]
        ]



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
