module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput, on, keyCode)
import Http
import Json.Decode
import Json.Encode
import Random


-- data


accessToken : String
accessToken =
    ""


type alias ButtonItem =
    { text : String
    , responseValue : String
    }


type alias RichMessageItem =
    { thumbnailSrc : String
    , description : String
    , responseValue : String
    }


type Message
    = TextMessage String
    | ListButtonMessage (List ButtonItem)
    | ListRichMessage (List RichMessageItem)


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
    }


type alias DialogFlowV1FulfillmentMessage =
    { type_ : Int
    , speech : String
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
    Json.Decode.map3 DialogFlowV1Fulfillment
        (Json.Decode.field "speech" Json.Decode.string)
        (Json.Decode.maybe <| Json.Decode.field "displayText" Json.Decode.string)
        (Json.Decode.field "messages" <|
            Json.Decode.list <|
                Json.Decode.map2 DialogFlowV1FulfillmentMessage
                    (Json.Decode.field "type" Json.Decode.int)
                    (Json.Decode.field "speech" Json.Decode.string)
        )


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
    , errorMessage : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" [] Nothing
    , Random.int 1 1000
        |> Random.map ((++) "SESSION_" << toString)
        |> Random.generate SetSessionID
    )



-- update


type Msg
    = SetSessionID String
    | TypingInput String
    | InputBoxKeyDown Int
    | DialogFlowResponse (Result Http.Error DialogFlowV1Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSessionID sessionID ->
            ( { model | sessionID = sessionID }, Cmd.none )

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

        DialogFlowResponse (Err error) ->
            ( { model | errorMessage = Just (errorHandler error) }, Cmd.none )

        DialogFlowResponse (Ok response) ->
            let
                responseText =
                    case response.result.fulfillment.displayText of
                        Just displayText ->
                            displayText

                        Nothing ->
                            response.result.fulfillment.speech

                botMessage =
                    ConversationMessage "Bot" (TextMessage responseText)
            in
                ( { model
                    | errorMessage = Nothing
                    , conversation = model.conversation ++ [ botMessage ]
                  }
                , Cmd.none
                )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


view : Model -> Html Msg
view model =
    div []
        [ conversationBox model.conversation
        , inputBox model.inputText
        , errorBox model.errorMessage
        ]


errorBox : Maybe String -> Html Msg
errorBox maybeErrMsg =
    case maybeErrMsg of
        Just errorMessage ->
            div [] [ text errorMessage ]

        Nothing ->
            div [] []


inputBox : String -> Html Msg
inputBox inputText =
    let
        onKeyDown action =
            on "keydown" (Json.Decode.map action keyCode)
    in
        div []
            [ input
                [ type_ "text"
                , onInput TypingInput
                , onKeyDown InputBoxKeyDown
                , value inputText
                ]
                []
            , button [] [ text "Voice" ]
            ]


conversationBox : List ConversationMessage -> Html Msg
conversationBox messages =
    div [] <| List.map conversationMessageItem messages


conversationMessageItem : ConversationMessage -> Html Msg
conversationMessageItem (ConversationMessage user message) =
    let
        messageBox =
            case message of
                TextMessage textMessage ->
                    textMessageBox textMessage

                _ ->
                    div [] [ text "Not Supported" ]
    in
        div []
            [ div [] [ text user ]
            , div [] [ messageBox ]
            ]


textMessageBox : String -> Html Msg
textMessageBox textMessage =
    div [] [ text textMessage ]



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



-- error handling


errorHandler : Http.Error -> String
errorHandler error =
    case error of
        Http.BadUrl url ->
            "Bad URL"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus res ->
            "Bad Status " ++ ((toString res.status.code) ++ "::" ++ res.status.message)

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
