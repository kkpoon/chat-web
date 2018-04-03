# chat-web

This is a reference implementation to use DialogFlow API from elm

## Getting Started

To connect the `chat-web` to your DialogFlow app, you have to modify a block of code in `public/index.html` 

```js
var app = Elm.Main.fullscreen({
    accessToken: "your-dialogflow-access-token",
    hasSpeechRecognition: window.hasOwnProperty("webkitSpeechRecognition")
});
```

Change the `your-dialogflow-access-token` to your dialogflow access token.

To fit the program to your DialogFlow fulfillment logic. You may need to modify `src/Main.elm`.

```elm
type alias DialogFlowV1Fulfillment =
    { speech : String
    , displayText : Maybe String
    , messages : List DialogFlowV1FulfillmentMessage
    , data : Maybe DialogFlowV1FulfillmentData
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
```

`DialogFlowV1FulfillmentData` is my custom type to represent data send to `chat-web` client. The data type should match with your implementation in DialogFlow fulfillment code.

For example, I have the following javascript function in firebase to create `DialogFlowV1FulfillmentWebData` data field in the fulfillment object.

```js
function formWebResponse(responseToUser) {
    let res = {
        web: {
            text: responseToUser.displayText
        }
    };
    if (responseToUser.listItems) {
        let items = responseToUser.listItems.map(r => {
            let thumbnail = r.image || null;
            return {
                text: r.name,
                responseValue: r.id,
                responseEvent: responseToUser.listEvent,
                thumbnailSrc: thumbnail
            };
        });
        res.web.attachment = {
            type: "LIST",
            title: responseToUser.listTitle,
            items: items
        };
    } else if (responseToUser.cardTitle) {
        res.web.attachment = {
            type: "CARD",
            title: responseToUser.cardTitle,
            text: responseToUser.cardText,
            thumbnailSrc: responseToUser.cardImage,
            buttons: responseToUser.cardButtons.map(b => {
                return {
                    title: b.title,
                    responseEvent: b.event,
                    responseValue: b.value
                };
            })
        };
    }
    return res;
}
```
