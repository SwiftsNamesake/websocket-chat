
------------------------------------------------------------------------------------------------------------------------------------------

module App exposing (main)

------------------------------------------------------------------------------------------------------------------------------------------

import Html exposing (..)
import Html.Attributes exposing (..)
import WebSocket as Websocket
import Html.Events exposing (onClick, onInput)
import FontImport as Font
import FontAwesome.Web as Icon
import Markdown as Markdown
import Debug
import Time exposing (Time)
import Json.Decode as JD
import Json.Encode as JE


-- Types ---------------------------------------------------------------------------------------------------------------------------------

-- | Define the application state
type Message =   UserJoins Person
               | UserLeaves Person
               | AcceptMessage ChatMessage
               | SendCurrentDraft
               | EditDraft String
               | BotchedMessage String


-- |
type alias SocketMessage = {
  kind : String,
  payload: JD.Value
}


-- | A url is just a string, for now
type alias Url = String


-- |
type alias UUID = Int


-- |
type alias Notification = String


-- | This isn't strictly necessary
type Personality = Smartarse | Wanker | Knowitall | Darling | Toff | Frump


-- | We should eventually make this secure and encrypted and all that faff
type alias Person = {
  uuid        : UUID,
  username    : String,
  personality : Personality,
  appearance  : Url
}


-- The bread and butter of every chat client
type alias ChatMessage = {
  sender    : UUID,
  payload   : String,
  timestamp : Time
}


-- |
type alias Model = {
  user          : Person,
  notifications : List String,
  history       : List ChatMessage,
  chatEndpoint  : Url,
  roster        : List Person,
  draft         : Maybe String
}

-- Lenses --------------------------------------------------------------------------------------------------------------------------------

type Lens s a = Lens { get : s -> a, set : s -> a -> s }


-- |
-- TODO | - Rename
revise : Lens s a -> (a -> a) -> s -> s
revise (Lens lens) f s = lens.set s (f (lens.get s))

notifications : Lens Model (List Notification)
notifications = Lens { get = .notifications, set = \s new -> {s|notifications=new} }

history : Lens Model (List ChatMessage)
history = Lens { get = .history, set = \s new -> {s|history=new} }

roster : Lens Model (List Person)
roster = Lens { get = .roster, set = \s new -> {s|roster=new} }


user : Lens Model Person
user = Lens { get = .user, set = \s new -> {s|user=new} }

------------------------------------------------------------------------------------------------------------------------------------------

-- |
apply : JD.Decoder (a -> b) -> JD.Decoder a -> JD.Decoder b
apply f decode = JD.andThen (\g -> JD.map g decode) f


-- |
(|:) : JD.Decoder (a -> b) -> JD.Decoder a -> JD.Decoder b
(|:) = apply


-- |
messageAsJson : ChatMessage -> String
messageAsJson msg = JE.encode 0 (encodeSocketMessage "chat-message" (encodeMessage msg))


-- |
encodeSocketMessage : String -> JE.Value -> JE.Value
encodeSocketMessage kind payload = JE.object [("kind", JE.string kind), ("payload", payload)]


-- | Convert ChatMessage to JSON string
encodeMessage : ChatMessage -> JE.Value
encodeMessage msg = JE.object [
                      ("sender",    JE.int msg.sender),
                      ("timestamp", JE.float msg.timestamp),
                      ("payload",   JE.string msg.payload)]


-- | Convert Person to JSON string
encodePerson : Person -> JE.Value
encodePerson person = JE.object [
                        ("username",    JE.string person.username),
                        ("appearance",  JE.string person.appearance),
                        ("personality", JE.string (toString person.personality))]


-- |
decodePerson : JD.Decoder Person
decodePerson = JD.succeed Person
                 |: (JD.field "uuid"        JD.int)
                 |: (JD.field "username"    JD.string)
                 |: (JD.field "personality" decodePersonality)
                 |: (JD.field "appearance"  JD.string)


-- |
--  (JD.string s) |: (JD.succeed l)
decodeLiteral : String -> a -> JD.Decoder a
decodeLiteral s l = JD.andThen (\v -> if s == v then JD.succeed l else JD.fail "[wrong literal]") JD.string


-- |
decodePersonality : JD.Decoder Personality
decodePersonality = JD.oneOf [
                      decodeLiteral "Smartarse" Smartarse,
                      decodeLiteral "Wanker"    Wanker,
                      decodeLiteral "Knowitall" Knowitall,
                      decodeLiteral "Darling"   Darling,
                      decodeLiteral "Toff"      Toff,
                      decodeLiteral "Frump"     Frump,
                      JD.fail "Failed to decode Personality"]


-- |
decodeChatMessage : JD.Decoder ChatMessage
decodeChatMessage = JD.succeed ChatMessage
                      |: (JD.field "sender"    JD.int)
                      |: (JD.field "payload"   JD.string)
                      |: (JD.field "timestamp" JD.float)


-- |
decodeSocketMessage : JD.Decoder Message
decodeSocketMessage = JD.andThen (\kind -> JD.field "payload" (chooseMessageFromKind kind)) (JD.field "kind" JD.string)


-- |
chooseMessageFromKind : String -> JD.Decoder Message
chooseMessageFromKind kind = case kind of
  "chat-message" -> JD.succeed AcceptMessage |: decodeChatMessage
  "user-joins"   -> JD.succeed UserJoins     |: decodePerson
  "user-leaves"  -> JD.succeed UserLeaves    |: decodePerson
  _              -> JD.fail "Unknown message kind"

------------------------------------------------------------------------------------------------------------------------------------------

-- | What everyone looks like
defaultAppearance = "http://img03.deviantart.net/94d7/i/2011/159/d/b/gandalf_the_grey_by_cblair-d2qi97i.jpg"
-- defaultAppearance = "http://img07.deviantart.net/72af/i/2011/345/f/0/i_geek_weekly__gandalf_the_grey_by_joshuafitzpatrick-d4iufqz.png"


-- | Humble beginnings
initialModel : Model
initialModel =
  let user = { uuid        = 0, -- TODO: Fix this
               username    = "SwiftsNamesake",
               personality = Smartarse,
               appearance  = defaultAppearance }
  in {
    user          = user,
    notifications = [],
    roster        = [],
    history       = List.map (\msg -> { sender = user.uuid, payload = msg, timestamp = 0 }) ["There is something odd at play", "Wanna hang out?", "Who reads the newspaper anyways"],
    draft         = Nothing,
    chatEndpoint  = "ws://localhost:8001" }


-- |
initialState : (Model, Cmd Message)
initialState = (initialModel, initialHandshake initialModel)


-- |
initialHandshake : Model -> Cmd Message
initialHandshake model = Websocket.send model.chatEndpoint (JE.encode 0 (encodeSocketMessage "user-joins" (encodePerson model.user)))

------------------------------------------------------------------------------------------------------------------------------------------

-- |
update : Message -> Model -> (Model, Cmd Message)
update action model = case action of
  (UserJoins user)    -> (revise roster ((::) user) model, Cmd.none)
  (UserLeaves user)   -> (revise roster (List.filter (\other -> other.username /= user.username)) model, Cmd.none)
  (AcceptMessage msg) -> (revise history ((::) msg) model, Cmd.none)
  (BotchedMessage s)  -> (revise notifications ((::) ("Failed to parse message: " ++ s)) model, Cmd.none)
  (SendCurrentDraft)  -> trySendDraft model
  (EditDraft s)   -> ({model|draft = Just s}, Cmd.none)


-- |
trySendDraft : Model -> (Model, Cmd Message)
trySendDraft model = case model.draft of
  (Just draft) -> let msg = createMessage model draft
                      new = revise history ((::) msg) model
                  in ({new|draft=Nothing}, encodeAndSend model msg)
  Nothing      -> (revise notifications ((::) "Nothing to send") model, Cmd.none)


-- | Encodes a ChatMessage as JSON and broadcasts it to through a websocket
encodeAndSend : Model -> ChatMessage -> Cmd Message
encodeAndSend model msg = Websocket.send model.chatEndpoint (messageAsJson msg)


-- |
createMessage : Model -> String -> ChatMessage
createMessage model msg = {
  sender = model.user.uuid,
  payload = msg,
  timestamp = 0 }


-- |
subscriptions : Model -> Sub Message
subscriptions model = Sub.batch [
                        Websocket.listen model.chatEndpoint acceptSocketMessage]


-- |
acceptSocketMessage : String -> Message
acceptSocketMessage str = case JD.decodeString decodeSocketMessage str of
  Ok msg  -> msg
  Err err -> BotchedMessage err

------------------------------------------------------------------------------------------------------------------------------------------

-- Define the View

-- |
mainStyle : List (String, String)
mainStyle = [("fontFamily", "Source Sans Pro")]


-- |
stylesheet : String -> Html a
stylesheet href_ = node "link" [ rel "stylesheet", href href_] []


-- |
view : Model -> Html Message
view model = div []
               [stylesheet "css/app.css",
                stylesheet "http://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.3.0/css/font-awesome.css",
                div [class "page"] [chat model]]


-- |
findUserByUUID : Model -> UUID -> Maybe Person
findUserByUUID model uuid = List.head <| List.filter (\user -> user.uuid == uuid) (model.user :: model.roster)


-- |
showMessage : Model -> ChatMessage -> Html Message
showMessage model msg =
  let anon = "http://orig08.deviantart.net/97d9/f/2014/261/3/3/anon_mask_wallpaper_v1_0_by_techdrawer-d7zo4ba.png"
      pic  = Maybe.withDefault (anon) (Maybe.map (.appearance) (findUserByUUID model msg.sender))
  in li [class "message"]
       [img [src pic, class "appearance"] [],
        div [class "message-content"] [text msg.payload]]


-- |
showNotification : Notification -> Html Message
showNotification n = div [class "notification"] [span [] [Icon.bell_o], span [] [text (toString n)]]


-- |
showPerson : Person -> Html Message
showPerson person = div [class "person"] [img [src person.appearance, class "appearance", alt person.username] []]


-- div [class "notifications"] (List.map (showNotification) (List.take 5 model.notifications)),
--  div [class "roster"]        [ul [] (List.map showPerson model.roster)],
-- |
chat : Model -> Html Message
chat model = div [class "chat-client", style mainStyle]
               [div [class "read-area"]
                  [ul [class "message-list"] (List.map (showMessage model) (List.reverse model.history))],
                div [class "write-area"]
                  [input  [class "draft", placeholder "Write here...", autofocus True, onInput EditDraft, value (Maybe.withDefault "" model.draft)] [],
                   button [class "send-button", onClick SendCurrentDraft, class "send-button"] [div [] [Icon.paper_plane_o]]]]


-- | The entry point to our application
main : Program Never Model Message
main = Html.program {
  init          = initialState,
  update        = update,
  subscriptions = subscriptions,
  view          = view }
