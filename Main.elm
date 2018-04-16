import Html exposing (..)
import Char exposing (fromCode)
import WebSocket
import Result exposing (withDefault)
import Json.Decode exposing (..)
import Keyboard exposing (downs, KeyCode)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

server : String
server = "ws://127.0.0.1:3000/ws"

-- MODEL
type alias Model = { 
    word : String,
    clientName: String
  }

init : (Model, Cmd Msg)
init =
  (Model "" "", Cmd.none)

-- UPDATE
type Msg
  = NewMessage String
  | KeyPressed Keyboard.KeyCode

-- filterChars : Keyboard.KeyCode -> Int
-- filterChars c = case c of
--   65...90 -> c

update : Msg -> Model -> (Model, Cmd Msg)
update msg {word, clientName} =
  case msg of
    KeyPressed key ->
      (Model word clientName, WebSocket.send server ("{\"Author\": \"Chris\", \"Body\": \"" ++ (toString (fromCode (key))) ++ "\"}"))

    NewMessage str ->
      (Model (getValue "Body" str) (getValue "Author" str) , Cmd.none)

getValue : String -> String -> String
getValue v str = 
  case decodeString (field v string) str of
    Err msg -> ""
    Ok value -> value

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch[ Keyboard.downs KeyPressed, WebSocket.listen server NewMessage ]

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ span [] []
    , text (toString model.clientName)
    , br [] []
    , span [] []
    , text (toString model.word)
    ]