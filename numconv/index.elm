-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List
import ParseInt
import String

import Debug

import Validation
import Formatting

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { content : String,
    decimalContent : String,
    binaryContent : String,
    hexContent : String,
    octalContent : String
  }

model : Model
model =
  { content = "",
    decimalContent = "",
    binaryContent = "",
    hexContent = "",
    octalContent = ""
  }


-- Functions for UPDATE

convertStringNumberToString : String -> Int -> Int -> String
convertStringNumberToString stringNumber fromradix toradix =
  case (ParseInt.parseIntRadix fromradix (Formatting.clearF stringNumber)) of
    Ok num ->
      case (ParseInt.toRadix toradix num) of
        Ok bin -> bin
        Err _ -> "error"
    Err _ -> "error"


calculateContent : Model -> String -> Int -> Model
calculateContent model newContent fromradix =
  { model |
    decimalContent =
      convertStringNumberToString newContent fromradix 10,
    binaryContent =
      Formatting.formatContent (convertStringNumberToString newContent fromradix 2) 4,
    hexContent =
      Formatting.formatContent (convertStringNumberToString newContent fromradix 16) 2,
    octalContent =
      convertStringNumberToString newContent fromradix 8
  }


-- UPDATE

type Msg
  = Change String
  | DecimalChange String
  | BinaryChange String
  | HexChange String
  | OctalChange String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      calculateContent model newContent 13
    DecimalChange newContent ->
      if Validation.decimal newContent
      then
        calculateContent model newContent 10
      else
        model
    BinaryChange newContent ->
      if Validation.binary newContent
      then
        calculateContent model newContent 2
      else
        model
    HexChange newContent ->
      calculateContent model newContent 16
    OctalChange newContent ->
      calculateContent model newContent 8


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "???", onInput Change, myStyle ] []
    , input [ placeholder "Decimal", onInput DecimalChange, value model.decimalContent, myStyle ] []
    , input [ placeholder "Binary", onInput BinaryChange, value model.binaryContent, myStyle] []
    , input [ placeholder "Hex", onInput HexChange, value model.hexContent, myStyle] []
    , input [ placeholder "Octal", onInput OctalChange, value model.octalContent, myStyle] []
    ]


myStyle =
  style
    [ ("display", "block")
    , ("width", "80%")
    , ("max-width", "800px")
    , ("margin", "10px auto")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
