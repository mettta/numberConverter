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
    octalContent : String,
    hexContent : String
  }

model : Model
model =
  { content = "",
    decimalContent = "",
    binaryContent = "",
    octalContent = "",
    hexContent = ""
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
  | OctalChange String
  | HexChange String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      calculateContent model newContent 10
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
    OctalChange newContent ->
      if Validation.octal newContent
      then
        calculateContent model newContent 8
      else
        model
    HexChange newContent ->
      if Validation.hex newContent
      then
        calculateContent model newContent 16
      else
        model

-- VIEW

view : Model -> Html Msg
view model =
  div [class "numconv"]
    [ p [] [ text "Enter a number to convert:" ]
    , input [ placeholder "Decimal",
              required True,
              onInput DecimalChange,
              value model.decimalContent] []
    , label [] [ text "Decimal" ]
    , input [ placeholder "Binary",
              required True,
              onInput BinaryChange,
              value model.binaryContent] []
    , label [] [ text "Binary" ]
    , input [ placeholder "Octal",
              required True,
              onInput OctalChange,
              value model.octalContent] []
    , label [] [ text "Octal" ]
    , input [ placeholder "Hex",
              required True,
              onInput HexChange,
              value model.hexContent] []
    , label [] [ text "Hex" ]
    --, input [ placeholder "???",
    --      required True,
    --      onInput Change ] []
    --, label [] [ text "not specific" ]
    ]

