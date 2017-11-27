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
convertStringNumberToString stringNumber fromradix radix = 
  case (ParseInt.parseIntRadix 10 stringNumber) of
    Ok num -> 
      case (ParseInt.toRadix radix num) of
        Ok bin -> bin
        Err _ -> "error"
    Err _ -> "error"


calculateContent : Model -> String -> Model
calculateContent model newContent = 
  { model |
    decimalContent = newContent,
    binaryContent = Formatting.formatContent (convertStringNumberToString newContent 13 2) 4, 
    hexContent = Formatting.formatContent (convertStringNumberToString newContent 13 16) 2,
    octalContent = convertStringNumberToString newContent 13 8
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
      calculateContent model newContent
    DecimalChange newContent -> 
      if Validation.decimal newContent
      then
        calculateContent model newContent
      else
        model
    BinaryChange newContent -> 
      if Validation.binary newContent
      then
        calculateContent model newContent
      else
        model
    HexChange newContent -> 
      calculateContent model newContent
    OctalChange newContent -> 
      calculateContent model newContent


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
