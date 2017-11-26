-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List
import ParseInt
import String

import Debug

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { content : String,
    binaryContent : String,
    hexContent : String,
    octalContent : String
  }

model : Model
model =
  { content = "",
    binaryContent = "",
    hexContent = "",
    octalContent = ""
  }


-- UPDATE

type Msg
  = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent -> 
      calculateContent model newContent

calculateContent : Model -> String -> Model
calculateContent model newContent = 
  { model |
    binaryContent = formatContent (convertStringNumberToString newContent 2) 4, 
    hexContent = 
      addHeadToHexString (formatContent (convertStringNumberToString newContent 16) 2),
    octalContent = convertStringNumberToString newContent 8
  }

formatContent : String -> Int -> String
formatContent sourceString width = 
  addCharGroupsToString (addPaddingToString sourceString width) width

convertStringNumberToString : String -> Int -> String
convertStringNumberToString stringNumber radix = 
  case (ParseInt.parseIntRadix 10 stringNumber) of
    Ok num -> 
      case (ParseInt.toRadix radix num) of
        Ok bin -> bin
        Err _ -> "error"
    Err _ -> "error"

addPaddingToString : String -> Int -> String
addPaddingToString sourceString width = 
  --todo
  let zeroes = (width - (String.length sourceString) % width) % width
  --todo padLeft 
  in String.repeat zeroes "0" ++ sourceString

listify : Int -> List Int
listify num = 
  --let _ = Debug.log "number" num in
  case num of
    0 -> [ ]
    1 -> [ 0 ]
    _ -> List.append (listify (num - 1)) [ num - 1 ]

addCharGroupsToString : String -> Int -> String
addCharGroupsToString sourceString width =
  --let _ = Debug.log "number" sourceString in
  let nGroups = (String.length sourceString) // width in
  let loopList = listify nGroups in
  --let slices = List.map (\idx -> ParseInt.toRadixUnsafe 10 idx) loopList
  let slices = 
    List.map (\idx -> (String.slice (idx*width) (idx*width+width) sourceString)) loopList
  in
  String.join " " slices

addHeadToHexString : String -> String
addHeadToHexString sourceString = String.append "0x " sourceString


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Number to convert", onInput Change, myStyle ] []
    , div [myStyle] [ text model.binaryContent ]
    , div [myStyle] [ text model.hexContent ]
    , div [myStyle] [ text model.octalContent ]
    ]


myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
