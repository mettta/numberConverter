-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import ParseInt
import String

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { content : String,
    binaryContent : String
  }

model : Model
model =
  { content = "",
    binaryContent = ""
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
  case (ParseInt.parseIntRadix 10 newContent) of
    Ok num -> 
      case (ParseInt.toRadix 2 num) of
        Ok bin -> 
          { model |
            binaryContent = bin
          }
        Err _ -> 
          { model |
            binaryContent = "error"
          }
    Err _ -> 
      { model |
        binaryContent = "error"
      }
  

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Number to convert", onInput Change, myStyle ] []
    , div [myStyle] [ text model.binaryContent ]
    ]


myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
