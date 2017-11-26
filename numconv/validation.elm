module Validation exposing (..)

import Regex

decimal : String -> Bool
decimal str = 
    Regex.contains (Regex.regex "^[0-9]*$") str

binary : String -> Bool
binary str = True
    --Regex.contains (Regex.regex "^[0-1 ]*$") str