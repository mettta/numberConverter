module Validation exposing (..)

import Regex

decimal : String -> Bool
decimal str =
    Regex.contains (Regex.regex "^[0-9]*$") str

binary : String -> Bool
binary str =
    Regex.contains (Regex.regex "^[0-1 ]*$") str

octal : String -> Bool
octal str =
    Regex.contains (Regex.regex "^[0-7]*$") str

hex : String -> Bool
hex str =
    Regex.contains (Regex.regex "^[0-9a-fA-F ]*$") str