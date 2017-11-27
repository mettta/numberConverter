module Formatting exposing (..)

-- Adds zeroes at start of string for future formatting ( like 0010 )
addPaddingToString : String -> Int -> String
addPaddingToString sourceString width = 
  --todo
  let zeroes = (width - (String.length sourceString) % width) % width
  --todo padLeft 
  in String.repeat zeroes "0" ++ sourceString

-- Creates list of indexes for numbers groups ( like [0,1,2,3] )
listify : Int -> List Int
listify num = 
  --let _ = Debug.log "number" num in
  case num of
    0 -> [ ]
    1 -> [ 0 ]
    _ -> List.append (listify (num - 1)) [ num - 1 ]

-- Represents a string as symbols groups separated by spaces ( like 0000 0000 0000 )
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

-- It finally formats the string according to the typical "width" of the symbols group.
formatContent : String -> Int -> String
formatContent sourceString width = 
  addCharGroupsToString (addPaddingToString sourceString width) width