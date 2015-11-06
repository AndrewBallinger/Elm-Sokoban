module Levels where
import Array
import Matrix
import String
import Maybe exposing (..)
import SokobanTypes exposing (Arena, Tile(..))

getLevel : Int -> Maybe Arena
getLevel i = let level = Array.get i levels in
             case level of
               Just a -> Just (parseArena i a)
               Nothing -> Nothing

charToTile : Char -> Tile
charToTile c = case c of
                 '.' -> Boulder
                 '#' -> Wall
                 'P' -> Player
                 'O' -> Hole
                 'E' -> Exit
                 'B' -> Ball
                 _   -> Floor
             
parseArena : Int -> List String -> Arena
parseArena i s = { grid = Matrix.fromList ((List.map >> List.map) charToTile (List.reverse (List.map String.toList s))), level = i, moveCount = 0 }

levels : Array.Array (List String)
levels = Array.fromList [
          ["__#",
           "P.E",
           "__#"
          ],
          ["P._OE"],
          [
           "####_###",
           "P_.___.OE",
           "###__###"
          ],
          [
           "#_E#",
           "P..O",
           "._#",
           "_#"
          ],
          [
           "#_#",
           "_.OE",
           "__#",
           "_P#"
          ],
          [
           "# #",
           "P OE",
           "#. #",
           "   #",
           "   #"
          ],
          [
           "  #   ",
           " .    ",
           "  #   ",
           "P #",
           "O#",
           "E"
          ],
          [
           "###    ",
           "     # ",
           " #     ",
           "P B   ",
           "##   . ",
           "##     ",
           "### #",
           "###O",
           "###O",
           "###E"
          ],
          [
            "P      ",
            "   ..  ",
            "  . B  ",
            "O# # ",
            "O# # ",
            "O#   ",
            "O#",
            "E#"
          ],
          [
            "    ",
            " .# O",
            "P. . #  #E",
            " ## O.  #O",
            ".   # . #O",
            " # ##. ##O",
            "  . .  OOO",
            "   #####  "
          ],
          [
           "    OE",
           "    ",
           "    ",
           " B ",
           "   ",
           " P "
          ]
         ]
                              
