module Main where

import Char exposing (KeyCode)
import Color exposing (..)
import Graphics.Collage as C
import Graphics.Element exposing (show)
import Html exposing (div, button, text, fromElement)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard exposing (presses, arrows)
import Levels exposing (getLevel)
import Matrix exposing (Matrix, mapWithLocation, toList, Location, col, row, square, set, loc, get, flatten)
import Signal as S
import SokobanTypes exposing (Arena, Coord, Tile(..))

(<~) : (a -> b) -> Signal a -> Signal b
(<~) = S.map
(~>) : Signal a -> (a -> b) -> Signal b
(~>) = flip S.map
       
type alias Directions = { up : KeyCode, down : KeyCode, left : KeyCode, right : KeyCode }
type Update =  Skip | Reset | NoOp | Move Coord
                      
tileSize : number
tileSize = 40

gridSize : number
gridSize = 15

arenaSize : number
arenaSize = tileSize * gridSize

prettyButton : List (String, String)
prettyButton = [ ("color", "red"), ("margin-top", "1in")]
           
origin : { x : Int, y : Int }
origin = { x = 0, y = 0 }
         
(|+|) : Coord -> Coord -> Coord
(|+|) a b = { x = a.x + b.x, y = a.y + b.y }

(|-|) : Coord -> Coord -> Coord
(|-|) a b = { x = a.x - b.x, y = a.y - b.y }
            
renderArena : Arena -> C.Form
renderArena arena = groupMatrix (mapWithLocation placeTile arena.grid)
                    |> C.move ((-arenaSize * 0.5 + tileSize / 2),(-arenaSize * 0.5 + tileSize / 2)) 

groupMatrix : Matrix C.Form -> C.Form
groupMatrix matrix = (C.group ( List.map C.group (toList matrix)))
                    
placeTile : Location -> Tile -> C.Form
placeTile place tile = (render tile)
                         |> C.moveX (getShift (col place))
                         |> C.moveY (getShift (row place))

arenaToTileCoords : Arena -> Tile -> Coord
arenaToTileCoords a t = let sumList = List.foldl (|+|) { x=0 , y=0 } in
  sumList <| flatten <| mapWithLocation (matchingTileToCoords t) a.grid
                        
matchingTileToCoords : Tile -> Location -> Tile -> Coord
matchingTileToCoords to_match location to_be_matched =
                       case (to_match == to_be_matched) of
                         True -> {x = col location, y = row location}
                         False -> { x = 0, y = 0 }
                                  
getShift : Int -> Float
getShift offset = toFloat (tileSize*offset)
 
render : Tile -> C.Form
render t =
  C.group [C.square tileSize |> C.outlined (C.solid black) ,
  case t of
    Floor   -> C.square tileSize     |> C.outlined (C.solid black)
    Wall    -> C.square tileSize     |> C.filled black
    Boulder -> C.circle (tileSize/3) |> C.filled brown
    Ball    -> C.circle (tileSize/3) |> C.filled red
    Player  -> C.circle (tileSize/3) |> C.filled blue
    Hole    -> C.circle (tileSize/4) |> C.filled black 
    Exit    -> C.square (tileSize/2) |> C.filled green ]
               
position : Signal Coord
position = (S.foldp (|+|) origin (lastA))         
          
main = let mail = S.mailbox NoOp in
  (view mail.address)
  <~ (S.foldp updateArena startingArena
         (S.merge mail.signal
             (S.map Move lastA) ))

view : S.Address Update -> Arena -> Html.Html
view addr arena =
  div [style [("padding","0.5in")]] [
  fromElement (formToElement (renderArena arena)),
       button [onClick addr Reset, style prettyButton] [ text "Reset" ],
       button [onClick addr Skip] [ text "Skip (Cheatie)"],
       fromElement <| show arena.moveCount
       ]


playerPosition : Arena -> Coord
playerPosition a = arenaToTileCoords a Player

updateArena : Update -> Arena -> Arena
updateArena c a = case c of
                   Move coord ->
                     let current = playerPosition a
                         next = (current |+| coord) in
                     case (getTile next a) of
                       Just Exit -> nextArena a
                       Just Hole -> resetArena a
                       _ ->  case canMove coord next a of
                         True -> a |> move current next 
                         False -> a
                   Reset -> resetArena a
                   Skip -> nextArena a
                   _ -> a
                            
move : Coord -> Coord -> Arena -> Arena
move current next  =   incrementCount
                    >> pushBoulder current next
                    >> setTile Floor current
                    >> setTile Player next

incrementCount : Arena -> Arena
incrementCount a = { a | moveCount = a.moveCount + 1 }
                       
pushBoulder : Coord -> Coord -> Arena -> Arena
pushBoulder current next a = let dir = (next |-| current)
                                 last_next = (lastOpen next dir a)
                                 next_next = (next |+| dir) in
                             case (getTile next a) of
                               Just Boulder -> case (getTile next_next a) of
                                 Just Hole -> setTile Floor next_next a
                                 _         -> setTile Boulder next_next a
                               Just Ball -> case (getTile last_next a) of
                                 Just Hole -> setTile Floor last_next a
                                 _         -> setTile Ball last_next a
                               _ -> a

lastOpen : Coord -> Coord -> Arena -> Coord
lastOpen c d a = case canBoulderMove (c |+| d) a of
                    True -> lastOpen (c |+| d) d a
                    False -> c
                                    
canMove : Coord -> Coord -> Arena -> Bool
canMove direction next a = case (getTile next a) of
                   Just Wall -> False
                   Just Floor -> True
                   Just Boulder -> canBoulderMove (next |+| direction) a
                   Just Ball -> canBoulderMove (next |+| direction) a
                   Just Exit -> True
                   _ -> False
 
canBoulderMove : Coord -> Arena -> Bool
canBoulderMove next a = case  (getTile next a) of
                   Just Wall -> False
                   Just Floor -> True
                   Just Boulder -> False
                   Just Ball -> False
                   Just Hole -> True
                   _ -> False 

getTile : Coord -> Arena -> Maybe Tile
getTile c a = (get (loc c.y c.x) a.grid)
                        
formToElement : C.Form -> Graphics.Element.Element
formToElement f = C.collage arenaSize arenaSize [f]
       
setTile : Tile -> Coord -> Arena -> Arena
setTile t c a = { a | grid = set (loc c.y c.x) t a.grid }

startingArena : Arena
startingArena = {grid = Matrix.fromList [[Player, Floor, Exit]], level = -1, moveCount = 0}
                             
nextArena : Arena -> Arena
nextArena a = case getLevel (a.level + 1) of
                    Just a -> a
                    Nothing -> startingArena

resetArena : Arena -> Arena
resetArena a = case getLevel (a.level) of
                    Just a -> a
                    Nothing -> startingArena
                               
lastA : Signal Coord
lastA = (toXY { up = 119, down = 115, left = 97, right = 100 }) <~ presses --WASD
           
toXY : Directions -> KeyCode -> { x : Int, y : Int }
toXY {up,down,left,right} key =
  let is keyCode =
        if keyCode == key
        then 1
        else 0
  in
    { x =  is right - is left
    , y = is up - is down }
