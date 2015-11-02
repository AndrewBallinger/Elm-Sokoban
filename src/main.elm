module Main where
import Char exposing (KeyCode)
import Color exposing (..)
import Graphics.Collage as C
import Graphics.Element exposing (show)
import Html exposing (div, button, text, fromElement)
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
                      
tileSize : number
tileSize = 10

gridSize : number
gridSize = 10

arenaSize : number
arenaSize = tileSize * gridSize
           
origin : { x : Int, y : Int }
origin = { x = 0, y = 0 }
(|+|) : Coord -> Coord -> Coord
(|+|) a b = { x = a.x + b.x, y = a.y + b.y }

(|-|) : Coord -> Coord -> Coord
(|-|) a b = { x = a.x - b.x, y = a.y - b.y }
            
renderArena : Arena -> C.Form
renderArena arena = groupMatrix (mapWithLocation placeTile arena.grid)
                    |> C.move (-45,-45) 

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
    Player  -> C.circle (tileSize/3) |> C.filled blue
    Hole    -> C.circle (tileSize/4) |> C.filled black 
    Exit    -> C.square (tileSize/2) |> C.filled green ]
               
position : Signal Coord
position = (S.foldp (|+|) origin (lastA))         

type Update = Reset | NoOp | Move Coord
           
main = let mail = S.mailbox NoOp in
  (view mail.address) <~ (S.foldp movePlayer startingArena (S.merge mail.signal (S.map Move lastA) ))

view : S.Address Update  -> Arena -> Html.Html
view addr arena =
  div [] [
  fromElement (formToElement (renderArena arena)),
       button [onClick addr Reset] [ text "Reset" ]
       ]

playerPosition : Arena -> Coord
playerPosition a = arenaToTileCoords a Player

movePlayer : Update -> Arena -> Arena
movePlayer c a = case c of
                   Move coord ->
                     let current = playerPosition a
                         next = (current |+| coord) in
                     if (getTile next a) == Just Exit
                     then
                       getNextArena a
                     else
                       case canMove coord next a of
                         True -> a |> move current next 
                         False -> a
                   Reset -> resetArena a
                   _ -> a
                            
move : Coord -> Coord -> Arena -> Arena
move current next   =  pushBoulder current next
                    >> setTile Floor current
                    >> setTile Player next

pushBoulder : Coord -> Coord -> Arena -> Arena
pushBoulder current next a = let dir = (next |-| current)
                                 next_next = (next |+| dir) in
                             case (getTile next a) of
                               Just Boulder -> case (getTile next_next a) of
                                 Just Hole -> setTile Floor next_next a
                                 _         -> setTile Boulder next_next a
                               _ -> a
                            
canMove : Coord -> Coord -> Arena -> Bool
canMove direction next a = case (getTile next a) of
                   Just Wall -> False
                   Just Floor -> True
                   Just Boulder -> canBoulderMove direction (next |+| direction) a
                   Just Exit -> True
                   _ -> False
 
canBoulderMove : Coord -> Coord -> Arena -> Bool
canBoulderMove direction next a = case  (getTile next a) of
                   Just Wall -> False
                   Just Floor -> True
                   Just Boulder -> False
                   Just Hole -> True
                   _ -> False 

getTile : Coord -> Arena -> Maybe Tile
getTile c a = (get (loc c.y c.x) a.grid)
                        
formToElement : C.Form -> Graphics.Element.Element
formToElement f = C.collage arenaSize arenaSize [f]
       
setTile : Tile -> Coord -> Arena -> Arena
setTile t c a = { a | grid <- set (loc c.y c.x) t a.grid }

startingArena : Arena
startingArena = {grid = Matrix.fromList [[Player, Floor, Exit]], level = -1}
                             
getNextArena : Arena -> Arena
getNextArena a = case getLevel (a.level + 1) of
                    Just a -> a
                    Nothing -> {grid = Matrix.fromList [[Player, Floor, Exit]], level = -1}

resetArena : Arena -> Arena
resetArena a = case getLevel (a.level) of
                    Just a -> a
                    Nothing -> {grid = Matrix.fromList [[Player, Floor, Exit]], level = -1}
                               
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
