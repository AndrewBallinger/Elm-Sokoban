import Matrix exposing (Matrix, mapWithLocation, toList, Location, col, row, square, set, loc, get, flatten)
import Keyboard exposing (presses, arrows)
import Char exposing (KeyCode)
import Graphics.Element exposing (show)
import Graphics.Collage as C
import Color exposing (..)
import Signal as S

(<~) : (a -> b) -> Signal a -> Signal b
(<~) = S.map
(~>) : Signal a -> (a -> b) -> Signal b
(~>) = flip S.map
       
type alias Arena = Matrix Tile
type alias Coord = { x : Int, y : Int }
type alias Directions = { up : KeyCode, down : KeyCode, left : KeyCode, right : KeyCode }
type Tile = Floor | Wall | Boulder | Player | Hole
          
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
renderArena arena = groupMatrix (mapWithLocation placeTile arena)
                    |> C.move (-45,-45) 

groupMatrix : Matrix C.Form -> C.Form
groupMatrix matrix = (C.group ( List.map C.group (toList matrix)))
                    
placeTile : Location -> Tile -> C.Form
placeTile place tile = (render tile)
                         |> C.moveX (getShift (col place))
                         |> C.moveY (getShift (row place))

arenaToTileCoords : Arena -> Tile -> Coord
arenaToTileCoords a t = let sumList = List.foldl (|+|) { x=0 , y=0 } in
  sumList <| flatten <| mapWithLocation (matchingTileToCoords t) a 
                        
matchingTileToCoords : Tile -> Location -> Tile -> Coord
matchingTileToCoords to_match location to_be_matched =
                       case (to_match == to_be_matched) of
                         True -> {x = col location, y = row location}
                         False -> { x = 0, y = 0 }
                    
getShift : Int -> Float
getShift offset = toFloat (tileSize*offset)
 
render : Tile -> C.Form
render t =
  case t of
    Floor   -> C.square tileSize     |> C.outlined (C.solid black)
    Wall    -> C.square tileSize     |> C.filled black
    Boulder -> C.circle (tileSize/2) |> C.filled brown
    Player  -> C.circle (tileSize/2) |> C.filled blue
    Hole    -> C.circle (tileSize/3) |> C.filled black
               
position : Signal Coord
position = (S.foldp (|+|) origin (lastA))         
         
main = formToElement
       <~ (renderArena
           <~ (S.foldp movePlayer startingArena lastA))

playerPosition : Arena -> Coord
playerPosition a = arenaToTileCoords a Player

movePlayer : Coord -> Arena -> Arena
movePlayer c a = let current = playerPosition a in
                 let next = (current |+| c) in
                 case canMove c next a of
                   True -> a |> move current next 
                   False -> a
                            
move : Coord -> Coord -> Arena -> Arena
move current next a = a
                    |> pushBoulder current next
                    |> setTile Floor current
                    |> setTile Player next

pushBoulder : Coord -> Coord -> Arena -> Arena
pushBoulder current next a = let dir = (next |-| current)
                                 next_next = (next |+| dir) in
                             case (get (loc next.y next.x) a) of
                               Just Boulder -> case (get (loc next_next.y next_next.x) a) of
                                 Just Hole -> setTile Floor next_next a
                                 _         -> setTile Boulder next_next a
                               _ -> a
                            
canMove : Coord -> Coord -> Arena -> Bool
canMove direction next a = case  (get (loc next.y next.x) a) of
                   Just Wall -> False
                   Just Floor -> True
                   Just Boulder -> canBoulderMove direction (next |+| direction) a
                   _ -> False
 
canBoulderMove : Coord -> Coord -> Arena -> Bool
canBoulderMove direction next a = case  (get (loc next.y next.x) a) of
                   Just Wall -> False
                   Just Floor -> True
                   Just Boulder -> False
                   Just Hole -> True
                   _ -> False 
                              
formToElement : C.Form -> Graphics.Element.Element
formToElement f = C.collage arenaSize arenaSize [f]
       
setTile : Tile -> Coord -> Arena -> Arena
setTile t c a = set (loc c.y c.x) t a
       
startingArena : Arena
startingArena = (square gridSize (\_ -> Floor))
                |> setTile Player {x = 0, y = 0}
                |> setTile Wall {x = 1, y = 1} 
                |> setTile Wall {x = 2, y = 1} 
                |> setTile Wall {x = 3, y = 1}
                |> setTile Boulder {x = 1, y = 3}
                |> setTile Boulder {x = 1, y = 2}
                |> setTile Hole {x = 1, y = 4}
                   
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
