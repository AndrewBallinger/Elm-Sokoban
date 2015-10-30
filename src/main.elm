import Matrix exposing (Matrix, mapWithLocation, toList, Location, col, row, square, set, loc)
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
type Tile = Floor | Wall | Boulder | Player
          
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

getShift : Int -> Float
getShift offset = toFloat (tileSize*offset)
 
render : Tile -> C.Form
render t =
  case t of
    Floor   -> C.square tileSize     |> C.outlined (C.solid black)
    Wall    -> C.square tileSize     |> C.filled black
    Boulder -> C.circle tileSize     |> C.filled brown
    Player  -> C.circle (tileSize/2) |> C.filled blue
               
               
position : Signal Coord
position = (S.foldp (|+|) origin (lastA))         

playerPosition : Arena -> Coord
playerPosition a = { x = 1, y = 1 }
           
main : Signal Graphics.Element.Element
main = formToElement
       <~ (renderArena
           <~ ( (\p -> setTile Player p startingArena)
               <~ position))

formToElement : C.Form -> Graphics.Element.Element
formToElement f = C.collage arenaSize arenaSize [f]
       
setTile : Tile -> Coord -> Arena -> Arena
setTile t c a = set (loc c.y c.x) t a
       
startingArena : Arena
startingArena = (square gridSize (\_ -> Floor))
                |> setTile Player {x = 1, y = 1} 
                |> setTile Wall {x = 1, y = 1} 
                |> setTile Wall {x = 2, y = 1} 
                |> setTile Wall {x = 3, y = 1} 
                   
                   
lastA : Signal { x : Int, y : Int }
lastA = (toXY { up = 38, down = 40, left = 37, right = 39 }) <~ presses
           
toXY : Directions -> KeyCode -> { x : Int, y : Int }
toXY {up,down,left,right} key =
  let is keyCode =
        if keyCode == key
        then 1
        else 0
  in
    { x = is right - is left
    , y = is up - is down }
 
dropMap : (a -> b) -> Signal a -> Signal b
dropMap f signal =
  Signal.dropRepeats (Signal.map f signal)
