module SokobanTypes where
import Matrix exposing (Matrix)

type alias Arena = { grid: Matrix Tile, level: Int }
type alias Coord = { x: Int, y: Int }
                 
type Tile = Floor | Wall | Boulder | Player | Hole | Exit | Ball
