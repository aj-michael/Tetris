import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import List
import Random (..)
import Set
import Signal (..)
import Time (..)
import Window


type Input = Move {x:Int, y:Int} | Timer Float


type alias Piece =
  { color:Color
  , blocks:List (Int, Int)
  , offsetX:Int
  , offsetY:Int
  }


blockSize = 20
floor = -10
dropHeight = 10
maxX = 5
minX = -5


piece1 : Piece
piece1 =
  { color=yellow
  , blocks=
    [ (0, 0)
    , (0, 1)
    , (1, 0)
    , (1, 1)
    ]
  , offsetX=0
  , offsetY=0
  }


piece2 : Piece
piece2 =
  { color=blue
  , blocks=
    [ (0, 0)
    , (0, 1)
    , (0, 2)
    , (0, 3)
    ]
  , offsetX=0
  , offsetY=0
  }


piece3 : Piece
piece3 =
  { color=red
  , blocks=
    [ (-1, 0)
    , (0, 0)
    , (0, 1)
    , (1, 0)
    ]
  , offsetX=0
  , offsetY=0
  }


piece4 : Piece
piece4 =
  { color=orange
  , blocks=
    [ (-1, 1)
    , (0, 0)
    , (0, 1)
    , (1, 0)
    ]
  , offsetX=0
  , offsetY=0
  }


piece5 : Piece
piece5 =
  { color=blue
  , blocks=
    [ (-1, 0)
    , (0, 0)
    , (0, 1)
    , (1, 1)
    ]
  , offsetX=0
  , offsetY=0
  }


indexedPiece : Int -> Piece
indexedPiece n = case n of
  1 -> piece1
  2 -> piece2
  3 -> piece3
  4 -> piece4
  5 -> piece5


update : Input -> (Piece, List Piece, Seed) -> (Piece, List Piece, Seed)
update s (piece, pieces, seed) = case s of
  Move arrows ->
    if arrows.y == 0
    then ({piece | offsetX <- piece.offsetX+arrows.x}, pieces, seed)
    else (piece, pieces, seed)
  Timer _ ->
    if finished piece pieces
    then
      let (piece',seed') = getPiece seed
      in (piece', piece :: pieces, seed')
    else ({piece | offsetY <- piece.offsetY - 1}, pieces, seed)


finished : Piece -> List Piece -> Bool
finished piece pieces = lowest piece == floor || List.any (touching piece) pieces


getPiece : Seed -> (Piece, Seed)
getPiece seed =
  let (index, seed') = generate (int 1 4) seed
      piece = indexedPiece index
  in ({piece | offsetY <- dropHeight}, seed')


touching : Piece -> Piece -> Bool
touching top bottom =
    let translated = coordinates top
        |> Set.map (\(x,y) -> (x,y-1))
    in Set.intersect translated (coordinates bottom)
    |> Set.toList
    |> List.isEmpty
    |> not


coordinates : Piece -> Set.Set (Int, Int)
coordinates piece = piece.blocks
    |> List.map (\(x,y) -> (x+piece.offsetX,y+piece.offsetY))
    |> Set.fromList


lowest : Piece -> Int
lowest piece = piece.blocks
    |> List.map (\(_,y) -> y + piece.offsetY)
    |> List.minimum


state : Signal (Piece, List Piece, Seed)
state = foldp update initial input


initial : (Piece, List Piece, Seed)
initial = ({piece3 | offsetY <- dropHeight}, [], initialSeed 4) -- guaranteed random


input : Signal Input
input = merge arrows timer

arrows : Signal Input
arrows = map Move Keyboard.arrows

timer : Signal Input
timer = map Timer (fps 5)


render : (Int, Int) -> (Piece, List Piece, Seed) -> Element
render (w', h') (currentPiece, groundedPieces, _) =
  let (w, h) = (toFloat w', toFloat h')
      pieces = currentPiece :: groundedPieces
  in List.map (draw (w', h')) pieces
  |> collage w' h'


draw : (Int, Int) -> Piece -> Form
draw (w, h) piece =
  List.map (\(x,y) ->
    square blockSize
    |> filled piece.color
    |> move (toFloat (x + piece.offsetX) * blockSize, toFloat (y + piece.offsetY) * blockSize))
  piece.blocks
  |> collage w h
  |> toForm


main : Signal Element
main = render <~ Window.dimensions ~ state
