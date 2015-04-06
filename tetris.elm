import Window
import Color (..)
import Signal (..)
import Time (..)
import List
import Graphics.Element (..)
import Graphics.Collage (..)

type alias Piece =
  { color:Color
  , blocks:List (Int, Int)
  , offsetX:Int
  , offsetY:Int
  }

blockSize = 20

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

update : Time -> (Piece, List Piece) -> (Piece, List Piece)
update _ (piece, pieces) =
  if touching piece pieces
  then (piece, pieces)
  else ({piece | offsetY <- piece.offsetY - 1}, pieces)


touching : Piece -> List Piece -> Bool
touching _ _ = True

state : Signal (Piece, List Piece)
state = foldp update initial input


initial : (Piece, List (Piece))
initial = ({piece1 | offsetY <- 10}, [piece2])


input : Signal Time
input = fps 5


render : (Int, Int) -> (Piece, List Piece) -> Element
render (w', h') (currentPiece, groundedPieces) =
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