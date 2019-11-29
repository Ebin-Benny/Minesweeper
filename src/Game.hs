-- | Contains data types and functions for the underlying game structure.
module Game
  ( Game(..)
  , BoardSquare(..)
  , SquareTypes(..)
  , DisplayTypes(..)
  , GameStatus(..)
  , Mode(..)
  , Board
  , initialiseGame
  , neighbourIndices
  , openSquare
  , markSquare
  , size
  )
where

import qualified Data.Vector                   as V

data SquareTypes = MINE | EMPTY
data DisplayTypes = OPEN | CLOSED
data GameStatus = PLAYING | GAMEOVER | WIN
data Mode = MARKING | OPENING
type Board = V.Vector (V.Vector BoardSquare)

size :: Int
size = 20

difficulty :: Int
difficulty = 2

data BoardSquare = Square {
    squareType :: SquareTypes,
    neighbours :: Int,
    marked :: Bool,
    position :: (Int, Int),
    shown :: DisplayTypes
}

instance Show BoardSquare where
  show (Square squareType neighbours marked _ shown) =
    case (squareType, shown, marked) of
      (_    , _     , True ) -> show "X"
      (MINE , CLOSED, False) -> show "m"
      (MINE , OPEN  , False) -> show "M"
      (EMPTY, CLOSED, False) -> show "C"
      (EMPTY, OPEN  , False) -> show neighbours

data Game = Game {
    board :: Board,
    gameStatus :: GameStatus
}

instance Show Game where
  show (Game board _) = show board

-- |Initialises a new game with unmarked and unopened square, populates mines based on random list of ints  
initialiseGame :: [Int] -> Game
initialiseGame randomNumbers = Game
  (V.generate size $ \i -> V.generate size $ \j ->
    let squareType = if (randomBoard V.! i) V.! j then MINE else EMPTY
    in  Square squareType (numberOfMines randomBoard (i, j)) False (i, j) CLOSED
  )
  PLAYING
 where
  randomBoard = V.generate size $ \i ->
    V.generate size $ \j -> (randomNumbers !! (size * i + j)) <= difficulty
  numberOfMines board (i, j) =
    length $ filter (\(i, j) -> (board V.! i) V.! j) (neighbourIndices (i, j))

-- |Returns the indices for the neighbours of a certain position         
neighbourIndices :: (Int, Int) -> [(Int, Int)]
neighbourIndices (i, j) = filter isValidIndex neighbours
 where
  neighbours =
    [ (i - 1, j - 1)
    , (i    , j - 1)
    , (i + 1, j - 1)
    , (i - 1, j)
    , (i + 1, j)
    , (i - 1, j + 1)
    , (i    , j + 1)
    , (i + 1, j + 1)
    ] :: [(Int, Int)]
  isValidIndex (i, j) = i >= 0 && i < size && j >= 0 && j < size

-- |Updates a specific board square in the board
updateBoardSquare :: Board -> (Int, Int) -> BoardSquare -> Board
updateBoardSquare board (i, j) square =
  board V.// [(i, (board V.! i V.// [(j, square)]))]

{-|
    Opens a square on the board and updates the game state accordingly.
    
    Will propagate opening if the square that was opened was empty and had no neighbours.
-}
openSquare :: Game -> (Int, Int) -> Game
openSquare (Game board status) (i, j) = case (board V.! i) V.! j of
  (Square squareType 0 marked (i, j) CLOSED) -> foldl
    openSquare
    (Game updatedBoard (updatedGameStatus updatedBoard))
    (neighbourIndices (i, j))
   where
    updatedBoard =
      updateBoardSquare board (i, j) (Square squareType 0 marked (i, j) OPEN)
  (Square squareType neighbours marked (i, j) CLOSED) ->
    (Game updatedBoard (updatedGameStatus updatedBoard))
   where
    updatedBoard = updateBoardSquare
      board
      (i, j)
      (Square squareType neighbours marked (i, j) OPEN)
  (Square squareType neighbours marked (i, j) OPEN) -> Game board status

-- |Marks a square on the board as a mine and updates the game state accordingly
markSquare :: Game -> (Int, Int) -> Game
markSquare (Game board status) (i, j) = case (board V.! i) V.! j of
  (Square squareType neighbours True (i, j) CLOSED) ->
    (Game updatedBoard (updatedGameStatus updatedBoard))
   where
    updatedBoard = updateBoardSquare
      board
      (i, j)
      (Square squareType neighbours False (i, j) CLOSED)
  (Square squareType neighbours False (i, j) CLOSED) ->
    (Game updatedBoard (updatedGameStatus updatedBoard))
   where
    updatedBoard = updateBoardSquare
      board
      (i, j)
      (Square squareType neighbours True (i, j) CLOSED)
  (Square squareType neighbours False (i, j) OPEN) -> (Game board status)

-- |Gives an updates game status based on a board
updatedGameStatus :: Board -> GameStatus
updatedGameStatus board = foldl (foldRows) WIN board
 where
  foldRows gameStatus row = foldl
    (\oldStatus (Square squareType _ marked _ shown) ->
      case (oldStatus, squareType, marked, shown) of
        (GAMEOVER, _    , _, _     ) -> GAMEOVER
        (_       , MINE , _, OPEN  ) -> GAMEOVER
        (_       , MINE , _, CLOSED) -> oldStatus
        (_       , EMPTY, _, CLOSED) -> PLAYING
        (_       , EMPTY, _, OPEN  ) -> oldStatus
    )
    gameStatus
    row
