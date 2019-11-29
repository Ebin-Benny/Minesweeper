-- | Contains implemetations for different ways of getting the next move.
module Moves
  ( makeMove
  )
where

import qualified Data.Vector                   as V
import           Control.Applicative
import           Game

type Move = (Mode, (Int, Int))

-- |Makes the next best move.
makeMove :: [Int] -> Game -> Game
makeMove randomNumbers game@(Game _ PLAYING) =
  case nextMove randomNumbers game of
    Just (MARKING, ij) -> markSquare game ij
    Just (OPENING, ij) -> openSquare game ij
    Nothing            -> game

makeMove _ game@(Game _ _) = game

-- |Returns the next move, uses rules to find moves, if there are no moves from the rules, then returns random move.
nextMove :: [Int] -> Game -> Maybe Move
nextMove randomNumbers (Game board _) =
  openWithNeighbours board <|> markWithNeighbours board <|> randomMove randomNumbers board

-- |Makes a random move
randomMove :: [Int] -> Board -> Maybe Move
randomMove (x : y : xs) board = case (board V.! y) V.! x of
  (Square _ _ _ _ CLOSED) -> Just (OPENING, (y, x))
  (Square _ _ _ _ OPEN  ) -> randomMove xs board
randomMove (x : xs) _ = Nothing

-- |Makes a move to open a square if the amount of neighbours it has is already marked.
openWithNeighbours :: Board -> Maybe Move
openWithNeighbours board = foldl (foldRows) Nothing board
 where
  foldRows foundMove row =
    foundMove
      <|> foldl
            (\moves (Square squareType _ marked (i, j) shown) ->
              moves <|> openSquareWithNeighbours (i, j) board
            )
            foundMove
            row

-- |Checks if the amount of neighbours it has is already marked, if so, open a unopened unmarked square
openSquareWithNeighbours :: (Int, Int) -> Board -> Maybe Move
openSquareWithNeighbours (i, j) board = case (board V.! i) V.! j of
  (Square _ neighbours _ _ OPEN) -> if markedCount == neighbours
    then unMarkedSquare
    else Nothing
   where
    markedCount = foldl
      (\count (x, y) -> case (board V.! x) V.! y of
        (Square _ _ True _ _) -> (count + 1)
        _                     -> count
      )
      0
      (neighbourIndices (i, j))
    unMarkedSquare = foldl
      (\unmarked (x, y) -> unmarked <|> case (board V.! x) V.! y of
        (Square _ _ False _ CLOSED) -> Just (OPENING, (x, y))
        _                           -> Nothing
      )
      Nothing
      (neighbourIndices (i, j))
  _ -> Nothing

-- |Makes a move to mark a square as a mine if the number of unopened squares is equal to number of neighbours  
markWithNeighbours :: Board -> Maybe Move
markWithNeighbours board = foldl (foldRows) Nothing board
 where
  foldRows foundMove row =
    foundMove
      <|> foldl
            (\moves (Square squareType _ marked (i, j) shown) ->
              moves <|> markSquareWithNeighbours (i, j) board
            )
            foundMove
            row

-- |Marks a square as a mine if the number of unopened squares is equal to number of neighbours 
markSquareWithNeighbours :: (Int, Int) -> Board -> Maybe Move
markSquareWithNeighbours (i, j) board = case (board V.! i) V.! j of
  (Square _ neighbours _ _ OPEN) -> if unOpenedCount == neighbours
    then unMarkedSquare
    else Nothing
   where
    unOpenedCount = foldl
      (\count (x, y) -> case (board V.! x) V.! y of
        (Square _ _ _ _ CLOSED) -> (count + 1)
        _                       -> count
      )
      0
      (neighbourIndices (i, j))
    unMarkedSquare = foldl
      (\unmarked (x, y) -> unmarked <|> case (board V.! x) V.! y of
        (Square _ _ False _ CLOSED) -> Just (MARKING, (x, y))
        _                           -> unmarked
      )
      Nothing
      (neighbourIndices (i, j))
  _ -> Nothing
