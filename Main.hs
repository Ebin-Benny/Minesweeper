module Main where

import qualified Data.Vector as V
import System.Random

type Position = (Int, Int)
data SquareTypes = MINE | EMPTY
data DisplayTypes = OPEN | CLOSED
data GameStatus = PLAYING | GAMEOVER | WIN

data BoardSquare = Square {  
    squareType :: SquareTypes, 
    neighbours :: Int, 
    marked :: Bool,
    position :: Position,
    shown :: DisplayTypes
}

instance Show BoardSquare where
    show (Square squareType neighbours marked _ shown) = case (squareType, shown, marked) of
        (_,_,True)              -> show "X"
        (MINE,CLOSED,False)     -> show "m"
        (MINE,OPEN,False)       -> show "M"
        (EMPTY,CLOSED,False)    -> show "C"
        (EMPTY,OPEN,False)      -> show neighbours

data Game = Game {
    boardSquares :: (V.Vector (V.Vector BoardSquare)),
    gameStatus :: GameStatus
} 

instance Show Game where
    show (Game board _) = show board

width :: Int
width = 10

height :: Int
height = 10

random2DBoard :: StdGen -> Int -> Int -> V.Vector (V.Vector Bool)
random2DBoard gen height width = V.generate height $ \i-> V.generate width $ \j -> (randomNumbers !! (height * i + j)) == 1
                where randomNumbers = randomRs (1,10) gen :: [Int]

initialiseGame :: StdGen -> Int -> Int -> Game
initialiseGame gen width height = Game ( V.generate height $
    \i -> V.generate width $
        \j -> let squareType = if (randomBoard V.! i) V.! j then MINE else EMPTY in
            Square squareType (numberOfMines randomBoard (i,j)) False (i,j) CLOSED ) PLAYING
    where randomBoard = random2DBoard gen width height

numberOfMines :: V.Vector (V.Vector Bool) -> (Int,Int) -> Int
numberOfMines board (i,j) = length $ filter (\(i,j) -> (board V.! i) V.! j) (neighbourIndices (i,j))

neighbourIndices :: (Int, Int) -> [(Int, Int)]
neighbourIndices (i,j) = filter isValidIndex neighbours   
    where neighbours = [(i-1,j-1),(i,j-1),(i+1,j-1),(i-1,j),(i+1,j),(i-1,j+1),(i,j+1),(i+1,j+1)] :: [(Int,Int)]

isValidIndex :: (Int,Int) -> Bool
isValidIndex (i,j) = i >= 0 && i < height && j >= 0 && j < width

openSquare :: Game -> (Int, Int) -> Game
openSquare (Game game status) (i,j) = case (game V.! i) V.! j of
    (Square squareType 0 marked (i,j) CLOSED) -> foldl openSquare (Game updatedBoard status) (neighbourIndices (i,j))
        where updatedBoard = game V.// [(i,(game V.! i V.// [(j, (Square squareType 0 marked (i,j) OPEN))]))]
    (Square squareType neighbours marked (i,j) CLOSED) -> (Game updatedBoard status)
        where updatedBoard = game V.// [(i,(game V.! i V.// [(j, (Square squareType neighbours marked (i,j) OPEN))]))]
    (Square squareType neighbours marked (i,j) OPEN) -> Game game status

markSquare :: Game -> (Int, Int) -> Game
markSquare (Game game status) (i,j) = case (game V.! i) V.! j of
    (Square squareType neighbours True (i,j) shown) -> (Game updatedBoard status)
        where updatedBoard = game V.// [(i,(game V.! i V.// [(j, (Square squareType neighbours False (i,j) shown))]))]
    (Square squareType neighbours False (i,j) shown) -> (Game updatedBoard status)
        where updatedBoard = game V.// [(i,(game V.! i V.// [(j, (Square squareType neighbours True (i,j) shown))]))]



main :: IO ()
main = do
    gen <- newStdGen
    let game = initialiseGame gen 10 10
    let markedBoard = markSquare game (2,1)
    let updatedBoard = openSquare markedBoard (3,4)
    print updatedBoard