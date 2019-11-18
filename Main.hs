module Main where

import qualified Data.Vector as V
import System.Random

type Position = (Int, Int)
data SquareTypes = MINE | EMPTY

data BoardSquare = Square {  
    squareType :: SquareTypes, 
    neighbours :: Int, 
    marked :: Bool,
    position :: Position
}

instance Show BoardSquare where
    show (Square squareType neighbours _ _) = case squareType of
        MINE -> show "M"
        _   -> show neighbours

newtype Board = Board (V.Vector (V.Vector BoardSquare)) deriving (Show)

width :: Int
width = 10

height :: Int
height = 10

random2DBoard :: StdGen -> Int -> Int -> V.Vector (V.Vector Bool)
random2DBoard gen height width = V.generate height $ \i-> V.generate width $ \j -> (randomNumbers !! (height * i + j)) == 1
                where randomNumbers = randomRs (1,10) gen :: [Int]

initialiseBoard :: StdGen -> Int -> Int -> Board
initialiseBoard gen width height = Board ( V.generate height $
    \i -> V.generate width $
        \j -> let squareType = if (randomBoard V.! i) V.! j then MINE else EMPTY in
            Square squareType (numberOfMines randomBoard (i,j)) False (i,j) )
    where randomBoard = random2DBoard gen width height

numberOfMines :: V.Vector (V.Vector Bool) -> (Int,Int) -> Int
numberOfMines board (i,j) = length $ filter (\(i,j) -> (board V.! i) V.! j) (filter isValidIndex neighbours)
    where   neighbours = [(i-1,j-1),(i,j-1),(i+1,j-1),(i-1,j),(i+1,j),(i-1,j+1),(i,j+1),(i+1,j+1)] :: [(Int,Int)]

isValidIndex :: (Int,Int) -> Bool
isValidIndex (i,j) = i >= 0 && i < height && j >= 0 && j < width

main :: IO ()
main = do
    gen <- newStdGen
    let board = initialiseBoard gen 10 10
    print board