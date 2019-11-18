{-# LANGUAGE LambdaCase #-}

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



checkNeighbours :: Int -> Int -> [Int] -> Int
checkNeighbours i j randomNumbers = length $ filter (\case
                                                        (1, k) -> isNeighbour i j k
                                                        _ -> False) indexedList
                                    where indexedList = zip randomNumbers [0..]

isNeighbour :: Int -> Int -> Int -> Bool
isNeighbour i j k
    | k == (i-1 * height + j-1)     = isValid i j
    | k == (i-1 * height + j)       = isValid i j
    | k == (i-1 * height + j+1)     = isValid i j
    | k == (i * height + j-1)       = isValid i j
    | k == (i * height + j+1)       = isValid i j
    | k == (i+1 * height + j-1)     = isValid i j
    | k == (i+1 * height + j)       = isValid i j
    | k == (i+1 * height + j+1)     = isValid i j
    | otherwise = False

isValid :: Int -> Int -> Bool
isValid i j = i >= 0 && i < height && j >= 0 && j < width

random2DBoard :: StdGen -> Int -> Int -> V.Vector (V.Vector Bool)
random2DBoard gen = V.generate height $ \_-> V.generate width $ \_ -> (randomNumbers !! (height * i + j)) == 1
                where randomNumbers = randomRs (1,10) g :: [Int]

initialiseBoard :: StdGen -> Int -> Int -> Board
initialiseBoard gen width height = Board $ V.generate height $
    \i -> V.generate width $
        \j -> case randomNumbers !! (height * i + j) of
            1 -> Square MINE 0 False (i,j)
            _ -> Square EMPTY 0 False (i,j)
    where randomBoard = random2DBoard gen

main :: IO ()
main = do
    gen <- newStdGen
    let board = initialiseBoard gen 10 10
    print board