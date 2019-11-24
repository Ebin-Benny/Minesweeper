{-# Language ScopedTypeVariables #-}
module Main where

import qualified Data.Vector as V
import System.Random
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Reactive.Threepenny

type Position = (Int, Int)
data SquareTypes = MINE | EMPTY
data DisplayTypes = OPEN | CLOSED
data GameStatus = PLAYING | GAMEOVER | WIN

canvasSize = 400

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
openSquare (Game board status) (i,j) = case (board V.! i) V.! j of
    (Square squareType 0 marked (i,j) CLOSED) -> foldl openSquare (Game updatedBoard status) (neighbourIndices (i,j))
        where updatedBoard = board V.// [(i,(board V.! i V.// [(j, (Square squareType 0 marked (i,j) OPEN))]))]
    (Square squareType neighbours marked (i,j) CLOSED) -> (Game updatedBoard status)
        where updatedBoard = board V.// [(i,(board V.! i V.// [(j, (Square squareType neighbours marked (i,j) OPEN))]))]
    (Square squareType neighbours marked (i,j) OPEN) -> Game board status

markSquare :: Game -> (Int, Int) -> Game
markSquare (Game board status) (i,j) = case (board V.! i) V.! j of
    (Square squareType neighbours True (i,j) shown) -> (Game updatedBoard status)
        where updatedBoard = board V.// [(i,(board V.! i V.// [(j, (Square squareType neighbours False (i,j) shown))]))]
    (Square squareType neighbours False (i,j) shown) -> (Game updatedBoard status)
        where updatedBoard = board V.// [(i,(board V.! i V.// [(j, (Square squareType neighbours True (i,j) shown))]))]

updateGameStatus :: Game -> Game
updateGameStatus (Game board _) = Game board (foldl (foldRows) WIN board)
    where foldRows gameStatus row = foldl (\oldStatus (Square _ _ marked _ _) -> case marked of
                                            True    -> gameStatus
                                            False   -> PLAYING
                                            ) gameStatus row

main :: IO ()
main = do
    gen <- newStdGen
    let game = initialiseGame gen 10 10
    startGUI defaultConfig (setup game)

setup :: Game -> Window -> UI ()
setup game window = do
    return window # set title "Minesweeper"

    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width canvasSize
        # set UI.style [("border", "solid black 1px"), ("background", "#000")]

    drawBoard game canvas

    getBody window #+
        [element canvas]
  
    mousePos <- stepper (0,0) $ UI.mousemove canvas

    let inputEvent :: Event (Game -> Game)
        inputEvent = updateGame <$ UI.click canvas
  
    currentGame <- accumB game inputEvent

    let bst :: Behavior (Game, (Int,Int))
        bst = (,) <$> currentGame <*> mousePos
  
        eDraw :: Event (Game, (Int,Int))
        eDraw = bst <@ UI.click canvas
  
    onEvent eDraw $ \(latestGame,_) -> do drawBoard latestGame canvas
    return ()

    

updateGame :: Game -> Game
updateGame game = openSquare game (1,1) 

drawShape :: (Int,Int) -> Element -> UI ()
drawShape (x,y) canvas = do
    canvas # set' UI.fillStyle   (UI.htmlColor "white")
    canvas # UI.fillRect (fromIntegral x,fromIntegral y) 1 1

drawBoard :: Game -> Element -> UI ()
drawBoard (Game board _) canvas = do 
    board <- V.forM board (drawRow)
    return ()
    where drawRow row = V.forM row (\square -> drawBoardSquare square canvas)

drawBoardSquare :: BoardSquare -> Element -> UI ()
drawBoardSquare (Square squareType _ marked (i,j) CLOSED) canvas = do
    canvas # set' UI.fillStyle   (UI.htmlColor "gray")
    canvas # UI.fillRect (fromIntegral ((j*(canvasSize `div` width) + 3)),fromIntegral ((i*(canvasSize `div` height) + 3))) (fromIntegral(canvasSize `div` width - 6)) (fromIntegral(canvasSize `div` height - 6))

drawBoardSquare (Square squareType 0 marked (i,j) OPEN) canvas = do
    canvas # set' UI.fillStyle   (UI.htmlColor "white")
    canvas # UI.fillRect (fromIntegral ((j*(canvasSize `div` width) + 3)),fromIntegral ((i*(canvasSize `div` height) + 3))) (fromIntegral(canvasSize `div` width - 6)) (fromIntegral(canvasSize `div` height - 6))

drawBoardSquare (Square squareType neighbours marked (i,j) OPEN) canvas = do
    canvas # set' UI.fillStyle   (UI.htmlColor "white")
    canvas # UI.fillRect (fromIntegral ((j*(canvasSize `div` width) + 3)),fromIntegral ((i*(canvasSize `div` height) + 3))) (fromIntegral(canvasSize `div` width - 6)) (fromIntegral(canvasSize `div` height - 6))
    canvas # set' UI.fillStyle   (UI.htmlColor "black")
    canvas # set' UI.textAlign   (UI.Center)
    canvas # set' UI.textFont    "24px sans-serif"
    canvas # UI.fillText (show neighbours) (fromIntegral ((j*(canvasSize `div` width)) + ((canvasSize `div` width) `div` 2)), fromIntegral ((i*(canvasSize `div` height)) + ((canvasSize `div` height)-10)))
    
--main :: IO ()
--main = do
--    gen <- newStdGen
--    let game = initialiseGame gen 10 10
--    let markedBoard = markSquare game (2,1)
--    let updatedBoard = openSquare markedBoard (3,4)
--    print updatedBoard