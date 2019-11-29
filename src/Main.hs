{-# Language ScopedTypeVariables #-}
module Main where

import qualified Data.Vector                   as V
import           System.Random
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Data.IORef
import           Control.Monad.Trans            ( liftIO )
import           Game
import           Moves

canvasSize :: Int
canvasSize = 800

main :: IO ()
main = do
  gen <- newStdGen
  let randomNumbers = randomRs (0, size-1) gen :: [Int]
  let game = initialiseGame randomNumbers
  startGUI defaultConfig (setup game randomNumbers)

-- |Setup takes in the initial game as well as a list of random numbers  
setup :: Game-> [Int] -> Window -> UI ()
setup game randomNumbers window = do
  return window # set title "Minesweeper"

  canvas <- UI.canvas # set UI.width canvasSize # set UI.height canvasSize # set
    UI.style
    [("border", "solid black 1px"), ("background", "#000")]

  openMode   <- UI.button #+ [string "Open"]
  markMode   <- UI.button #+ [string "Mark"]
  moveButton <- UI.button #+ [string "Make Move"]
  newButton <- UI.button #+ [string "Start New Game"]

  drawGame game canvas

  getBody window
    #+ [ column [element canvas]
       , element openMode
       , element markMode
       , element moveButton
       , element newButton
       ]

  currentGame <- liftIO $ newIORef (game)
  currentMode <- liftIO $ newIORef OPENING
  currentCount <- liftIO $ newIORef 1

  on UI.click openMode $ \_ -> do
    liftIO $ writeIORef currentMode OPENING

  on UI.click markMode $ \_ -> do
    liftIO $ writeIORef currentMode MARKING

  on UI.click moveButton $ \_ -> do
    current <- liftIO $ readIORef currentGame
    let latestGame = makeMove randomNumbers current
    liftIO $ writeIORef currentGame latestGame
    do
      drawGame latestGame canvas

  on UI.click newButton $ \_ -> do
    canvas # UI.clearCanvas
    current <- liftIO $ readIORef currentGame
    count <- liftIO $ readIORef currentCount
    let latestGame = initialiseGame (drop (count * (size * size)) randomNumbers)
    liftIO $ writeIORef currentGame latestGame
    liftIO $ writeIORef currentCount (count+1)
    do
      drawGame latestGame canvas

  on UI.mousedown canvas $ \(x, y) -> do
    mode <- liftIO $ readIORef currentMode
    case mode of
      OPENING -> do
        current <- liftIO $ readIORef currentGame
        let latestGame = openSquare
              current
              ( (y `div` (canvasSize `div` size))
              , (x `div` (canvasSize `div` size))
              )
        liftIO $ writeIORef currentGame latestGame
        do
          drawGame latestGame canvas
      MARKING -> do
        current <- liftIO $ readIORef currentGame
        let latestGame = markSquare
              current
              ( (y `div` (canvasSize `div` size))
              , (x `div` (canvasSize `div` size))
              )
        liftIO $ writeIORef currentGame latestGame
        do
          drawGame latestGame canvas

-- |Draws the game          
drawGame :: Game -> Element -> UI ()
drawGame (Game board status) canvas = do
  board <- V.forM board (drawRow)
  case status of
    GAMEOVER -> do
      canvas # set' UI.fillStyle (UI.htmlColor "black")
      canvas # UI.fillRect (200, 275) 400 200
      canvas # set' UI.fillStyle (UI.htmlColor "white")
      canvas # set' UI.textAlign (UI.Center)
      canvas # set' UI.textFont "52px sans-serif"
      canvas # UI.fillText "LOSE" (400, 400)
    WIN -> do
      canvas # set' UI.fillStyle (UI.htmlColor "black")
      canvas # UI.fillRect (200, 275) 400 200
      canvas # set' UI.fillStyle (UI.htmlColor "white")
      canvas # set' UI.textAlign (UI.Center)
      canvas # set' UI.textFont "52px sans-serif"
      canvas # UI.fillText "WIN" (400, 400)
    _ -> return ()
  return ()
  where drawRow row = V.forM row (\square -> drawBoardSquare square canvas)

-- |Draws a board square  
drawBoardSquare :: BoardSquare -> Element -> UI ()
drawBoardSquare (Square squareType _ False (i, j) CLOSED) canvas = do
  canvas # set' UI.fillStyle (UI.htmlColor "gray")
  canvas # UI.fillRect
    ( fromIntegral ((j * (canvasSize `div` size) + 3))
    , fromIntegral ((i * (canvasSize `div` size) + 3))
    )
    (fromIntegral (canvasSize `div` size - 6))
    (fromIntegral (canvasSize `div` size - 6))

drawBoardSquare (Square _ _ True (i, j) _) canvas = do
  canvas # set' UI.fillStyle (UI.htmlColor "green")
  canvas # UI.fillRect
    ( fromIntegral ((j * (canvasSize `div` size) + 3))
    , fromIntegral ((i * (canvasSize `div` size) + 3))
    )
    (fromIntegral (canvasSize `div` size - 6))
    (fromIntegral (canvasSize `div` size - 6))
  canvas # set' UI.fillStyle (UI.htmlColor "white")
  canvas # set' UI.textAlign (UI.Center)
  canvas # set' UI.textFont "32px sans-serif"
  canvas # UI.fillText
    ("*")
    ( fromIntegral
      ((j * (canvasSize `div` size)) + ((canvasSize `div` size) `div` 2))
    , fromIntegral
      ((i * (canvasSize `div` size)) + ((canvasSize `div` size) - 6))
    )

drawBoardSquare (Square MINE _ False (i, j) OPEN) canvas = do
  canvas # set' UI.fillStyle (UI.htmlColor "red")
  canvas # UI.fillRect
    ( fromIntegral ((j * (canvasSize `div` size) + 3))
    , fromIntegral ((i * (canvasSize `div` size) + 3))
    )
    (fromIntegral (canvasSize `div` size - 6))
    (fromIntegral (canvasSize `div` size - 6))

drawBoardSquare (Square EMPTY 0 False (i, j) OPEN) canvas = do
  canvas # set' UI.fillStyle (UI.htmlColor "white")
  canvas # UI.fillRect
    ( fromIntegral ((j * (canvasSize `div` size) + 3))
    , fromIntegral ((i * (canvasSize `div` size) + 3))
    )
    (fromIntegral (canvasSize `div` size - 6))
    (fromIntegral (canvasSize `div` size - 6))

drawBoardSquare (Square EMPTY neighbours False (i, j) OPEN) canvas = do
  canvas # set' UI.fillStyle (UI.htmlColor "white")
  canvas # UI.fillRect
    ( fromIntegral ((j * (canvasSize `div` size) + 3))
    , fromIntegral ((i * (canvasSize `div` size) + 3))
    )
    (fromIntegral (canvasSize `div` size - 6))
    (fromIntegral (canvasSize `div` size - 6))
  case neighbours of
    1          ->   canvas # set' UI.fillStyle (UI.htmlColor "#0000ff")
    2          ->   canvas # set' UI.fillStyle (UI.htmlColor "#008100")
    3          ->   canvas # set' UI.fillStyle (UI.htmlColor "#ff1300")
    4          ->   canvas # set' UI.fillStyle (UI.htmlColor "#000083")
    5          ->   canvas # set' UI.fillStyle (UI.htmlColor "#810500")
    6          ->   canvas # set' UI.fillStyle (UI.htmlColor "#2a9494")
    7          ->   canvas # set' UI.fillStyle (UI.htmlColor "#000000")
    8          ->   canvas # set' UI.fillStyle (UI.htmlColor "#808080")
    _          ->   canvas # set' UI.fillStyle (UI.htmlColor "black")
  canvas # set' UI.textAlign (UI.Center)
  canvas # set' UI.textFont "24px sans-serif"
  canvas # UI.fillText
    (show neighbours)
    ( fromIntegral
      ((j * (canvasSize `div` size)) + ((canvasSize `div` size) `div` 2))
    , fromIntegral
      ((i * (canvasSize `div` size)) + ((canvasSize `div` size) - 10))
    )

    
