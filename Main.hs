{-# Language ScopedTypeVariables #-}
module Main where

import qualified Data.Vector                   as V
import           System.Random
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Data.IORef
import           Control.Monad.Trans            ( liftIO )

type Position = (Int, Int)
data SquareTypes = MINE | EMPTY
data DisplayTypes = OPEN | CLOSED
data GameStatus = PLAYING | GAMEOVER | WIN
data GameMode = MARKING | OPENING

canvasSize = 800

width :: Int
width = 20

height :: Int
height = 20

difficulty :: Int
difficulty = 5

data BoardSquare = Square {
    squareType :: SquareTypes,
    neighbours :: Int,
    marked :: Bool,
    position :: Position,
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
    boardSquares :: (V.Vector (V.Vector BoardSquare)),
    gameStatus :: GameStatus
}

instance Show Game where
  show (Game board _) = show board

random2DBoard :: [Int] -> V.Vector (V.Vector Bool)
random2DBoard randomNumbers = V.generate height
  $ \i -> V.generate width $ \j -> (randomNumbers !! (height * i + j)) == 1

initialiseGame :: [Int] -> Game
initialiseGame randomNumbers = Game
  (V.generate height $ \i -> V.generate width $ \j ->
    let squareType = if (randomBoard V.! i) V.! j then MINE else EMPTY
    in  Square squareType (numberOfMines randomBoard (i, j)) False (i, j) CLOSED
  )
  PLAYING
  where randomBoard = random2DBoard randomNumbers

numberOfMines :: V.Vector (V.Vector Bool) -> (Int, Int) -> Int
numberOfMines board (i, j) =
  length $ filter (\(i, j) -> (board V.! i) V.! j) (neighbourIndices (i, j))

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

isValidIndex :: (Int, Int) -> Bool
isValidIndex (i, j) = i >= 0 && i < height && j >= 0 && j < width

openSquare :: Game -> (Int, Int) -> Game
openSquare (Game board status) (i, j) = case (board V.! i) V.! j of
  (Square squareType 0 marked (i, j) CLOSED) -> foldl
    openSquare
    (Game updatedBoard (updatedGameStatus updatedBoard))
    (neighbourIndices (i, j))
   where
    updatedBoard =
      board
        V.// [ ( i
               , (    board
                 V.!  i
                 V.// [(j, (Square squareType 0 marked (i, j) OPEN))]
                 )
               )
             ]
  (Square squareType neighbours marked (i, j) CLOSED) ->
    (Game updatedBoard (updatedGameStatus updatedBoard))
   where
    updatedBoard =
      board
        V.// [ ( i
               , (    board
                 V.!  i
                 V.// [(j, (Square squareType neighbours marked (i, j) OPEN))]
                 )
               )
             ]
  (Square squareType neighbours marked (i, j) OPEN) -> Game board status

markSquare :: Game -> (Int, Int) -> Game
markSquare (Game board status) (i, j) = case (board V.! i) V.! j of
  (Square squareType neighbours True (i, j) CLOSED) ->
    (Game updatedBoard (updatedGameStatus updatedBoard))
   where
    updatedBoard =
      board
        V.// [ ( i
               , (    board
                 V.!  i
                 V.// [ ( j
                        , (Square squareType neighbours False (i, j) CLOSED)
                        )
                      ]
                 )
               )
             ]
  (Square squareType neighbours False (i, j) CLOSED) ->
    (Game updatedBoard (updatedGameStatus updatedBoard))
   where
    updatedBoard =
      board
        V.// [ ( i
               , (    board
                 V.!  i
                 V.// [(j, (Square squareType neighbours True (i, j) CLOSED))]
                 )
               )
             ]
  (Square squareType neighbours False (i, j) OPEN) -> (Game board status)

updatedGameStatus :: (V.Vector (V.Vector BoardSquare)) -> GameStatus
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

makeMove :: Game -> Game
makeMove (Game board PLAYING) = case openAllN board of
  Just ij -> openSquare (Game board PLAYING) ij
  Nothing -> case markAllN board of
    Just ij -> markSquare (Game board PLAYING) ij
    Nothing -> (Game board PLAYING)
makeMove game@(Game _ _) = game

openAllN :: (V.Vector (V.Vector BoardSquare)) -> Maybe (Int, Int)
openAllN board = foldl (foldRows) Nothing board
 where
  foldRows foundOpen row = foldl
    (\oldOpen (Square squareType _ marked (i, j) shown) ->
      case (oldOpen, openN (i, j) board) of
        (Nothing, Nothing    ) -> Nothing
        (Nothing, Just (x, y)) -> Just (x, y)
        (_)                    -> oldOpen
    )
    foundOpen
    row

openN :: (Int, Int) -> (V.Vector (V.Vector BoardSquare)) -> Maybe (Int, Int)
openN (i, j) board = case (board V.! i) V.! j of
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
      (\unmarked (x, y) -> case (board V.! x) V.! y of
        (Square _ _ False _ CLOSED) -> Just (x, y)
        _                           -> unmarked
      )
      Nothing
      (neighbourIndices (i, j))
  _ -> Nothing

markAllN :: (V.Vector (V.Vector BoardSquare)) -> Maybe (Int, Int)
markAllN board = foldl (foldRows) Nothing board
 where
  foldRows foundMark row = foldl
    (\oldMark (Square _ _ _ (i, j) _) -> case (oldMark, markN (i, j) board) of
      (Nothing, Nothing    ) -> Nothing
      (Nothing, Just (x, y)) -> Just (x, y)
      (_)                    -> oldMark
    )
    foundMark
    row

markN :: (Int, Int) -> (V.Vector (V.Vector BoardSquare)) -> Maybe (Int, Int)
markN (i, j) board = case (board V.! i) V.! j of
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
      (\unmarked (x, y) -> case (board V.! x) V.! y of
        (Square _ _ False _ CLOSED) -> Just (x, y)
        _                           -> unmarked
      )
      Nothing
      (neighbourIndices (i, j))
  _ -> Nothing

main :: IO ()
main = do
  gen <- newStdGen
  let randomNumbers = randomRs (1, difficulty) gen :: [Int]
  let game = initialiseGame randomNumbers
  startGUI defaultConfig (setup game randomNumbers)

setup :: Game-> [Int] -> Window -> UI ()
setup game randomNumbers window = do
  return window # set title "Minesweeper"

  canvas <- UI.canvas # set UI.height canvasSize # set UI.width canvasSize # set
    UI.style
    [("border", "solid black 1px"), ("background", "#000")]

  openMode   <- UI.button #+ [string "Open"]
  markMode   <- UI.button #+ [string "Mark"]
  moveButton <- UI.button #+ [string "Make Move"]
  newButton <- UI.button #+ [string "Start New Game"]

  drawBoard game canvas

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
    let latestGame = makeMove current
    liftIO $ writeIORef currentGame latestGame
    do
      drawBoard latestGame canvas

  on UI.click newButton $ \_ -> do
    canvas # UI.clearCanvas
    current <- liftIO $ readIORef currentGame
    count <- liftIO $ readIORef currentCount
    let latestGame = initialiseGame (drop (count * (width * height)) randomNumbers)
    liftIO $ writeIORef currentGame latestGame
    liftIO $ writeIORef currentCount (count+1)
    do
      drawBoard latestGame canvas

  on UI.mousedown canvas $ \(x, y) -> do
    mode <- liftIO $ readIORef currentMode
    case mode of
      OPENING -> do
        current <- liftIO $ readIORef currentGame
        let latestGame = openSquare
              current
              ( (y `div` (canvasSize `div` height))
              , (x `div` (canvasSize `div` width))
              )
        liftIO $ writeIORef currentGame latestGame
        do
          drawBoard latestGame canvas
      MARKING -> do
        current <- liftIO $ readIORef currentGame
        let latestGame = markSquare
              current
              ( (y `div` (canvasSize `div` height))
              , (x `div` (canvasSize `div` width))
              )
        liftIO $ writeIORef currentGame latestGame
        do
          drawBoard latestGame canvas

drawBoard :: Game -> Element -> UI ()
drawBoard (Game board status) canvas = do
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

drawBoardSquare :: BoardSquare -> Element -> UI ()
drawBoardSquare (Square squareType _ False (i, j) CLOSED) canvas = do
  canvas # set' UI.fillStyle (UI.htmlColor "gray")
  canvas # UI.fillRect
    ( fromIntegral ((j * (canvasSize `div` width) + 3))
    , fromIntegral ((i * (canvasSize `div` height) + 3))
    )
    (fromIntegral (canvasSize `div` width - 6))
    (fromIntegral (canvasSize `div` height - 6))

drawBoardSquare (Square _ _ True (i, j) _) canvas = do
  canvas # set' UI.fillStyle (UI.htmlColor "green")
  canvas # UI.fillRect
    ( fromIntegral ((j * (canvasSize `div` width) + 3))
    , fromIntegral ((i * (canvasSize `div` height) + 3))
    )
    (fromIntegral (canvasSize `div` width - 6))
    (fromIntegral (canvasSize `div` height - 6))
  canvas # set' UI.fillStyle (UI.htmlColor "white")
  canvas # set' UI.textAlign (UI.Center)
  canvas # set' UI.textFont "32px sans-serif"
  canvas # UI.fillText
    ("*")
    ( fromIntegral
      ((j * (canvasSize `div` width)) + ((canvasSize `div` width) `div` 2))
    , fromIntegral
      ((i * (canvasSize `div` height)) + ((canvasSize `div` height) - 6))
    )

drawBoardSquare (Square MINE _ False (i, j) OPEN) canvas = do
  canvas # set' UI.fillStyle (UI.htmlColor "red")
  canvas # UI.fillRect
    ( fromIntegral ((j * (canvasSize `div` width) + 3))
    , fromIntegral ((i * (canvasSize `div` height) + 3))
    )
    (fromIntegral (canvasSize `div` width - 6))
    (fromIntegral (canvasSize `div` height - 6))

drawBoardSquare (Square EMPTY 0 False (i, j) OPEN) canvas = do
  canvas # set' UI.fillStyle (UI.htmlColor "white")
  canvas # UI.fillRect
    ( fromIntegral ((j * (canvasSize `div` width) + 3))
    , fromIntegral ((i * (canvasSize `div` height) + 3))
    )
    (fromIntegral (canvasSize `div` width - 6))
    (fromIntegral (canvasSize `div` height - 6))

drawBoardSquare (Square EMPTY neighbours False (i, j) OPEN) canvas = do
  canvas # set' UI.fillStyle (UI.htmlColor "white")
  canvas # UI.fillRect
    ( fromIntegral ((j * (canvasSize `div` width) + 3))
    , fromIntegral ((i * (canvasSize `div` height) + 3))
    )
    (fromIntegral (canvasSize `div` width - 6))
    (fromIntegral (canvasSize `div` height - 6))
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
      ((j * (canvasSize `div` width)) + ((canvasSize `div` width) `div` 2))
    , fromIntegral
      ((i * (canvasSize `div` height)) + ((canvasSize `div` height) - 10))
    )
