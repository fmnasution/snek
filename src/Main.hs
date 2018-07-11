module Main where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( concurrently )
import Control.Monad.Loops ( iterateUntilM )
import System.Console.ANSI ( clearScreen, setCursorPosition )
import System.IO ( hSetBuffering
                 , BufferMode(NoBuffering)
                 , stdin
                 , stdout
                 , hSetEcho)
import System.Random ( randomR, getStdGen, StdGen )
import System.Timeout ( timeout )

main :: IO Game
main =
  initScreen
  >> initGame
  >>= iterateUntilM gameOver (tickGame 150000)

-- game logic

type Coord = (Int, Int)

type Snake = [Coord]

type BoardSize = (Int,  Int)

type Board = [[Coord]]

data Direction = North | East | West | South

type Fruit = Coord

data Game = Game { getBoardSize   :: BoardSize
                 , getBoard       :: Board
                 , getSnake       :: Snake
                 , getDirection   :: Direction
                 , getFruit       :: Fruit
                 , getEatenFruits :: [Fruit]
                 , getGameStdGen  :: StdGen }

createBoard :: (Int, Int) -> Board
createBoard (x, y) = [[(x', y') | x' <- [1..x]] | y' <- [1..y]]

generateFruit :: Board -> StdGen -> (Fruit, StdGen)
generateFruit board stdGen =
  let
    flattenedBoard = mconcat board
    (index, gameStdGen) = randomR (0, length flattenedBoard - 1) stdGen
    fruitPos = flattenedBoard !! index
  in
    (fruitPos, gameStdGen)

updateGameGenerateFruit :: Game -> Game
updateGameGenerateFruit game@Game { getBoard = board, getGameStdGen = gameGen } =
  let
    (fruit, newGameStdGen) = generateFruit board gameGen
  in
    game { getFruit = fruit, getGameStdGen = newGameStdGen }

keypressDirection :: Maybe Char -> Maybe Direction
keypressDirection (Just 'w') = Just North
keypressDirection (Just 'a') = Just West
keypressDirection (Just 's') = Just South
keypressDirection (Just 'd') = Just East
keypressDirection _   = Nothing

updateGameDirection :: Maybe Direction ->  Game -> Game
updateGameDirection Nothing game = game
updateGameDirection (Just direction) game
  | direction `opposite` getDirection game = game
  | otherwise                              = game { getDirection = direction }
  where
    opposite :: Direction -> Direction -> Bool
    opposite North South = True
    opposite North _     = False
    opposite East West   = True
    opposite East _      = False
    opposite West East   = True
    opposite West _      = False
    opposite South North = True
    opposite South _     = False

collideWithBorder :: BoardSize -> Snake -> Bool
collideWithBorder (boardX, boardY) snake =
  let
    (headX, headY) = last snake
  in
    headX < 1 || headY < 1 || headX > boardX || headY > boardY

collideWithSelf :: Snake -> Bool
collideWithSelf snake = elem (last snake) . init $ snake

addCoord :: Coord -> Coord -> Coord
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subtractCoord :: Coord -> Coord -> Coord
subtractCoord (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

toCoord :: Coord -> Direction -> Coord
toCoord coord North = addCoord coord (0, -1)
toCoord coord West = addCoord coord (-1, 0)
toCoord coord South = addCoord coord (0, 1)
toCoord coord East = addCoord coord (1, 0)

moveSnake :: Direction -> Snake -> Snake
moveSnake direction =
  moveHead direction . moveTail
  where
    moveHead :: Direction -> Snake -> Snake
    moveHead direction' snake' = snake' ++ [toCoord (last snake') direction']

    moveTail = tail

updateGameSnakeDirection :: Game -> Game
updateGameSnakeDirection game@Game { getDirection = direction
                                   , getSnake     = snake } =
  game { getSnake = moveSnake direction snake }

updateGameFruitEating :: Game -> Game
updateGameFruitEating game@Game { getSnake       = snake
                                , getFruit       = fruit
                                , getEatenFruits = eatenFruits }
  | fruit == last snake =
      updateGameGenerateFruit game { getEatenFruits = eatenFruits ++ [fruit] }
  | otherwise           = game

updateGameGrowSnake :: Game -> Game
updateGameGrowSnake game@Game { getEatenFruits = [] } = game
updateGameGrowSnake game@Game
  { getSnake = snake , getEatenFruits = (eatenFruitFirst:eatenFruitsTail) }
  | eatenFruitFirst `shouldGrowSnake` snake =
      game { getSnake       = eatenFruitFirst : snake
           , getEatenFruits = eatenFruitsTail }
  | otherwise                               = game
  where
    shouldGrowSnake :: Fruit -> Snake -> Bool
    shouldGrowSnake eatenFruit' snake' =
      let
        (snakeTail:afterSnakeTail:_) = snake'
      in
        isAdjacent snakeTail eatenFruit' && eatenFruit' /= afterSnakeTail

    isAdjacent :: Coord -> Coord -> Bool
    isAdjacent c1 c2 = f North || f East || f West || f South
      where
        f direction = toCoord c1 direction == c2

updateGamePerTick :: Game -> Game
updateGamePerTick =
  updateGameGrowSnake
  . updateGameFruitEating
  . updateGameSnakeDirection

gameOver :: Game -> Bool
gameOver Game { getBoardSize = boardSize
              , getSnake     = snake } =
  collideWithSelf snake || collideWithBorder boardSize snake

-- IO game

initGame :: IO Game
initGame =
  getStdGen
  >>= \gen -> let
                boardSize = (40, 20)
                board = createBoard boardSize
                (fruit, gameStdGen) = generateFruit board gen
              in
                return Game { getBoardSize   = boardSize
                            , getBoard       = board
                            , getSnake       = [(1, 1), (2, 1), (3, 1)]
                            , getDirection   = East
                            , getFruit       = fruit
                            , getEatenFruits = []
                            , getGameStdGen  = gameStdGen }

updateGame :: Maybe Char -> Game ->  Game
updateGame directionKey =
  updateGameDirection (keypressDirection directionKey)
  . updateGamePerTick

tickGame :: Int -> Game -> IO Game
tickGame tickLength game =
  updateGame' game . fst
  <$> concurrently (timeout tickLength getChar) (threadDelay tickLength)
  >>= renderGame
  where
    updateGame' = flip updateGame

renderGame :: Game -> IO Game
renderGame game =
  setCursorPosition 0 0
  >> putStr (displayGame game)
  >> return game

constructRow :: Game -> [String]
constructRow Game { getBoard       = board
                  , getFruit       = fruit
                  , getSnake       = snake
                  , getEatenFruits = eatenFruits } =
  (fmap . fmap) (translateCoord fruit eatenFruits snake) board
  where
    translateCoord :: Fruit -> [Fruit] -> Snake -> Coord -> Char
    translateCoord fruit' eatenFruits' snake' coord'
      | coord' `elem` eatenFruits' = '$'
      | fruit' == coord'           = '#'
      | coord' `elem` snake'       = '@'
      | otherwise                  = ' '

applyBorder :: BoardSize -> [String] -> [String]
applyBorder (xLength, _) rows  =
  let
    border = [replicate (xLength + 2) 'X']
  in
    border ++ fmap (\row -> "X" ++ row ++ "X") rows ++ border

displayGame :: Game -> String
displayGame game@Game { getBoardSize = boardSize } =
  unlines . applyBorder boardSize . constructRow $ game

initScreen :: IO ()
initScreen = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  clearScreen
