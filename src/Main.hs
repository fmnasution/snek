module Main where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( concurrently )
import Control.Monad.Loops ( iterateUntilM )
import Data.List ( (\\) )
import Data.Tuple ( swap )
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

data Fruit = Fruit { getFruitPosition :: Coord
                   , getFruitScore    :: Int}

data Game = Game { getBoardSize   :: BoardSize
                 , getBoard       :: Board
                 , getSnake       :: Snake
                 , getDirection   :: Direction
                 , getFruit       :: Fruit
                 , getEatenFruits :: [Fruit]
                 , getBonusFruit  :: Maybe Fruit
                 , getScore       :: Int
                 , getGameStdGen  :: StdGen }

createBoard :: BoardSize -> Board
createBoard (x, y) = [[(x', y') | x' <- [1..x]] | y' <- [1..y]]

randomCoord :: StdGen -> [Coord] -> (StdGen, Coord)
randomCoord stdGen availableCoord =
  fmap (availableCoord !!)
  $ swap
  $ randomR (0, length availableCoord - 1) stdGen

generateFruit :: Board -> Snake -> StdGen -> (StdGen, Fruit)
generateFruit board snake stdGen =
  fmap mkFruit $ randomCoord  stdGen $ mconcat board \\ snake
  where
    mkFruit :: Coord -> Fruit
    mkFruit coord = Fruit { getFruitPosition = coord
                          , getFruitScore    = 10}

updateGameGenerateFruit :: Game -> Game
updateGameGenerateFruit game@Game { getSnake      = snake
                                  , getBoard      = board
                                  , getGameStdGen = gameStdGen } =
  let
    (newGameStdGen, fruit) = generateFruit board snake gameStdGen
  in
    game { getFruit = fruit, getGameStdGen = newGameStdGen }

generateBonusFruit :: Fruit -> Board -> Snake -> StdGen -> (StdGen, Fruit)
generateBonusFruit Fruit { getFruitPosition = fruitPosition } board snake stdGen =
  fmap mkFruit $ randomCoord stdGen $ mconcat board \\ (fruitPosition : snake)
  where
    mkFruit coord = Fruit { getFruitPosition = coord
                          , getFruitScore    = 40}

updateGameGenerateBonusFruit :: Game -> Game
updateGameGenerateBonusFruit game@Game { getBonusFruit = Just _ } = game
updateGameGenerateBonusFruit game@Game { getSnake      = snake
                                       , getBoard      = board
                                       , getFruit      = fruit
                                       , getGameStdGen = gameStdGen }
  | shouldGenerate = let
                       (newGameStdGen', bonusFruit) =
                         generateBonusFruit fruit board snake newGameStdGen
                     in
                       game { getBonusFruit = Just bonusFruit
                            , getGameStdGen = newGameStdGen' }
  | otherwise      = game
  where
    (newGameStdGen, shouldGenerate) =
      (> 0.8) <$> swap (randomR (0, 1) gameStdGen :: (Float, StdGen))

keypressDirection :: Maybe Char -> Maybe Direction
keypressDirection (Just 'w') = Just North
keypressDirection (Just 'a') = Just West
keypressDirection (Just 's') = Just South
keypressDirection (Just 'd') = Just East
keypressDirection _   = Nothing

oppositeDirection :: Direction -> Direction -> Bool
oppositeDirection North South = True
oppositeDirection East West   = True
oppositeDirection West East   = True
oppositeDirection South North = True
oppositeDirection _ _ = False

updateGameDirection :: Maybe Direction ->  Game -> Game
updateGameDirection Nothing game = game
updateGameDirection (Just direction) game
  | direction `oppositeDirection` getDirection game = game
  | otherwise                                       =
    game { getDirection = direction }

collideWithBorder :: BoardSize -> Snake -> Bool
collideWithBorder (boardX, boardY) snake =
  let
    (headX, headY) = last snake
  in
    headX < 1 || headY < 1 || headX > boardX || headY > boardY

collideWithSelf :: Snake -> Bool
collideWithSelf snake = elem (last snake) $ init snake

addCoord :: Coord -> Coord -> Coord
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

reflectCoordTo :: Coord -> Direction -> Coord
reflectCoordTo coord North = addCoord coord (0, -1)
reflectCoordTo coord West = addCoord coord (-1, 0)
reflectCoordTo coord South = addCoord coord (0, 1)
reflectCoordTo coord East = addCoord coord (1, 0)

moveSnake :: Direction -> Snake -> Snake
moveSnake direction =
  moveHead . moveTail
  where
    moveHead :: Snake -> Snake
    moveHead snake' = snake' ++ [newSnakeHeadCoord snake']

    moveTail = tail

    newSnakeHeadCoord snake' = reflectCoordTo (last snake') direction

updateGameSnakeDirection :: Game -> Game
updateGameSnakeDirection game@Game { getDirection = direction
                                   , getSnake     = snake } =
  game { getSnake = moveSnake direction snake }

fruitAtHead :: Fruit -> Snake -> Bool
fruitAtHead fruit snake = getFruitPosition fruit == last snake

updateGameFruitEating :: Game -> Game
updateGameFruitEating game@Game { getSnake       = snake
                                , getFruit       = fruit
                                , getEatenFruits = eatenFruits
                                , getScore       = score}
  | fruitAtHead' = genFruit game { getEatenFruits = eatenFruits ++ [fruit]
                                 , getScore       = score + getFruitScore fruit }
  | otherwise    = game
  where
    genFruit :: Game -> Game
    genFruit = updateGameGenerateBonusFruit . updateGameGenerateFruit

    fruitAtHead' = fruitAtHead fruit snake

updateGameBonusFruitEating :: Game -> Game
updateGameBonusFruitEating game@Game { getBonusFruit = Nothing } = game
updateGameBonusFruitEating game@Game { getSnake      = snake
                                     , getBonusFruit = (Just bonusFruit)
                                     , getScore      = score }
  | fruitAtHead' = game { getBonusFruit = Nothing
                        , getScore      = score + getFruitScore bonusFruit }
  where
    fruitAtHead' = fruitAtHead bonusFruit snake
updateGameBonusFruitEating game = game

updateGameGrowSnake :: Game -> Game
updateGameGrowSnake game@Game { getEatenFruits = [] } = game
updateGameGrowSnake game@Game
  { getSnake = snake , getEatenFruits = (eatenFruit:eatenFruitsTail) }
  | shouldGrowSnake =
      game { getSnake       = getFruitPosition eatenFruit : snake
           , getEatenFruits = eatenFruitsTail }
  | otherwise       = game
  where
    shouldGrowSnake :: Bool
    shouldGrowSnake =
      let
        (snakeTail:afterSnakeTail:_) = snake
        eatenFruitPos = getFruitPosition eatenFruit
      in
        isAdjacent snakeTail eatenFruitPos
        && eatenFruitPos /= afterSnakeTail

    isAdjacent :: Coord -> Coord -> Bool
    isAdjacent c1 c2 = f North || f East || f West || f South
      where
        f direction = reflectCoordTo c1 direction == c2

updateGamePerTick :: Game -> Game
updateGamePerTick =
  updateGameBonusFruitEating
  . updateGameGrowSnake
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
                snake = [(1, 1), (2, 1), (3, 1)]
                (gameStdGen, fruit) = generateFruit board snake gen
              in
                return Game { getBoardSize   = boardSize
                            , getBoard       = board
                            , getSnake       = snake
                            , getDirection   = East
                            , getFruit       = fruit
                            , getEatenFruits = []
                            , getBonusFruit  = Nothing
                            , getGameStdGen  = gameStdGen
                            , getScore       = 0}

updateGame :: Maybe Char -> Game ->  Game
updateGame directionKey =
  updateGameDirection' . updateGamePerTick
  where
    updateGameDirection' = updateGameDirection $ keypressDirection directionKey

tickGame :: Int -> Game -> IO Game
tickGame tickLength game =
  updateGame' game . fst
  <$> concurrently (timeout tickLength getChar) (threadDelay tickLength)
  >>= renderGame
  where
    updateGame' = flip updateGame

-- UI

renderGame :: Game -> IO Game
renderGame game =
  setCursorPosition 0 0
  >> putStr (displayGame game)
  >> return game

constructRow :: Game -> [String]
constructRow Game { getBoard       = board
                  , getFruit       = fruit
                  , getSnake       = snake
                  , getEatenFruits = eatenFruits
                  , getBonusFruit  = bonusFruit } =
  (fmap . fmap) translateCoord  board
  where
    translateCoord ::  Coord -> Char
    translateCoord  coord
      | bonusFruit `isBonusFruit` coord                = '&'
      | coord `elem` fmap getFruitPosition eatenFruits = '$'
      | getFruitPosition fruit == coord                = '#'
      | coord `elem` snake                             = '@'
      | otherwise                                      = ' '

    isBonusFruit :: Maybe Fruit -> Coord -> Bool
    isBonusFruit Nothing _ = False
    isBonusFruit (Just bonusFruit') coord =
      getFruitPosition bonusFruit' == coord

applyBorder :: BoardSize -> [String] -> [String]
applyBorder (xLength, _) rows  =
  let
    border = [replicate (xLength + 2) 'X']
  in
    border ++ fmap (\row -> "X" ++ row ++ "X") rows ++ border

constructInfo :: Int -> [String] -> [String]
constructInfo score rows =
  ("SCORE: " ++ show score) : rows

displayGame :: Game -> String
displayGame game@Game { getBoardSize = boardSize
                      , getScore     = score } =
  unlines . constructInfo score . applyBorder boardSize . constructRow $ game

initScreen :: IO ()
initScreen = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  clearScreen
