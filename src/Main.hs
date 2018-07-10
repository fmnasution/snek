module Main where

import System.Random (randomR, StdGen)

main :: IO ()
main = putStrLn "Hello World!"

-- game logic

type Snake = [(Int, Int)]

type BoardSize = (Int,  Int)

type Board = [[(Int, Int)]]

type Direction = (Int, Int)

type Fruit = (Int, Int)

data Game = Game { getBoardSize :: BoardSize
                 , getBoard     :: Board
                 , getSnake     :: Snake
                 , getDirection :: Maybe Direction
                 , getFruit     :: Fruit
                 , getGameStdGen :: StdGen}

createBoard :: (Int, Int) -> Board
createBoard (x, y) = [[(x', y') | x' <- [1..x]] | y' <- [1..y]]

generateFruit :: Board -> StdGen -> (Fruit, StdGen)
generateFruit board stdGen = (fruitPos, gameStdGen)
  where
    flattenedBoard = mconcat board
    (index, gameStdGen) = randomR (0, length flattenedBoard - 1) stdGen
    fruitPos = flattenedBoard !! index

updateGameFruit :: Game -> Game
updateGameFruit game@Game { getBoard = board, getGameStdGen = gameGen } =
  game { getFruit = fruit, getGameStdGen = newGameStdGen }
  where
    (fruit, newGameStdGen) = generateFruit board gameGen

translateDirection :: String -> Maybe Direction
translateDirection "w" = Just (0, 1)
translateDirection "a" = Just (-1, 0)
translateDirection "s" = Just (0, -1)
translateDirection "d" = Just (1, 0)
translateDirection _   = Nothing

updateGameDirection :: Maybe Direction ->  Game -> Game
updateGameDirection Nothing game = game
updateGameDirection newDirection game = game { getDirection = newDirection }

collided :: BoardSize -> Snake -> Bool
collided (boardX, boardY) snake =
  let
    currentHead@(headX, headY) = last snake
  in
    headX < 0
    || headY < 0
    || headX > boardX
    || headY > boardY
    || (elem currentHead . init) snake

moveSnake :: Maybe Direction -> Snake -> Snake
moveSnake Nothing snake = snake
moveSnake (Just direction) snake = addNewHead direction . tail $ snake
  where
    addNewHead ::  Direction -> Snake -> Snake
    addNewHead direction' snake' = snake ++ [calculateHead direction' snake']

    calculateHead ::  Direction -> Snake -> (Int, Int)
    calculateHead (dirX, dirY) snake' =
      let
        (headX, headY) = last snake'
      in
        (headX + dirX, headY + dirY)

updateGameSnakeMovement :: Game -> Game
updateGameSnakeMovement game@Game { getDirection = direction
                                  , getSnake     = snake } =
  game { getSnake = moveSnake direction snake }
