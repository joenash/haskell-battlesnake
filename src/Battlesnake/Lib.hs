module Battlesnake.Lib where

import Battlesnake.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Control.Applicative

data CellContents
  = FoodCell
  | HazCell
  | SnakeCell Text.Text SnakePart
  deriving (Show)

data SnakePart = Head | Tail | Body deriving (Show)

newtype FullBoard = FullBoard (Map.Map Coord [CellContents])
  deriving (Show)

mkMove :: Direction -> Text.Text -> Move
mkMove DUp s = Move {move = DUp, shout = s}
mkMove DDown s = Move {move = DDown, shout = s}
mkMove DLeft s = Move {move = DLeft, shout = s}
mkMove DRight s = Move {move = DRight, shout = s}

coordsToMove :: Coord -> Coord -> Move 
coordsToMove (Coord x y) (Coord x1 y1) 
  | x > x1 && y == y1 = mkMove DLeft "Move left"
  | x < x1 && y == y1 = mkMove DRight "Move right"
  | x == x && y > y1 = mkMove DDown "Move down" 
  | x == x && y < y1 = mkMove DUp "Move up"
coordsToMove (Coord _ _) (Coord _ _) = mkMove DUp "Error"

coordsToDirection :: Coord -> Coord -> Direction
coordsToDirection (Coord x y) (Coord x1 y1)
  | x > x1 && y == y1 = DLeft
  | x < x1 && y == y1 = DRight
  | x == x && y > y1 = DDown
  | x == x && y < y1 = DUp
coordsToDirection (Coord _ _) (Coord _ _) = DUp

buildBoard :: Board -> FullBoard
buildBoard b = board
  where 
    w = boardWidth b 
    h = boardHeight b
    snakes = boardSnakes b 
    food = boardFood b 
    hazards = boardHazards b 
    board = 
      hazardsOnBoard hazards
      $ foodOnBoard food 
      $ snakesOnBoard snakes 
      $ emptyBoard w h 

emptyBoard :: Int -> Int -> FullBoard 
emptyBoard w h = FullBoard $ foldr (\c m -> Map.insert c [] m) (Map.empty) coords 
  where 
    coords = [ Coord {coordX=x, coordY =y} | x<-[0..w-1], y<-[0..h-1]]

hazardsOnBoard :: [Hazard] -> FullBoard -> FullBoard
hazardsOnBoard hs (FullBoard m) = FullBoard b
  where
    f = (<|> (Just [HazCell])) . fmap (HazCell:)
    b = foldr (\(Hazard x) n -> Map.alter f x n) m hs

foodOnBoard :: [Food] -> FullBoard -> FullBoard
foodOnBoard fs (FullBoard m) = FullBoard b 
  where
    f = (<|> (Just [FoodCell])) . fmap (FoodCell:) 
    b = foldr (\(Food x) n -> Map.alter f x n) m fs

snakesOnBoard :: [Snake] -> FullBoard -> FullBoard
snakesOnBoard s (FullBoard m)= FullBoard b
  where
    b = foldr Map.union m f
    f = map snakeOnBoard s

snakeOnBoard :: Snake -> Map.Map Coord [CellContents]
snakeOnBoard s = b
  where
    i = snakeId s
    h = Map.insert (snakeHead s) ([SnakeCell i Head]) Map.empty
    t = Map.alter 
      ((<|> (Just [SnakeCell i Tail])) . fmap (SnakeCell i Tail:))
      (last $ snakeBody s)
      h
    f = 
      ((<|> (Just [SnakeCell i Body])) . fmap (SnakeCell i Body:))
    b =
      foldr
        (\x m -> Map.alter f x m) t
        (tail $ init $ snakeBody s)

simulateHead :: Snake -> [Coord]
simulateHead s = filter (/=n) [
  (Coord {coordX = x+1, coordY = y}),
  (Coord {coordX = x-1, coordY = y}),
  (Coord {coordX = x, coordY = y+1}),
  (Coord {coordX = x, coordY = y-1})
  ] 
  where
    (Coord x y) = snakeHead s
    n = (snakeBody s) !! 1

simulateMove :: Snake -> Coord -> Snake
simulateMove s@(Snake {snakeHead=h, snakeBody=b}) c = newSnake
  where
  (Coord x y) = h
  (m, n) = case (coordsToDirection h c) of
    DUp -> (0, 1)
    DDown -> (0, (-1))
    DLeft -> ((-1), 0)
    DRight -> (1, 0)
  newHead = updateCoord h m n
  oldBody = snakeBody s
  newBody = newHead:(init oldBody)
  newSnake = s { snakeHead = newHead, snakeBody = newBody}

updateCoord :: Coord -> Int -> Int -> Coord
updateCoord (Coord x y) x1 y1 = Coord {coordX = x+x1, coordY = y+y1}

--simulateSnakes :: [Snakes] -> 