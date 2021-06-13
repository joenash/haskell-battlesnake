module Main (main) where

import Battlesnake.Lib
import Battlesnake.Types
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty

snakeInfo :: SnakeInfo
snakeInfo =
  SnakeInfo
    { siAPI = "1",
      siAuthor = Just "joenash",
      siColor = mkColor "#000000",
      siHead = Just "sand-worm",
      siTail = Just "bolt",
      siVersion = Just "0.0.1"
    }

info :: ScottyM ()
info = get "/" $ json snakeInfo

start :: ScottyM ()
start = post "/start" $ html "start"

movePost :: ScottyM ()
movePost = post "/move" $ do
  b <- body
  let parsedBody = A.decode b :: Maybe MoveReq
  case parsedBody of
    Nothing -> json $ mkMove DDown "I FAIL"
    Just moveReq -> do
      let parsedBoard = buildBoard (mrBoard moveReq)
      let you = mrSnake moveReq
      -- liftIO $ print $ parsedBoard
      json $
        coordsToMove (snakeHead you) $
          head $
            checkSurroundings parsedBoard you

checkSurroundings :: FullBoard -> Snake -> [Coord]
checkSurroundings fb s = emptycoords
  where
    h = snakeHead s
    x = coordX h
    y = coordY h
    surroundings =
      [ (Coord {coordX = x + 1, coordY = y}),
        (Coord {coordX = x -1, coordY = y}),
        (Coord {coordX = x, coordY = y + 1}),
        (Coord {coordX = x, coordY = y - 1})
      ]
    emptycoords =
      filter (\c -> coordIsEmpty fb c || coordIsFood fb c) surroundings

coordIsEmpty :: FullBoard -> Coord -> Bool
coordIsEmpty (FullBoard m) c = case cell of
  Nothing -> False
  Just x -> case x of
    [] -> True
    _ -> False
  where
    cell = Map.lookup c m

coordIsFood :: FullBoard -> Coord -> Bool
coordIsFood (FullBoard m) c = case cell of
  Nothing -> False
  Just x -> case x of
    [] -> False
    [FoodCell] -> True
    _ -> False
  where
    cell = Map.lookup c m

end :: ScottyM ()
end = post "/end" $ html "end"

main :: IO ()
main =
  scotty 3000 $
    do
      middleware logStdoutDev
      info
      >> start
      >> movePost
      >> end
