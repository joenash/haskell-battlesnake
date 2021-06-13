module Battlesnake.Types where

import Data.Aeson
import Data.Char (isAlphaNum)
import qualified Data.Text as Text
import GHC.Generics (Generic)

newtype HexRGB = HexRGB {unColor :: Text.Text}
  deriving (Eq, Generic, Show)

instance ToJSON HexRGB where
  toJSON = genericToJSON defaultOptions {unwrapUnaryRecords = True}

mkColor :: Text.Text -> Maybe HexRGB
mkColor color
  | Text.length color /= 7 = Nothing
  | Text.head color /= '#' = Nothing
  | Text.any (not . isAlphaNum) (Text.tail color) = Nothing
  | Text.any (> 'f') $ Text.toLower $ Text.tail color = Nothing
  | otherwise = Just HexRGB {unColor = color}

data SnakeInfo = SnakeInfo
  { siAPI :: Text.Text,
    siAuthor :: Maybe Text.Text,
    siColor :: Maybe HexRGB,
    siHead :: Maybe Text.Text,
    siTail :: Maybe Text.Text,
    siVersion :: Maybe Text.Text
  }
  deriving (Generic, Show)

instance ToJSON SnakeInfo where
  toJSON (SnakeInfo api author color shead stail vers) =
    object
      [ "apiversion" .= api,
        "author" .= author,
        "color" .= color,
        "head" .= shead,
        "tail" .= stail,
        "version" .= vers
      ]

data Game = Game
  { gameId :: Text.Text,
    gameRuleset :: Maybe Ruleset,
    gameTimeout :: Int
  }
  deriving (Generic, Show)

instance FromJSON Game where
  parseJSON = withObject "Game" $ \v ->
    Game
      <$> v .: "id"
      <*> v .:? "ruleset"
      <*> v .: "timeout"

data Ruleset = Ruleset
  { ruleName :: Text.Text,
    ruleVersion :: Text.Text
  }
  deriving (Generic, Show)

instance FromJSON Ruleset where
  parseJSON = withObject "Ruleset" $ \v ->
    Ruleset
      <$> v .: "name"
      <*> v .: "version"

{- Desugared, kept here for reference
instance FromJSON Ruleset where
  parseJSON = withObject "Ruleset" $ \obj -> do
    name <- obj .: "name"
    version <- obj .: "version"
    return (Ruleset {ruleName = name, ruleVersion = version})
-}

data Board = Board
  { boardHeight :: Int,
    boardWidth :: Int,
    boardFood :: [Food],
    boardHazards :: [Hazard],
    boardSnakes :: [Snake]
  }
  deriving (Show)

instance FromJSON Board where
  parseJSON = withObject "Board" $ \v ->
    Board
      <$> v .: "height"
      <*> v .: "width"
      <*> v .: "food"
      <*> v .: "hazards"
      <*> v .: "snakes"

data Coord = Coord
  { coordX :: Int,
    coordY :: Int
  }
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Coord where
  parseJSON = withObject "Coord" $ \v ->
    Coord
      <$> v .: "x"
      <*> v .: "y"

newtype Food = Food Coord
  deriving (Show, FromJSON)

newtype Hazard = Hazard Coord
  deriving (Show, FromJSON)

data Snake = Snake
  { snakeId :: Text.Text,
    snakeName :: Text.Text,
    snakeHealth :: Int,
    snakeBody :: [Coord],
    snakeLatency :: Text.Text,
    snakeHead :: Coord,
    snakeLength :: Int,
    snakeShout :: Text.Text,
    snakeSquad :: Maybe Text.Text
  }
  deriving (Show)

instance FromJSON Snake where
  parseJSON = withObject "Snake" $ \v ->
    Snake
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "health"
      <*> v .: "body"
      <*> v .: "latency"
      <*> v .: "head"
      <*> v .: "length"
      <*> v .: "shout"
      <*> v .:? "squad"

data MoveReq = MoveReq
  { mrGame :: Game,
    mrTurn :: Int,
    mrBoard :: Board,
    mrSnake :: Snake
  }
  deriving (Generic, Show)

instance FromJSON MoveReq where
  parseJSON = withObject "MoveReq" $ \v ->
    MoveReq
      <$> v .: "game"
      <*> v .: "turn"
      <*> v .: "board"
      <*> v .: "you"

data Direction = DUp | DDown | DLeft | DRight
  deriving (Eq, Show)

data Move = Move
  { move :: Direction,
    shout :: Text.Text
  }
  deriving (Show, Generic)

instance ToJSON Move where
  toJSON Move {move = move, shout = shout} =
    object
      [ "move" .= move',
        "shout" .= shout
      ]
    where
      move' = case move of
        DUp -> "up" :: Text.Text
        DDown -> "down" :: Text.Text
        DLeft -> "left" :: Text.Text
        DRight -> "right" :: Text.Text