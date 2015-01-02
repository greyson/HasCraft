module Data.Pocketmine.Common where

-- 20 ticks = 1 second
type Ticks = Integer

data Position = Position { x :: Float
                         , y :: Float
                         , z :: Float
                         }
                deriving (Show, Ord, Eq)

data Rotation = Rotation { yaw :: Float
                         , pitch :: Float
                         }
                deriving (Show, Eq)

-- These colors appear to be universal (wool, carpet (duh), clay, etc)
data Color = White     | Orange | Magenta | LightBlue
           | Yellow    | Lime   | Pink    | Gray
           | LightGray | Cyan   | Purple  | Blue
           | Brown     | Green  | Red     | Black
           deriving (Show, Enum, Eq, Ord)
