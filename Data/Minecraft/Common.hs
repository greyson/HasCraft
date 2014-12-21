module Data.Minecraft.Common where

-- 20 ticks = 1 second
type Ticks = Integer

data Position = Position { x :: Float
                         , y :: Float
                         , z :: Float
                         }

data Rotation = Rotation { yaw :: Float
                         , pitch :: Float
                         }

-- These colors appear to be universal (wool, carpet (duh), clay, etc)
data Color = White     | Orange | Magenta | LightBlue
           | Yellow    | Lime   | Pink    | Gray
           | LightGray | Cyan   | Purple  | Blue
           | Brown     | Green  | Red     | Black
           deriving (Show, Enum, Eq, Ord)
