module Data.Minecraft where

import Data.Text

import Data.Minecraft.Common
import Data.Minecraft.Entity
import Data.Minecraft.Chunk

data GameType = Survival | Creative
              deriving (Show, Enum, Eq)

data Dimension = Overworld
               deriving (Show, Enum, Eq)

-- This is kind of important.
newtype Generator = Generator {
  generation :: World -> Int -> Int -> Chunk
  }

data World = World { dimension :: Dimension
                   , gameType :: GameType
                   , generator :: Generator
                   , lastPlayed :: Integer
                   , levelName :: Text
                   , limitedWorldOrigin :: Maybe Position
                   , _platform :: Int -- unknown, android/ios/raspi?
                   , seed :: Integer
                   , _size :: Integer -- appears to be 0
                   , _storageVersion :: Int -- 4?
                   , worldSpawn :: Position
                   , time :: Ticks
                   , spawnMobs :: Bool
                   }
