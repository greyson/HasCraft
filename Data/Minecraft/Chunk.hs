module Data.Minecraft.Chunk where

import Data.Array.ST

import Data.Minecraft.Entity

data Terrain = Terrain

data Chunk = Chunk { terrain :: Terrain
                   , entities :: Entity a => [a]
                   }