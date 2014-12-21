module Data.Minecraft.Chunk where

import Data.Array.ST

import Data.Minecraft.Entity
import Data.Minecraft.Block

data Terrain = Terrain

data Chunk = Chunk { terrain :: Terrain
                   , entities :: [Entity]
                   }