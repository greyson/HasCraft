module Data.Minecraft.Chunk where

import Control.Applicative ( (<$>) )
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.ByteString (ByteString, index)
import Data.Maybe (fromJust)

import Data.Minecraft.Entity
import Data.Minecraft.Block

import qualified Data.ByteString.Lazy as BL

type Terrain = ByteString

getBlock :: Int -> Int -> Int -> Terrain -> BlockType
getBlock x y z ter =
  toEnum $ fromIntegral $ ter `index` (coordToIndex x y z)

coordToIndex x y z = (128 * 16 * x) + (128 * z) + y

data Chunk = Ungenerated { east :: Int
                         , north :: Int
                         }
           | Chunk { east  :: Int -- The most easterly coordinate of this chunk
                   , north :: Int -- The most northerly coordinate of this chunk
                   , terrain :: Terrain
                   , entities :: [Entity]
                   }
           deriving Show

south = (16+) . north
west  = (16+) . east
