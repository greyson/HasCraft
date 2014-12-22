module Data.Minecraft.Chunk where

import Control.Applicative ( (<$>) )
import Data.Maybe (fromJust)
import Data.Minecraft.Entity
import Data.Minecraft.Block
import Database.MCPE (Database, dbGet, Key(..), ChunkType(..) )

import Data.ByteString (ByteString, index)
import qualified Data.ByteString.Lazy as BL

type Terrain = ByteString

getBlock :: Int -> Int -> Int -> Terrain -> BlockType
getBlock x y z ter =
  toEnum $ fromIntegral $ ter `index` (coordToInt x y z)

coordToInt x y z = (128 * 16 * x) + (128 * z) + y

data Chunk = Ungenerated
           | Chunk { terrain :: Terrain
                   , entities :: [Entity]
                   }
           deriving Show

-- TODO: Remove 'fromJust' to become NotFound or somesuch,
-- then use the generator to make the next one.
loadChunk x z db = do
  maybeTerrain <- dbGet (Key x z TerrainData) db
  return $ case maybeTerrain of
    Nothing -> Ungenerated
    Just terrain -> Chunk (BL.toStrict terrain) []
