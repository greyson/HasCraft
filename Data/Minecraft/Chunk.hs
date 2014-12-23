module Data.Minecraft.Chunk where

import Control.Applicative ( (<$>) )
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Maybe (fromJust)
import Data.Minecraft.Entity
import Data.Minecraft.Block
import Database.MCPE (Database, dbGet, Key(..), ChunkType(..) )

import Data.ByteString (ByteString, index)
import qualified Data.ByteString.Lazy as BL

type Terrain = ByteString

getBlock :: Int -> Int -> Int -> Terrain -> BlockType
getBlock x y z ter =
  toEnum $ fromIntegral $ ter `index` (coordToIndex x y z)

coordToIndex x y z = (128 * 16 * x) + (128 * z) + y

data Chunk = Ungenerated
           | Chunk { east  :: Int -- The most easterly coordinate of this chunk
                   , north :: Int -- The most northerly coordinate of this chunk
                   , terrain :: Terrain
                   , entities :: [Entity]
                   }
           deriving Show

south = (16+) . north
west  = (16+) . east

-- TODO: Remove 'fromJust' to become NotFound or somesuch,
-- then use the generator to make the next one.
loadChunk x z db = do
  wholeTerrain <- dbGet (Key x z TerrainData) db
  return $ case wholeTerrain of
    Nothing -> Ungenerated
    Just terrain -> Chunk { east = z * 16
                          , north = x * 16
                          , terrain = (BL.toStrict $ BL.take (16*16*128) terrain)
                          , entities = []
                          }
