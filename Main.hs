module Main where

import Control.Applicative ( (<$>) )
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Database.LevelDB (runResourceT)

import Data.NBT.MCPE
import Database.MCPE
import Data.Minecraft.Entity
import Data.Minecraft.Chunk
import Data.Minecraft.Block

-- for interactive
import qualified Data.Text as T

path = "My World/db"

-- DB.open ::

getPlayer :: IO (Maybe NBT)
getPlayer = runResourceT $ open path >>= getNbt LocalPlayer

getChunk0Terrain db = runResourceT $ loadChunk 0 0 db

getBlockColumn x z db =
  sequence $ map (\i -> getBlock x i z <$> terrain <$> getChunk0Terrain db) [0..127]

main = runResourceT $ do
  db <- open path
  length <$> (sequence $ map (\i -> getBlockColumn 0 i db) [0..127])
