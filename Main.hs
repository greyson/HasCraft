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

getChunk0Terrain db = loadChunk 0 0 db

getBlockColumn x z ter =
  map (\y -> getBlock x y z ter) [0..127]

getChunkLayer y ter =
  map (\(x,z) -> getBlock x y z) $ cartProd [0..15] [1..15]
  where
    cartProd xs ys = [(x,y) | x <- xs, y <- ys]

main = runResourceT $ do
  db <- open path
  -- remember to load the chunk only once; re-fetching is terribly slow.
  chunk0terrain <- terrain <$> getChunk0Terrain db

  let columns = map (\i -> getBlockColumn 0 i chunk0terrain) [0..127]
  liftIO $ print (last columns)
  liftIO $ putStrLn (show (length columns) ++ " columns")
