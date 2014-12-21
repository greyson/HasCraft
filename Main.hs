module Main where

import Control.Applicative ( (<$>) )
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
