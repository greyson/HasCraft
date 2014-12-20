module Main where

import Control.Applicative ( (<$>) )
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Database.LevelDB (runResourceT)

import Data.NBT.MCPE
import Database.MCPE

path = "My World/db"

-- DB.open ::

getPlayer :: IO (Maybe NBT)
getPlayer = runResourceT $ open path >>= getNbt LocalPlayer
