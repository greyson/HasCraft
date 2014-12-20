module Main where

import Control.Applicative ( (<$>) )
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Database.LevelDB (runResourceT)

import Data.NBT.MCPE
import Database.MCPE

path = "My World/db"

-- DB.open ::

{-
getPlayer :: IO (NBT.NBT)
getPlayer = print $ runResourceT $ do
  DB.open path >>= DB.getNbt DB.LocalPlayer
-}
