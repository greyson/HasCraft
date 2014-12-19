module MCPE.Map.Database where

import Prelude hiding (put)

import Control.Exception (bracket)
import Data.Binary (put, putWord8, getWord8, decode, encode, Binary(..) )
import Data.Binary.Put (runPut, putWord32le, putByteString)
import Data.Binary.Get (getWord32le, lookAheadM)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.Char8 (pack)
import Data.Int (Int64(..))
import Data.Word (Word8(..), Word32(..))
import Database.LevelDB (open, withIterator, defaultReadOptions,
                         runResourceT,
                         Compression(..), getProperty, Options(..) )
import qualified Database.LevelDB as DB
import qualified Database.LevelDB.Streaming as S
import qualified Data.ByteString as B

path = "My World/db" -- Testing only

defaultOptions = DB.defaultOptions{ compression = Zlib }

getDbProperty p = runResourceT $ do
  db <- open path defaultOptions
  getProperty db p

data Key = Key Word32 Word32 BlockType
         | LocalUser
         deriving (Show, Eq)

data BlockType = Terrain
               | TileEntity
               | Entity
               | OneByte
               | Unknown Word8
               deriving (Show, Eq)

instance Binary BlockType where
  put Terrain     = putWord8 0x30
  put TileEntity  = putWord8 0x31
  put Entity      = putWord8 0x32
  put OneByte     = putWord8 0x76
  put (Unknown b) = putWord8 b
  get = do
    byte <- getWord8
    return $ case byte of
      0x30  -> Terrain
      0x31  -> TileEntity
      0x32  -> Entity
      0x76  -> OneByte
      other -> Unknown other

instance Binary Key where
  put LocalUser = putByteString (pack "~local_player")
  put (Key x z t) = do
    putWord32le x
    putWord32le z
    put t

  get = do
    loc <- lookAheadM getLocKey
    case loc of
      Just v -> return v
      Nothing -> return LocalUser


getLocKey = do
    x <- getWord32le
    z <- getWord32le
    t <- get
    return $ if (t == (Unknown 0x6C))
             then Nothing
             else Just $ Key x z t

getAllKeys :: IO [Key]
getAllKeys = runResourceT $ do
  db <- open path defaultOptions
  withIterator db defaultReadOptions $ \it -> do
    keys <- S.toList $ S.keySlice it S.AllKeys S.Asc
    let decodeKey x = decode (fromStrict x) :: Key
    return $ map decodeKey keys

isUnknownType (Key x z (Unknown _)) = True
isUnknownType _ = False

getUnknownKeys :: IO [Key]
getUnknownKeys =
  filter isUnknownType `fmap` getAllKeys
