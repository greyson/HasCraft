module Database.MCPE where

import Prelude hiding (put)

import Control.Applicative ( (<$>), (<*>) )
import Control.Exception (bracket)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Data.Binary (put, putWord8, getWord8, decode, encode, Binary(..) )
import Data.Binary.Put (runPut, putWord32le, putByteString)
import Data.Binary.Get (getWord32le, lookAheadM)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.ByteString.Char8 (pack)
import Data.Int (Int64(..))
import Data.Maybe (fromJust)
import Data.NBT.MCPE (NBT(..), readNbt)
import Data.Word (Word8(..), Word32(..))
import Database.LevelDB (withIterator, defaultReadOptions,
                         runResourceT,
                         Compression(..), getProperty, Options(..) )

import qualified Database.LevelDB as DB
import qualified Database.LevelDB.Streaming as S
import qualified Data.ByteString as B

defaultOptions = DB.defaultOptions{ compression = Zlib }

open path = DB.open path defaultOptions

getNbt key db = do
  value <- dbGet key db
  return $ readNbt <$> value

dbGet :: DB.MonadResource m => Key -> DB.DB -> m (Maybe ByteString)
dbGet key db = do
  let binkey = runPut $ put key
  value <- DB.get db defaultReadOptions (toStrict binkey)
  return $ fromStrict <$> value


getDbProperty path p = runResourceT $ do
  db <- open path
  getProperty db p

data Key = Key Word32 Word32 BlockType
         | LocalPlayer
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
  put LocalPlayer = putByteString (pack "~local_player")
  put (Key x z t) = do
    putWord32le x
    putWord32le z
    put t

  get = do
    loc <- lookAheadM getLocKey
    case loc of
      Just v -> return v
      Nothing -> return LocalPlayer


getLocKey = do
    x <- getWord32le
    z <- getWord32le
    t <- Data.Binary.get
    return $ if (t == (Unknown 0x6C))
             then Nothing
             else Just $ Key x z t

getAllKeys :: FilePath -> IO [Key]
getAllKeys path = runResourceT $ do
  db <- open path
  withIterator db defaultReadOptions $ \it -> do
    keys <- S.toList $ S.keySlice it S.AllKeys S.Asc
    let decodeKey x = decode (fromStrict x) :: Key
    return $ map decodeKey keys

isUnknownType (Key x z (Unknown _)) = True
isUnknownType _ = False

getUnknownKeys :: FilePath -> IO [Key]
getUnknownKeys path =
  filter isUnknownType `fmap` getAllKeys path
