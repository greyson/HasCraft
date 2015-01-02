{-# LANGUAGE FlexibleInstances #-}
module Data.NBT.MCPE ( NBT

                       -- manipulating
                     , lookup
                     , value
                     , (</>)

                       -- dealing with payloads
                     , TagType, NBTPayload(..)
                     , asIntegral, asFloat, asDouble
                     , asPosition, asRotation

                       -- loading from binary
                     , readDat
                     , readNbt
                     ) where

import Prelude hiding (lookup, (/))

import Control.Applicative ( (<$>) )
import Control.Monad (replicateM)
import Data.Binary (Binary(..))
import Data.Binary.Get (Get(..), runGet,
                        getWord8, getWord16le, getWord32le, getWord64le, getByteString)
import Data.Binary.Put (Put(..),
                        putWord8, putWord16le, putWord32le, putWord64le, putByteString)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map (Map(..))
import Data.ReinterpretCast (wordToFloat, wordToDouble)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Printf (printf)

import Data.Minecraft.Common

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

data TagType = EndType
             | ByteType
             | ShortType
             | IntType
             | LongType
             | FloatType
             | DoubleType
             | ByteArrayType
             | StringType
             | ListType
             | CompoundType
             | IntArrayType
             deriving (Show, Eq, Enum)

instance Binary TagType where
  get = (toEnum . fromIntegral) `fmap` getWord8
  put = putWord8 . fromIntegral . fromEnum

value :: NBT -> NBTPayload
value (NBT _ pay) = pay

asPosition :: NBTPayload -> Position
asPosition (ListTag _ [FloatTag x, FloatTag y, FloatTag z]) =
  Position x y z

asRotation :: NBTPayload -> Rotation
asRotation (ListTag _ [FloatTag yaw, FloatTag pitch]) =
  Rotation yaw pitch

asIntegral :: Integral a => NBTPayload -> a
asIntegral (ByteTag x)  = fromIntegral x
asIntegral (ShortTag x) = fromIntegral x
asIntegral (IntTag x)   = fromIntegral x
asIntegral (LongTag x)  = fromIntegral x

asFloat (FloatTag x)  = x
asDouble (DoubleTag x) = x

lookup :: T.Text -> NBT -> [NBTPayload]
lookup key (NBT _ (CompoundTag l)) =
  map value $ filter (\(NBT t pay) -> t == key) l

class Searchable a where
  (</>) :: a -> T.Text -> Maybe NBTPayload

instance Searchable NBTPayload where
  (CompoundTag l) </> key =
    case map value (filter (\(NBT t _) -> t == key) l) of
      [] -> Nothing
      [v] -> Just v
  _ </> _ = Nothing

instance Searchable NBT where
  (NBT _ (CompoundTag l)) </> key = (CompoundTag l) </> key

instance Searchable a => Searchable (Maybe a) where
  Nothing </> _ = Nothing
  (Just x) </> key = x </> key

data NBT = NBT T.Text NBTPayload
         deriving (Eq)

data NBTPayload = EndTag
                | ByteTag Int8
                | ShortTag Int16
                | IntTag Int32
                | LongTag Int64
                | FloatTag Float
                | DoubleTag Double
                | ByteArrayTag ByteString
                | StringTag T.Text
                | ListTag TagType [NBTPayload]
                | CompoundTag [NBT]
                | IntArrayTag [Int32]
                deriving (Show, Eq)

getByType :: TagType -> Get NBTPayload
getByType ByteType = ByteTag . fromIntegral <$> getWord8
getByType ShortType = ShortTag . fromIntegral <$> getWord16le
getByType IntType = IntTag . fromIntegral <$> getWord32le
getByType LongType = LongTag . fromIntegral <$> getWord64le
getByType FloatType = FloatTag . wordToFloat <$> getWord32le
getByType DoubleType = DoubleTag . wordToDouble <$> getWord64le
getByType ByteArrayType = do
  len <- fromIntegral <$> getWord32le
  ByteArrayTag . fromStrict <$> getByteString len
getByType StringType = StringTag <$> getStringValue
getByType ListType = do
  typ <- get
  len <- fromIntegral <$> getWord32le
  ListTag typ <$> replicateM len (getByType typ)
getByType CompoundType = CompoundTag <$> getToEnd
  where
    getToEnd = do
      typ <- get
      if typ == EndType
        then return []
        else do
        name <- getStringValue
        payload <- getByType typ
        (NBT name payload:) <$> getToEnd
getByType IntArrayType = do
  len <- fromIntegral <$> getWord32le
  IntArrayTag <$> replicateM len (fromIntegral <$> getWord32le)

getStringValue :: Get T.Text
getStringValue = do
  len <- fromIntegral <$> getWord16le
  decodeUtf8 <$> getByteString len

instance Binary NBT where
  put = undefined
  get = do
    CompoundType <- get
    name <- getStringValue
    payload <- getByType CompoundType
    return $ NBT name payload

typeOf :: NBTPayload -> TagType
typeOf (ByteTag _) = ByteType
typeOf (ShortTag _) = ShortType
typeOf (IntTag _) = IntType
typeOf (LongTag _) = LongType
typeOf (FloatTag _) = FloatType
typeOf (DoubleTag _) = DoubleType
typeOf (ByteArrayTag _) = ByteArrayType
typeOf (StringTag _) = StringType
typeOf (ListTag _ _) = ListType
typeOf (CompoundTag _) = CompoundType
typeOf (IntArrayTag _) = IntArrayType

instance Show NBT where
  show = showPretty 0

class Show a => PrettyPrintable a where
  showPretty :: Int -> a -> String
  showPretty x v = indent x ++ showUnindented v ++ "\n"

  showUnindented :: a -> String
  showUnindented = show

instance PrettyPrintable NBT where
  showPretty ind (NBT name payload) =
    indent ind ++ show (typeOf payload)
    ++ "(" ++ show name ++ "): "
    ++ showPretty ind payload

instance PrettyPrintable NBTPayload where
  showPretty ind (CompoundTag l) =
    showEntryCount (length l) ++ "\n"
    ++ concatMap (showPretty (succ ind)) l

  showPretty _ (ByteTag b) = show b ++ "\n"
  showPretty _ (ShortTag s) = show s ++ "\n"
  showPretty _ (IntTag i) = show i ++ "\n"
  showPretty _ (LongTag l) = show l ++ "\n"
  showPretty _ (FloatTag f) = show f ++ "\n"
  showPretty _ (DoubleTag d) = show d ++ "\n"

  showPretty ind (ByteArrayTag b) =
    '\n' : prettyPrintBinary (indent $ succ ind) b

  showPretty _ (StringTag s) = show s ++ "\n"

  -- Treat lists of compound objects a bit differently.
  showPretty ind (ListTag CompoundType l) =
    showEntryCount (length l) ++ " [" ++ show CompoundType ++ "]\n"
    ++ concatMap showBareCompound l
    where
      showBareCompound c =
        indent (succ ind) ++ "{}: "
        ++ showPretty (succ ind) c

  -- Also lists of lists

  showPretty ind (ListTag t l) =
    showEntryCount (length l) ++ " [" ++ show t ++ "]\n"
    ++ showPretty (succ ind) l


  showPretty x v = indent x ++ show v ++ "\n"

instance PrettyPrintable a => PrettyPrintable [a] where
  showPretty ind l = indent ind ++ show l ++ "\n"

indent x = replicate (3 * x) ' '

showEntryCount 1 = "1 entry"
showEntryCount n = show n ++ " entries"

binGroupBy n l
  | BL.null l = []
  | otherwise =
    let (line, rest) = BL.splitAt n l
    in line:binGroupBy n rest

prettyPrintBinary :: String -> ByteString -> String
prettyPrintBinary prefix bin =
  (unlines $ map showBareLine (binGroupBy 40 bin))
    where
      showBareLine l = prefix ++ showGroups (binGroupBy 2 l)
      showGroups g = unwords $ map showHexidecimal g
      showHexidecimal bin
        | BL.null bin = []
        | otherwise = printf "%02x" (BL.head bin) ++ showHexidecimal (BL.tail bin)

data Dat = Dat Int NBT
         deriving Show

instance Searchable Dat where
  (Dat 4 (NBT _ (CompoundTag l))) </> key = (CompoundTag l) </> key


instance Binary Dat where
  put = undefined
  get = do
    4 <- getWord32le
    len <- getWord32le
    nbt <- get
    return $ Dat 4 nbt

readDat :: ByteString -> Dat
readDat = runGet get

readNbt :: ByteString -> NBT
readNbt = runGet get
