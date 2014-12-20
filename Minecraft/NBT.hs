{-# LANGUAGE FlexibleInstances #-}
module Minecraft.NBT where

import Control.Applicative ( (<$>) )
import Data.Binary (Binary(..))
import Data.Binary.Get (Get(..), runGet, runGetOrFail, ByteOffset,
                        getWord8, getWord16le, getWord32le, getWord64le, getByteString)
import Data.Binary.Put (Put(..), runPut,
                        putWord8, putWord16le, putWord32le, putWord64le, putByteString)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map (Map(..))
import Data.ReinterpretCast
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Typeable (cast)
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Data.Text as T

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

data NBT = NBT T.Text NBTPayload
         deriving (Show, Eq)

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
getByType StringType = do
  StringTag <$> getStringValue
getByType ListType = do
  typ <- get
  len <- fromIntegral <$> getWord32le
  ListTag typ <$> (sequence $ take len $ repeat (getByType typ))
getByType CompoundType = CompoundTag <$> getToEnd
  where
    getToEnd = do
      typ <- get
      if typ == EndType
        then return []
        else do
        name <- getStringValue
        payload <- getByType typ
        ((NBT name payload):) <$> getToEnd
getByType IntArrayType = do
  len <- fromIntegral <$> getWord32le
  IntArrayTag <$> (sequence $ take len $
                   repeat (fromIntegral <$> getWord32le))

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

{-
 - SECOND DRAFT BELOW
 -
 -    I'm just saving it for the pretty printing later.
 -}


class Taggable a where
  tagType :: a -> Word8

class NBTElement a where
  toNbt :: a -> NBT

data NBT' = NBTEnd
         | NBTByte Int8
         | NBTShort Int16
         | NBTInt Int32
         | NBTLong Int64
         | NBTFloat Float
         | NBTDouble Double
         | NBTByteArray ByteString
         | NBTString String
         | NBTList [NBT']
         | NBTCompound [NBTNamed]
         | NBTIntList [Int32]

newtype NBTNamed = NBTNamed (String, NBT')

instance Taggable () where
  tagType _ = 0
instance Taggable Int8 where
  tagType _ = 1
instance Taggable Int16 where
  tagType _ = 2
instance Taggable Int32 where
  tagType _ = 3
instance Taggable Int64 where
  tagType _ = 4
instance Taggable Float where
  tagType _ = 5
instance Taggable Double where
  tagType _ = 6
instance Taggable ByteString where
  tagType _ = 7
instance Taggable String where
  tagType _ = 8
instance Taggable a => Taggable [a] where
  tagType _ = 9
instance Taggable [NBTNamed] where
  tagType _ = 10


getTag :: Taggable a => a -> Word8
getTag a = tagType a


class PrettyPrintable a where
  showPretty :: Int -> a -> String

showEntryCount 1 = "1 entry"
showEntryCount n = show n ++ " entries"

showIndent n = take (3 * n) $ cycle " "

instance Show NBT' where
  show (NBTByte i) = show i
  show (NBTShort s) = show s
  show (NBTInt a) = show a
  show (NBTString s) = show s
  show x = showPretty 0 x

instance PrettyPrintable NBT' where
  showPretty indent (NBTCompound l) =
    let showNext x = showPretty (succ indent) x ++ "\n"
    in showEntryCount (length l) ++ "\n"
       ++ showIndent indent ++ "{\n"
       ++ concat (map showNext l)
       ++ showIndent indent ++ "}"

  showPretty indent (NBTList l) =
    let showElem x =
          tagName x ++ "(): "
          ++ showPretty (succ indent) x ++ "\n"
    in showEntryCount (length l) ++ "\n"
       ++ showIndent indent ++ "[\n"
       ++ concat (map showElem l)
       ++ showIndent indent ++ "]"
  showPretty indent other =
    showIndent indent ++ show other

instance Show NBTNamed where
  show (NBTNamed (name, nbt)) = tagName nbt ++ "(" ++ show name ++ "): " ++ show nbt

instance PrettyPrintable NBTNamed where
  showPretty indent other = showIndent indent ++ show other

tagName (NBTByte _) = "TAG_Byte"
tagName (NBTShort _) = "TAG_Short"
tagName (NBTInt _) = "TAG_Int"
tagName (NBTLong _) = "TAG_Long"
tagName (NBTString _) = "TAG_String"
tagName (NBTList _) = "TAG_List"
tagName (NBTCompound _) = "TAG_Compound"


{-
tagType (NBTByte _) = 1
tagType (NBTShort _) = 2
tagType (NBTInt _) = 3
tagType (NBTLong _) = 4
tagType (NBTFloat _) = 5
tagType (NBTDouble _) = 6
tagType (NBTByteArray _) = 7
tagType (NBTString _) = 8
tagType (NBTCompound _) = 9

instance Binary Tag where
  get = undefined
  put (NBTByte b) = putWord8 $ fromIntegral b
  put (NBTShort s) = putWord16be $ fromIntegral s
  put (NBTInt i) = putWord32be $ fromIntegral i
  put (NBTLong l) = putWord64be $ fromIntegral l
  put (NBTFloat f) = putWord32be (floatToWord f)
  put (NBTDouble d) = putWord64be (doubleToWord d)
  put (NBTByteArray a) = do putWord32be $ fromIntegral (length a)
                            putByteString a
  put (NBTCompound m) = do
    putWord8 (tagType (NBTCompound m))
-}

{- | Define types which can act as the payload in NBT values.
 -}
class Payloadable a where
  payload :: a -> Put
  payunload :: Get a

instance Payloadable Int8 where
  payload = putWord8 . fromIntegral
  payunload = fromIntegral `fmap` getWord8
instance Payloadable Int16 where
  payload = putWord16le . fromIntegral
  payunload = fromIntegral `fmap` getWord16le
instance Payloadable Int32 where
  payload = putWord32le . fromIntegral
  payunload = fromIntegral `fmap` getWord32le
instance Payloadable Int64 where
  payload = putWord64le . fromIntegral
  payunload = fromIntegral `fmap` getWord64le
instance Payloadable Float where
  payload = putWord32le . floatToWord
  payunload = wordToFloat `fmap` getWord32le
instance Payloadable Double where
  payload = putWord64le . doubleToWord
  payunload = wordToDouble `fmap` getWord64le
instance Payloadable ByteString where
  payload a = do
    putWord32le $ fromIntegral (BL.length a)
    putByteString $ toStrict a
  payunload = do
    bytelen <- fromIntegral `fmap` getWord32le
    fromStrict `fmap` getByteString bytelen
instance Payloadable String where
  payload s = do
    let bs = UTF8.fromString s
    putWord16le $ fromIntegral (BL.length bs)
    putByteString $ toStrict bs
  payunload = do
    bytelen <- fromIntegral `fmap` getWord16le
    getByteString bytelen >>= return . UTF8.toString . fromStrict

newtype NBTFile = NBTFile (String, [NBTNamed])

instance Show NBTFile where
  show (NBTFile (name, l)) = showPretty 0 (NBTNamed (name, NBTCompound l))

instance Binary NBTFile where
  put = undefined
  get = do
    10 <- getWord8
    name <- payunload
    all <- getNBTNamed []
    -- Parse the rest
    return $ NBTFile (name, all)

getList :: Get [NBT']
getList = do
  t <- getWord8
  len <- fromIntegral `fmap` (payunload :: Get Int32)
  value <- sequence $ take len $ repeat (getNBT t)
  return value

getNBT :: Word8 -> Get NBT'
getNBT t = case t of
      1 -> fmap NBTByte payunload
      2 -> fmap NBTShort payunload
      3 -> fmap NBTInt payunload
      4 -> fmap NBTLong payunload
      5 -> fmap NBTFloat payunload
      6 -> fmap NBTDouble payunload
      7 -> fmap NBTByteArray payunload
      8 -> fmap NBTString payunload
      9 -> fmap NBTList getList
      10 -> return $ NBTCompound [] -- TODO: testing
      11 -> return $ NBTList []

getNBTNamed :: [NBTNamed] -> Get [NBTNamed]
getNBTNamed prev = do
  t <- getWord8
  if t == 0
    then return prev
    else do
    name <- payunload :: Get String
    value <- getNBT t
    getNBTNamed (prev ++ [NBTNamed (name, value)])


nbtGet :: Get NBT
nbtGet = get