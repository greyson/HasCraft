{-# LANGUAGE FlexibleInstances #-}
module Minecraft.NBT where

import Data.Binary (Binary(..))
import Data.Binary.Get (Get(..))
import Data.Binary.Put (Put(..), runPut,
                        putWord8, putWord16be, putWord32be, putWord64be, putByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map (Map(..))
import Data.ReinterpretCast
import Data.Typeable (cast)
import Data.Word (Word8, Word16, Word32, Word64)
import Codec.Binary.UTF8.String (encode)

data NBT = NBTByte Int8
         | NBTShort Int16
         | NBTInt Int32
         | NBTLong Int64
         | NBTString String
         | NBTList [NBT]
         | NBTCompound [NBTNamed]

newtype NBTNamed = NBTNamed (String, NBT)

class Taggable a where
  tagType :: a -> Word8
  putNamed :: Binary a => String -> a -> Put
  putNamed s t = do
    putWord8 $ tagType t
    put (NBTString s)
    put t

class PrettyPrintable a where
  showPretty :: Int -> a -> String

showEntryCount 1 = "1 entry"
showEntryCount n = show n ++ " entries"

showIndent n = take (3 * n) $ cycle " "

instance PrettyPrintable NBT where
  showPretty indent (NBTCompound l) =
    let showNext x = showPretty (succ indent) x ++ "\n"
    in showEntryCount (length l) ++ "\n"
       ++ showIndent indent ++ "{\n"
       ++ concat (map showNext l)
       ++ showIndent indent ++ "}"

instance PrettyPrintable NBTNamed where
  showPretty indent other = showIndent indent ++ show other


instance Show NBTNamed where
  show (NBTNamed (name, nbt)) = tagName nbt ++ "(" ++ show name ++ "): " ++ show nbt

instance Show NBT where
  show (NBTByte i) = show i
  show (NBTShort s) = show s
  show (NBTInt a) = show a
  show (NBTString s) = show s
  show x = showPretty 0 x

tagName (NBTByte _) = "TAG_Byte"
tagName (NBTShort _) = "TAG_Short"
tagName (NBTInt _) = "TAG_Int"
tagName (NBTLong _) = "TAG_Long"
tagName (NBTString _) = "TAG_String"
tagName (NBTList _) = "TAG_List"
tagName (NBTCompound _) = "TAG_Compound"


instance Taggable NBT where
  tagType (NBTByte a) = 1
  tagType (NBTShort a) = 2
  tagType (NBTInt a) = 3
  tagType (NBTLong a) = 4
  tagType (NBTString a) = 8

  tagType (NBTList a) = 9
  tagType (NBTCompound a) = 10

instance Binary NBT where
  put (NBTByte a) = putWord8 $ fromIntegral a
  put (NBTShort a) = putWord16be $ fromIntegral a
  put (NBTInt a) = putWord32be $ fromIntegral a
  put (NBTLong a) = putWord64be $ fromIntegral a
  put (NBTString s) = do
    putWord32be $ fromIntegral (length s)
    foldl (>>) (return ()) . (map putWord8) $ encode s
  put (NBTList l) = do
    putWord8 $ tagType (head l)
    putWord32be $ fromIntegral (length l)
    foldl (>>) (return ()) $ map put l
  put (NBTCompound m) = undefined

  -- get can only be used for named stuff
  get = undefined
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