module Minecraft.NBT where

import Prelude hiding (length)

import Data.Binary (Binary(..))
import Data.Binary.Get ()
import Data.Binary.Put (putWord8, putWord16be, putWord32be, putWord64be, putByteString)
import Data.ByteString (ByteString, length)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map (Map(..))
import Data.ReinterpretCast
import Data.Typeable (cast)
import Data.Word (Word8, Word16, Word32, Word64)

data Tag = NBTByte Int8
         | NBTShort Int16
         | NBTInt Int32
         | NBTLong Int64
         | NBTFloat Float
         | NBTDouble Double
         | NBTByteArray ByteString
         | NBTString String
         | NBTList [Tag]
         | NBTCompound (Map String Tag)
         deriving (Show)

castError :: String -> a -> b
castError name _ = error ("Tag::put can't cast " ++ name)

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
