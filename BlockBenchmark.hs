module Main where

import Control.DeepSeq (NFData(..))
import Criterion.Main
import Data.Minecraft.Block

instance NFData BlockType

decodingN :: Int -> [BlockType]
decodingN i = take i $ map toEnum (cycle [0..255])

isUnknown (Unknown _) = True
isUnknown _ = False

main = defaultMain [
  bgroup "decode" [ bench "init"  $ nf (toEnum::Int->BlockType) 255
                  , bench "cycle" $ nf decodingN 65535
                  ]
  ]
