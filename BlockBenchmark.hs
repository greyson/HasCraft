module Main where

import Control.DeepSeq (NFData(..))
import Criterion.Main
import Data.Minecraft.Block

instance NFData BlockType

decodingN :: Int -> [BlockType]
decodingN i = map toEnum [0..i]

isUnknown (Unknown _) = True
isUnknown _ = False

main = defaultMain [
  bgroup "decode" [ bench "10"  $ nf decodingN 10
                  , bench "all" $ nf decodingN 65535
                  ]
  ]
