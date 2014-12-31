module Main where


import Criterion.Main
import Data.Minecraft.Block

decodingN :: Int -> [BlockType]
decodingN i = map toEnum [0..i]

isUnknown (Unknown _) = True
isUnknown _ = False

main = defaultMain [
  bgroup "decode" [ bench "10"  $ whnf (length . (map isUnknown) . decodingN) 10
                  , bench "all" $ whnf (length . (map isUnknown) . decodingN) 255
                  ]
  ]
