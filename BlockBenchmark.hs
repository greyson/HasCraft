module Main where


import Criterion.Main
import Data.Minecraft.Block

decodingN :: Int -> [BlockType]
decodingN i = map toEnum [0..i]

main = defaultMain [
  bgroup "decode" [ bench "10"  $ whnf decodingN 10
                  , bench "all" $ whnf decodingN 255
                  ]
  ]
