module Main where

import Data.Word
import Data.Bits
import Debug.Trace

positive, negtive :: [Word8]
positive = [0..0x78]    -- [0..2^(k+t)-2^t]
negtive  = [0x80..0xf8] -- [2^(k+t)..2^(k+t+1)-2^t]

main = writeFile "float.csv" $ unlines $ map (toCSV . toFloat) (positive ++ negtive)
    where 
      toCSV (x,y) = show x ++ ", " ++ show y
      toFloat w = (w,uint8ToFloat w)

uint8ToFloat :: Word8 -> Double
uint8ToFloat w 
    | 0 <= w && w <= 7       = fromIntegral w / 512
    | 8 <= w && w <= 0x77    = normal (shiftR (w .&. 0x7f) 3) (w .&. 0x7)
    | w == 0x78              = inf
    | 0x80 <= w && w <= 0x87 = (-fromIntegral (w .&. 0x7f) / 512)
    | 0x88 <= w && w <= 0xf7 = (-normal (shiftR (w .&. 0x7f) 3) (w .&. 0x7))
    | w == 0xf8              = (-inf)
    where
      normal e m = (fromIntegral m / 8 + 1) * (2 ^^ (fromIntegral e-7))
      inf = 500