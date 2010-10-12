{-# OPTIONS -XScopedTypeVariables #-}
module Main where

import Data.Bits
import Numeric(showHex)
import Debug.Trace

-- 0, 1, 无关
data Bit = Z | S | U deriving (Eq, Show)
-- 配置包括指数域长与尾数域长
data Config = Config{ cfg_efields, cfg_mfields :: Int }
-- 二进制展开表示一个数
-- sign为符号, Z表示正数, S表示负数
-- bin_integer为整数部分,从高位到低位
-- bin_fraction为小数部分,无穷长
data Binary = Binary{ sign :: Bit, bin_integer, bin_fraction :: [Bit] }
-- IEEE754的数,定长的Bit序列
newtype IEEE754 = IEEE754 [Bit]

-- 将任意的实数转换为Binary
toBits   :: RealFrac a => a -> Binary

-- 根据配置将Binary转换为IEEE754表述的浮点数,丢失精度
encode :: Config -> Binary -> IEEE754

-- 给定配置下的最大正规格化数
max_positive_normal    :: Config -> IEEE754
-- 给定配置下的最小正规格化数
min_positive_normal    :: Config -> IEEE754
-- 给定配置下的最大非正规格化数
max_positive_subnormal :: Config -> IEEE754
-- 给定配置下的最小非正规格化数
min_positive_subnormal :: Config -> IEEE754
-- +/- 0
zero :: Config -> IEEE754
-- +/- 无穷
infinite :: Config -> IEEE754
-- 判断是否为NaN
is_NaN :: Config -> IEEE754 -> Bool



toBits num = let s = case signum num of{ -1 -> S; 0  -> U; 1  -> Z }
                 (i :: Integer,f) = properFraction (abs num)
             in  Binary s (reverse $ iToBin i) (fToBin f)
    where --整数部分到二进制,从低位到高位
          iToBin :: (Integral t, Bits t) => t -> [Bit]
          iToBin 0 = []
          iToBin i = let v = case i .&. 1 of
                               0 -> Z
                               1 -> S
                     in v : iToBin (shiftR i 1)
          -- 小数部分到二进制
          fToBin f = let (i',f') = properFraction (f*2)
                         v = case i' of 
                               0 -> Z
                               1 -> S
                     in v : fToBin f'

encode cfg (Binary s i f) = let (e,m) = if null i then caseA f else caseB i f
                            in IEEE754 ([s] ++ exp_and_significant e m)
    where -- 情况A:没有整数部分
          caseA f = let (zs,s) = span (== Z) $ take (t * 3) (f ++ repeat Z)
                    in if null s
                       -- 为0,则指数为-t-b,标准式(1.*)无定义
                       then (- (t + b), undefined)
                       -- 不为0,则指数为负的0比特数减一,标准式为s
                       else (- length zs - 1, s)
          -- 情况B:有整数部分,则指数为整数部分比特数减一,标准式为i++s
          caseB i f = (length i - 1, i++f)

          -- 偏移指数,确定尾数
          -- 若e < -t+1-b, 则移码为0
          -- 若e in [-t+1-b, -b],移码为 1 - off_exp
          -- 若e in [-b+1,b],移码为 e + b
          -- 否则移码为 2^n-1.
          -- ******************
          -- Fixme. 舍入是否正确?
          -- ******************
          exp_and_significant e m 
              | e < fst sn                = replicate (k + t) Z
              | fst sn <= e && e<= snd sn = let shift = - b - e
                                            in replicate k Z ++ replicate shift Z ++ take (t - shift) m
              | fst no <= e && e<= snd no = let efield = reverse $ take k $ toBits $ b + e
                                                mfield = take t (tail m)
                                            in efield ++ mfield
              | otherwise                 = replicate (k + t) S
              where -- 转换成Bit序列
                    toBits x = case (x .&. 1) of {0 -> Z; 1-> S} : toBits (shiftR x 1)
                    sn = (- t + 1 - b, - b)
                    no = (- b + 1, b)
          k  = cfg_efields cfg
          t  = cfg_mfields cfg
          b  = 2 ^ (k - 1) - 1

bit_value Z = 0
bit_value S = 1
bit_value U = 0
          
simple     = Config 4 3
ieee32bits = Config 8 23

max_positive_normal cfg = let k = cfg_efields cfg
                              t = cfg_mfields cfg
                          in IEEE754 ([Z] ++ replicate (k-1) S ++ [Z] ++ replicate t S)

min_positive_normal cfg = let k = cfg_efields cfg
                              t = cfg_mfields cfg
                          in IEEE754 ([Z] ++ replicate (k-1) Z ++ [S] ++ replicate t Z)

max_positive_subnormal cfg = let k = cfg_efields cfg
                                 t = cfg_mfields cfg
                             in IEEE754 ([Z] ++ replicate k Z ++ replicate t S)

min_positive_subnormal cfg = let k = cfg_efields cfg
                                 t = cfg_mfields cfg
                             in IEEE754 ([Z] ++ replicate k Z ++ replicate (t-1) Z ++ [S])

zero cfg = let k = cfg_efields cfg
               t = cfg_mfields cfg
           in IEEE754 ([U] ++ replicate (k+t) Z)

infinite cfg = let k = cfg_efields cfg
                   t = cfg_mfields cfg
               in IEEE754 ([U] ++ replicate (k+t) S)

is_NaN cfg (IEEE754 (_:bits)) = let k = cfg_efields cfg
                                    t = cfg_mfields cfg
                                in take k bits == replicate k S && drop k bits /= replicate t S

instance Show Binary where
    show (Binary s i f) = let sign = case s of {Z -> ""; S -> "-"}
                          in sign ++ integral i ++ "." ++ fractional f
        where integral   = show . foldr (\a e -> bit_value a + e*2) (0 :: Integer)
              fractional = drop 2 . show . foldr (\a e -> (bit_value a + e)/2) (0 :: Double) . take 50 

instance Show IEEE754 where
    show (IEEE754 bits) = showHex (foldl (\e a -> e*2 + bit_value a) 0 bits) ""
