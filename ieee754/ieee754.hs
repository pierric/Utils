module Main where

import Numeric(showHex)

-- for testing
import Data.Binary.IEEE754
import Debug.Trace

-- 0, 1, 无关
data Bit = Z | S | U deriving (Eq, Show, Enum)
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
n2b :: RealFrac a => a -> Binary

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


n2b num = let s = case signum num of{ -1 -> S; 0  -> U; 1  -> Z }
              (i,f) = properFraction (abs num)
          in  Binary s (reverse $ iToBin i) (fToBin f)
    where --整数部分到二进制,从低位到高位
          iToBin 0 = []
          iToBin i = let (q,r) = quotRem i 2
                     in toEnum (fromIntegral r) : iToBin q
          -- 小数部分到二进制
          fToBin f = let (q,r) = properFraction (f*2)
                     in toEnum (fromIntegral q) : fToBin r

-- 转换成Bit序列, 低位到高位
w2b x = let (q,r) = quotRem x 2 in toEnum (fromIntegral r) : w2b q

-- 将Binary按Config转换为IEEE754
encode cfg (Binary s i f) = let (m,e) = round $ normalize i f
                            in IEEE754 ([s] ++ make_exponent_and_significant e m)
    where k  = cfg_efields cfg
          t  = cfg_mfields cfg
          b  = 2 ^ (k - 1) - 1

          -- 将Binary统一化为m * 2^e，其中m成形于1.xx...
          -- 情况A:没有整数部分
          normalize [] f = let (zs,s) = span (== Z) $ take (t + b) (f ++ repeat Z)
                           in if null s
                              then (undefined, - (t + b))
                              else (s, - length zs - 1)
          -- 情况B:有整数部分
          normalize i f  = (i++f, length i - 1)

          sn = (- t + 1 - b, - b)      
          no = (- b + 1, b)

          round (m,e) | e < fst sn                 = (m,e)
                      | fst sn <= e && e <= snd sn = case mround t m of
                                                       ([S],m') -> (S:m', e+1)
                                                       ([Z],m') -> (m', e)
                      | fst no <= e && e <= snd no = case mround (t+1) m of
                                                       ([S],m') -> (S:m', e+1)
                                                       ([Z],m') -> (m', e)
                      | otherwise                  = (m,e)
              where -- 从bs中取n位,对n+1位舍入
                    -- 若第n+1位为0,舍
                    -- 若第n+1位为1,第n位为1,进
                    -- 若第n+1位为1,第n位为0,则判断从n+2位起是否存在1,有则进,无则舍
                    mround :: Int -> [Bit] -> ([Bit],[Bit])
                    mround 0 bs = error "t should be above zero"
                    mround n bs = let (b,a) = splitAt (n-1) bs 
                                  in case a of
                                       u:Z:_ -> ([Z], b ++ [u])
                                       S:S:_ -> carry S (b ++ [S])
                                       Z:S:c -> if S `elem` (take 80 c)
                                                then ([Z], b ++ [S])
                                                else ([Z], b ++ [Z])
                    carry c bs  = splitAt 1 $ reverse $ carry_ c $ reverse (Z:bs)
                        where carry_ Z b = b
                              carry_ S (Z:b) = S:b
                              carry_ S (S:b) = Z:carry_ S b
          -- 偏移指数,确定尾数
          -- 若e <  -t+1-b, 则移码为0
          -- 若e in [-t+1-b, -b],移码为 1 - off_exp
          -- 若e in [-b+1,b],移码为 e + b
          -- 否则移码为 2^n-1.
          make_exponent_and_significant e m 
              -- 0，指数与尾数部分全部为0
              | e < fst sn                = replicate (k + t) Z 
              -- subnormal，指数部分为0
              | fst sn <= e && e<= snd sn = let shift = - b - e 
                                            in replicate k Z ++ replicate shift Z ++ take (t - shift) m
              -- normal，指数部分为e+b
              | fst no <= e && e<= snd no = let efield = reverse $ take k $ w2b $ b + e
                                            in efield ++ take t (tail m)
              -- inf，指数全部为1，尾数全部为0
              | otherwise                 = replicate k S ++ replicate t Z
              

simple     = Config 4 3
ieee32bits = Config 8 23
ieee64bits = Config 11 52

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

bit_value Z = 0
bit_value S = 1
bit_value U = 0
          
prop_64bits :: Double -> Bool
prop_64bits dbl = let b1 = reverse $ take 64 $ w2b (doubleToWord dbl)
                      IEEE754 (s:b2) = encode ieee64bits (n2b dbl)
                  in case s of
                       U -> tail b1 == b2
                       _ -> b1 == s:b2
prop_32bits :: Float -> Bool
prop_32bits dbl = let b1 = reverse $ take 32 $ w2b (floatToWord dbl)
                      IEEE754 (s:b2) = encode ieee32bits (n2b dbl)
                  in case s of
                       U -> tail b1 == b2
                       _ -> b1 == s:b2