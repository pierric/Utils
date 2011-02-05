{-# LANGUAGE TemplateHaskell,ScopedTypeVariables #-}
module Main where

import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Bits
import Data.Char(ord)
import Data.Accessor
import Data.Accessor.Template
import Data.Char
import Data.List
import Data.Time
import Data.Maybe
import qualified Data.Binary.Get as Binary
import Codec.Text.IConv
import System.Locale
import System.IO.Unsafe

data IPD = IPD{
      dbNames_ :: [String],
      dbs_     :: [Block]
    }

data Block = Block{
      dbIdx_  :: Int,
      recSize_ :: Int,
      dbVersion_ :: Int,
      recHandler_ :: Int,
      recUID_ :: Int,
      fields_ :: [Field]
    }

data Field = Field{
      fieldLength_ :: Int,
      fieldType_   :: FieldType,
      fieldData_   :: BS.ByteString
    } deriving Show

data FieldType = PhoneNumber | SMSContent | O1 | O7 | O9 | Oa | Ob | Ounk Int deriving (Show, Eq)

$(deriveAccessors ''Field)
$(deriveAccessors ''Block)
$(deriveAccessors ''IPD)

toFieldType 0x01 = O1
toFieldType 0x02 = PhoneNumber
toFieldType 0x04 = SMSContent
toFieldType 0x07 = O7
toFieldType 0x09 = O9
toFieldType 0x0a = Oa
toFieldType 0x0b = Ob
toFieldType unk  = Ounk unk

ipd = do string "Inter@ctive Pager Backup/Restore File"
         char '\x0A'
         char '\x02'
         n <- halfbe
         char '\x00'
         dbNames <- count n databaseName
         dbs <- manyTill databaseBlock end
         return $ IPD dbNames dbs
    where
      databaseName  = do n <- halfle
                         cs <- count (n-1) anyChar
                         anyChar
                         return cs
      databaseBlock = do n <- halfle
                         l <- wordle
                         v <- byte
                         h <- halfle
                         uid <- wordle
                         fs <- fields l
                         return $ Block n l v h uid fs
      fields total | total < 7 = fail "mismatching field length"
                   | total ==7 = return []
                   | otherwise = do fl <- halfle
                                    ft <- liftM toFieldType byte
                                    stream <- getInput
                                    let (fd, rest) = BS.splitAt (fromIntegral fl) stream
                                    setInput rest
                                    liftM (Field fl ft fd :) $ fields (total - 3 - fl)

end = string "\xff\xff\x02\x00\x00\01"
                                        
byte, halfbe, halfle, wordle :: Integral int =>  Parser int
byte = do c <- anyChar
          return $ fromIntegral $ ord c
halfbe = do [ch,cl] <- count 2 anyChar
            return $ fromIntegral $ ord ch `shiftL` 8 .|. ord cl
halfle = do [cl,ch] <- count 2 anyChar
            return $ fromIntegral $ ord ch `shiftL` 8 .|. ord cl
wordle = do [cll,clh,chl,chh] <- count 4 anyChar
            return $ fromIntegral $ ord chh `shiftL` 24 .|. ord chl `shiftL` 16 .|. ord clh `shiftL` 8 .|. ord cll


-- TODO: replace '\n' with '\\n', '\t' with '\\t' in content
process :: Block -> IO ()
process block = let mapping = flip map (block^.fields)
                              (\f -> (f^.fieldType, f^.fieldData))
                    time    = lookup O1 mapping >>= parseTime
                    phone   = lookup PhoneNumber mapping >>= parsePhone
                    content = lookup SMSContent mapping >>= parseContent
                    dir     = lookup O1 mapping >>= parseDirection
                in do BS.putStr $ fromMaybe BS.empty $ time
                      BS.putStr (BS.singleton '\t')
                      BS.putStr (BS.singleton '\'') -- the leading \' prevents OO automatically convert it to numbers
                      BS.putStr $ fromMaybe (BS.pack "Unknown") phone
                      BS.putStr (BS.singleton '\t')
                      BS.putStr $ fromMaybe BS.empty dir
                      BS.putStr (BS.singleton '\t')
                      BS.putStr $ fromMaybe BS.empty content
                      BS.putStr (BS.singleton '\n')
    where
      parseTime s  = do let t1 = localTime $ (llong (BS.take 8 $ BS.drop 13 s) `div` 1000)
                            t2 = localTime $ (llong (BS.take 8 $ BS.drop 21 s) `div` 1000)
                        return $ BS.pack $ show t1
          where localTime secs = let t = readTime defaultTimeLocale "%s" (show secs)
                                 in (utcToZonedTime currentTimeZone t)
                currentTimeZone = unsafePerformIO getCurrentTimeZone
                llong = Binary.runGet Binary.getWord64le

      parsePhone s = return $ BS.takeWhile valid $ BS.dropWhile (not . valid) s
          where valid c = c == '+' || isDigit c

      parseContent s = return $ convertFuzzy Discard "UCS-2BE" "UTF-8" s
                                 
      parseDirection s = if (Binary.runGet Binary.getWord8 s) == 0 then
                             return $ BS.singleton 'S'
                         else 
                             return $ BS.singleton 'R'

main = parseFromFile ipd "Databases.ipd" >>= either (\ e -> putStrLn $ show e) handler
    where handler (IPD names blocks) = mapM_ process blocks