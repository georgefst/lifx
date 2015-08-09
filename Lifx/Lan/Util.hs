module Lifx.Lan.Util where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.Array.MArray
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy hiding (length, putStrLn, empty, map, take, replicate)
import qualified Data.ByteString.Lazy as L (length, take, replicate)
import Data.Char
import Data.Hourglass
import Data.Int
import Data.ReinterpretCast
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Text.Printf

bounds :: (Integral a, Bits a, Show a) => String -> Int -> a -> Put
bounds name n val =
  when (val >= limit) $ fail (name ++ ": " ++ show val ++ " >= " ++ show limit)
  where limit = bit n

bitBool :: Bits a => Int -> Bool -> a
bitBool _ False = zeroBits
bitBool n True = bit n

extract :: (Integral a, Bits a, Integral b) => a -> Int -> Int -> b
extract x n w = fromIntegral field
  where field = (x `shiftR` n) .&. mask
        mask = (bit w) - 1

putFloat32le :: Float -> Put
putFloat32le f = putWord32le $ floatToWord f

getFloat32le :: Get Float
getFloat32le = wordToFloat <$> getWord32le

putInt16le :: Int16 -> Put
putInt16le i = putWord16le $ fromIntegral i

getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le

padByteString :: Int64 -> ByteString -> ByteString
padByteString goal bs = f (l `compare` goal)
  where l = L.length bs
        f LT = bs `append` pad
        f EQ = bs
        f GT = L.take goal bs
        pad = L.replicate (goal - l) 0
