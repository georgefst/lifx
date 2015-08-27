module Lifx.Lan.Util
    ( putFloat32le,
      getFloat32le,
      putInt16le,
      getInt16le,
      bounds,
      bitBool,
      extract ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Data.Binary ( Put, Get )
import Data.Binary.Put ( putWord32le, putWord16le )
import Data.Binary.Get ( getWord32le, getWord16le )
import Data.Bits ( Bits((.&.), bit, shiftR, clearBit {- zeroBits -}) )
import Data.Int ( Int16, Int64 )
import Data.ReinterpretCast ( wordToFloat, floatToWord )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

bounds :: (Integral a, Bits a, Show a) => String -> Int -> a -> Put
bounds name n val =
  when (val >= limit) $ fail (name ++ ": " ++ show val ++ " >= " ++ show limit)
  where limit = bit n

bitBool :: Bits a => Int -> Bool -> a
bitBool _ False = clearBit (bit 0) 0 -- zeroBits
bitBool n True = bit n

extract :: (Integral a, Bits a, Integral b) => a -> Int -> Int -> b
extract x n w = fromIntegral field
  where field = (x `shiftR` n) .&. mask
        mask = bit w - 1

putFloat32le :: Float -> Put
putFloat32le f = putWord32le $ floatToWord f

getFloat32le :: Get Float
getFloat32le = wordToFloat <$> getWord32le

putInt16le :: Int16 -> Put
putInt16le i = putWord16le $ fromIntegral i

getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le
