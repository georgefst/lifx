module Lifx.Lan.Util
    ( putFloat32le,
      getFloat32le,
      putInt16le,
      getInt16le,
      padByteString,
      textToByteString,
      textToPaddedByteString,
      bounds,
      bitBool,
      extract ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Data.Binary ( Put, Get )
import Data.Binary.Put ( putWord32le, putWord16le )
import Data.Binary.Get ( getWord32le, getWord16le )
import Data.Bits ( Bits((.&.), bit, shiftR, zeroBits) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
    ( ByteString, append, length, take, replicate )
import Data.Int ( Int16, Int64 )
import Data.ReinterpretCast ( wordToFloat, floatToWord )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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
        mask = bit w - 1

putFloat32le :: Float -> Put
putFloat32le f = putWord32le $ floatToWord f

getFloat32le :: Get Float
getFloat32le = wordToFloat <$> getWord32le

putInt16le :: Int16 -> Put
putInt16le i = putWord16le $ fromIntegral i

getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le

-- pad (with 0) or truncate a bytestring to make it exactly the specified length
padByteString :: Int -> B.ByteString -> B.ByteString
padByteString goal bs = f (l `compare` goal)
  where l = B.length bs
        f LT = bs `B.append` pad
        f EQ = bs
        f GT = B.take goal bs
        pad = B.replicate (goal - l) 0

-- truncate a Text to fit in the specific number of bytes, encoded as UTF-8,
-- but without truncating in the middle of a character
textToByteString :: Int -> T.Text -> B.ByteString
textToByteString maxBytes txt = t2bs (maxBytes `div` 4)
  where t2bs n =
          let nPlus1 = n + 1
              bs = convert nPlus1
              bsLen = B.length bs
          in if bsLen > maxBytes
             then convert n
             else if bsLen == maxBytes || n >= txtLen
                  then bs
                  else t2bs nPlus1
        txtLen = T.length txt
        convert n = TE.encodeUtf8 $ T.take n txt

textToPaddedByteString :: Int -> T.Text -> B.ByteString
textToPaddedByteString maxBytes txt =
  padByteString maxBytes $ textToByteString maxBytes txt
