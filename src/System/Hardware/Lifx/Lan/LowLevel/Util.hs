{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.Lifx.Lan.LowLevel.Util
    ( putFloat32le,
      getFloat32le,
      putInt16le,
      getInt16le,
      putBool16,
      getBool16,
      bounds,
      bitBool,
      extract,
      endThread,
      untilKilled,
      notAsync ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent
import Control.Exception
import Control.Monad ( when, forever )
import Data.Binary ( Put, Get )
import Data.Binary.Put ( putWord32le, putWord16le )
import Data.Binary.Get ( getWord32le, getWord16le )
import Data.Bits ( Bits((.&.), bit, shiftR, zeroBits) )
import Data.Int ( Int16 )
import Data.Monoid
import Data.ReinterpretCast ( wordToFloat, floatToWord )
import qualified Data.Text as T
import System.Mem.Weak

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

putBool16 :: Bool -> Put
putBool16 True = putWord16le 0xffff
putBool16 False = putWord16le 0

getBool16 :: Get Bool
getBool16 = do
  x <- getWord16le
  return $ x /= 0

-- like killThread, but handles dereferencing weak thread,
-- and prints a message if dereferencing fails
endThread :: (T.Text -> IO ()) -> T.Text -> Weak ThreadId -> IO ()
endThread lg name wthr = do
  mthr <- deRefWeak wthr
  case mthr of
   Nothing -> lg $ "Couldn't kill " <> name <> " thread"
   (Just thr) -> killThread thr

-- like forever, but if an exception happens, print the exception
-- and keep on going forever, unless it's an AsyncException like
-- ThreadKilled.
untilKilled :: (T.Text -> IO ()) -> T.Text -> IO () -> IO ()
untilKilled lg name loop =
  forever $ catchJust notAsync (forever loop) $
  \e -> lg $ "Exception in " <> name <> " thread: " <> T.pack (show e)

-- Returns Nothing if argument is an AsyncException.
-- Otherwise, returns argument.
notAsync :: SomeException -> Maybe SomeException
notAsync e =
  if isAsync (fromException e)
  then Nothing
  else Just e

isAsync :: Maybe AsyncException -> Bool
isAsync (Just _ ) = True
isAsync Nothing = False
