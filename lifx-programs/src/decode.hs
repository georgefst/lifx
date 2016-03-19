{-# LANGUAGE OverloadedStrings #-}

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as L
import Data.Char

import Lifx
import Lifx.Lan.LowLevel

fromRight :: Either String a -> a
fromRight = either error id

main = do
  contents <- B.getContents
  let hex = B.filter (isHexDigit . chr . fromIntegral) contents
      (bs, _ ) = B16.decode hex
      (payloadBS, _ , hdr) = case decodeOrFail (L.fromStrict bs) of
                              Left (_ , _ , msg) -> error msg
                              Right x -> x
      (_, _, sc) = case decodeOrFail payloadBS of
                    Left (_, _, msg) -> error msg
                    Right x -> x
  print (hdr :: Header)
  print (sc :: SetColor)

  putStrLn ""

  let hdr' = dfltHdr { hdrSource = 0x12345678
                     , hdrTarget = read "0015edaabbcc"
                     , hdrType = 107
                     }
      sl = StateLight { slColor = HSBK 0x1111 0x2222 0x3333 5000
                      , slPower = On
                      , slLabel = fromRight $ fromText "Living Room"
                      }
      slBS = encode sl
      hdr'' = hdr' { hdrSize = hdrSize hdr' + fromIntegral (L.length slBS) }
      hdrBS = encode hdr''
      myBS = hdrBS `L.append` slBS
      hex' = B16.encode (L.toStrict myBS)
  print hdr''
  print sl
  B.putStr hex'
  putStrLn ""

