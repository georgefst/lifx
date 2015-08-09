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

import Lifx.Lan.Util
import Lifx.Lan.Types
import Lifx.Lan.Messages
import Lifx.Lan.Protocol

myTime :: Word64 -> String
myTime nanos =
  let (q, r) = quotRem nanos 1000000000
      elapp = ElapsedP (fromIntegral q) (fromIntegral r)
  in timePrint ISO8601_DateAndTime elapp


myCb :: Bulb -> IO ()
myCb bulb = do
  putStrLn (show bulb)
  doGetHostInfo bulb $ \shi -> do
    putStrLn (show shi)
    doGetLight bulb $ \sl -> do
      putStrLn (show sl)
      doSetPower bulb True $ do
        doSetColor bulb (HSBK 32768 65535 65535 3000) 0 $ do
          -- positive numbers mean more time spent on original color
          let swf = SetWaveform False (HSBK 0 0 65535 9000) 1000 10 (-20000) Pulse
          doSetWaveform bulb swf $ do
            doGetHostFirmware bulb $ \shf -> do
              putStrLn (show shf)
              let vHex = printf "%x" (shfVersion shf)
              putStrLn $ "build " ++ (myTime $ shfBuild shf) ++ ", version " ++ vHex
              doGetWifiFirmware bulb $ \swf -> do
                putStrLn (show swf)
                let vHex' = printf "%x" (swfVersion swf)
                putStrLn $ "build " ++ (myTime $ swfBuild swf) ++ ", version " ++ vHex'
                doGetVersion bulb $ \sv -> do
                  putStrLn (show sv)
                  printf "%x %x %x\n" (svVendor sv) (svProduct sv) (svVersion sv)
                  doGetInfo bulb $ \si -> do
                    putStrLn (show si)
                    putStrLn $ "current time = " ++ (myTime $ siTime si)
                    putStrLn "done!"

discovery :: InternalState -> STM ByteString
discovery st = do
  hdr <- newHdrAndCbDiscovery st myCb
  let hdr' = hdr { hdrTagged = True }
  return $ serializeMsg hdr' GetService

ethMtu = 1500

main = do
  putStrLn "Hello!"
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
  when (isSupportedSocketOption Broadcast) (setSocketOption sock Broadcast 1)
  let flags = [ AI_NUMERICHOST , AI_NUMERICSERV ]
      myHints = defaultHints { addrFlags = flags }
  (ai:_ ) <- getAddrInfo (Just myHints) (Just "192.168.11.255") (Just "56700")
  let bcast = addrAddress ai
  st <- atomically $ newState 37619 sock (Just putStrLn)
  pkt <- atomically $ discovery st
  sendManyTo sock (toChunks pkt) bcast
  forever $ do
    (bs, sa) <- recvFrom sock ethMtu
    runCallback st sa $ fromStrict bs
  -- close sock
