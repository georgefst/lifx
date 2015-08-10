import Control.Concurrent.STM ( STM, atomically )
import Control.Monad ( when, forever )
import Data.ByteString.Lazy ( ByteString, toChunks, fromStrict )
import Data.Hourglass
    ( ElapsedP(ElapsedP),
      ISO8601_DateAndTime(ISO8601_DateAndTime),
      timePrint )
import Data.Word ( Word64 )
import Network.Socket
    ( SocketType(Datagram),
      SockAddr(SockAddrInet),
      Family(AF_INET),
      SocketOption(Broadcast),
      AddrInfoFlag(AI_NUMERICHOST, AI_NUMERICSERV),
      AddrInfo(addrAddress, addrFlags),
      socket,
      setSocketOption,
      isSupportedSocketOption,
      iNADDR_ANY,
      getAddrInfo,
      defaultProtocol,
      defaultHints,
      bind,
      aNY_PORT )
import Network.Socket.ByteString ( sendManyTo, recvFrom )
import Text.Printf ( printf )

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

discovery :: Lan -> STM ByteString
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
