{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module System.Hardware.Lifx.Lan.LowLevel.Protocol
    ( Lan,
      Bulb(..),
      RetryParams(..),
      newHdrAndCallback,
      sendMsg,
      openLan,
      openLan',
      closeLan,
      discoverBulbs,
      deviceId,
      defaultRetryParams,
      reliableAction,
      reliableQuery
      ) where

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Control.Concurrent
import Control.Concurrent.STM
-- import Control.Exception
import Control.Monad ( when, unless )
import Data.Array.MArray ( writeArray, readArray, newListArray )
import Data.Binary
    ( Binary(..),
      putWord8,
      getWord8,
      encode,
      decodeOrFail )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le )
import Data.Bits
import qualified Data.ByteString.Lazy as L
  ( ByteString, toChunks, append, length, fromStrict )
import Data.Int ( Int64 )
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Word ( Word8, Word16, Word32, Word64 )
import qualified Network.Info as NI
import Network.Socket
    ( Socket,
      SocketType(Datagram),
      SockAddr(SockAddrInet),
      Family(AF_INET),
      SocketOption(Broadcast),
      socket,
      setSocketOption,
      isSupportedSocketOption,
      defaultProtocol,
      bind,
      socketPort,
      aNY_PORT,
      close )
import Network.Socket.ByteString ( sendManyTo, recvFrom )
import System.Mem.Weak

import System.Hardware.Lifx.Lan.LowLevel.BaseTypes
import System.Hardware.Lifx.Lan.LowLevel.Util
import System.Hardware.Lifx.Lan.LowLevel.Types

{- GetService and StateService are defined here instead of
 - Messages.hs for dependency reasons. -}

----------------------------------------------------------

data GetService = GetService

instance MessageType GetService where
  msgType _ = 2

instance Binary GetService where
  put _ = return ()
  get = return GetService

----------------------------------------------------------

data StateService
  = StateService
    { ssService :: !Word8
    , ssPort    :: !Word32
    } deriving Show

instance MessageType StateService where
  msgType _ = 3

instance Binary StateService where
  put x = do
    putWord8 $ ssService x
    putWord32le $ ssPort x

  get =
    StateService <$> getWord8 <*> getWord32le

----------------------------------------------------------

type Callback = Lan -> SockAddr -> Header -> L.ByteString -> IO ()

-- | Type representing all the LIFX bulbs on the local network
data Lan
  = Lan
    { stSeq :: TVar Word8
    , stSource :: !Word32
    , stCallbacks :: TArray Word8 Callback
    , stLog :: String -> IO ()
    , stLogText :: T.Text -> IO ()
    , stSocket :: Socket
    , stBcast :: SockAddr
    , stThread :: Weak ThreadId
    , stIfName :: Maybe T.Text
    }

instance Show Lan where
  show (Lan { stIfName = ifname }) =
    maybe "*" T.unpack ifname

instance Eq Lan where
  x1 == x2 = x1 `compare` x2 == EQ

instance Ord Lan where
  x1 `compare` x2 =
    (stIfName x1) `compare` (stIfName x2)
    <> (stSource x1) `compare` (stSource x2)

-- | Type representing one LIFX bulb
data Bulb = Bulb Lan SockAddr DeviceId deriving (Show, Eq, Ord)

-- | Determine the Device ID for a particular 'Bulb'.
deviceId :: Bulb -> DeviceId
deviceId (Bulb _ _ di) = di

serviceUDP = 1

serializeMsg :: (MessageType a, Binary a) => Header -> a -> L.ByteString
serializeMsg hdr payload = hdrBs `L.append` payloadBS
  where payloadBS = encode payload
        hsize = dfltHdrSize + L.length payloadBS
        hdr' = hdr { hdrType = msgType payload , hdrSize = fromIntegral hsize }
        hdrBs = encode hdr'

newState :: Maybe T.Text -> Word32 -> Socket -> SockAddr -> Weak ThreadId
            -> Maybe (T.Text -> IO ()) -> STM Lan
newState ifname src sock bcast wthr logFunc = do
  seq <- newTVar 0
  cbacks <- newListArray (0, 255) (map noSeq [0..255])
  let (lg, lgt) = mkLogState logFunc
  return Lan { stSeq = seq
             , stSource = src
             , stCallbacks = cbacks
             , stLog = lg
             , stLogText = lgt
             , stSocket = sock
             , stBcast = bcast
             , stThread = wthr
             , stIfName = ifname
             }
  where mkLogState Nothing = ((\_ -> return ()), (\_ -> return ()))
        mkLogState (Just f) = ((f . T.pack), f)
        noSeq i st sa _ _ =
          stLog st $ "No callback for sequence #" ++ show i ++ strFrom sa

newHdr :: Lan -> STM Header
newHdr st = do
  let seq = stSeq st
  n <- readTVar seq
  writeTVar seq (n + 1)
  return $ dfltHdr { hdrSource = stSource st , hdrSequence = n }

registerCallback :: Lan -> Header -> Callback -> STM ()
registerCallback st hdr cb =
  writeArray (stCallbacks st) (hdrSequence hdr) cb

-- resorted to this weird thing to fix type errors
contortedDecode :: Binary a => L.ByteString -> (a, Either String Int64)
contortedDecode bs =
  case decodeOrFail bs of
   Left ( _ , _ , msg ) -> ( undefined , Left msg )
   Right ( lftovr , _ , payload ) -> ( payload , Right (L.length lftovr) )

checkHeaderFields :: (MessageType a, Binary a)
                     => Header -> L.ByteString
                     -> Either String a
checkHeaderFields hdr bs =
  let (payload, decodeResult) = contortedDecode bs
      typ = hdrType hdr
      expected = msgType payload
  in if typ /= expected
     then Left $ "expected type " ++ show expected ++ " but got " ++ show typ
     else case decodeResult of
           Left msg -> Left msg
           Right lftovr
             | lftovr /= 0 -> Left $ show lftovr ++ " bytes left over"
             | otherwise -> Right payload

strFrom :: SockAddr -> String
strFrom sa = " (from " ++ show sa ++ ")"

wrapCallback :: (MessageType a, Binary a) => (Header -> a -> IO ()) -> Callback
wrapCallback cb st sa hdr bs = f $ checkHeaderFields hdr bs
  where f (Left msg) = stLog st $ msg ++ strFrom sa
        f (Right payload) = cb hdr payload

wrapStateService :: (Bulb -> IO ()) -> Callback
wrapStateService cb st sa hdr bs = f $ checkHeaderFields hdr bs
  where f (Left msg) = stLog st (msg ++ frm)
        f (Right payload) = bulb (ssService payload) (ssPort payload)
        frm = strFrom sa
        bulb serv port
          | serv /= serviceUDP = return ()
{-
                                 stLog st $ "service: expected "
                                 ++ show serviceUDP ++ " but got "
                                 ++ show serv ++ frm
-}
          | otherwise = cb $ Bulb st (substPort sa port) (hdrTarget hdr)
        substPort (SockAddrInet _ ha) port = SockAddrInet (fromIntegral port) ha
        substPort other _ = other

wrapAndRegister :: (MessageType a, Binary a)
                   => Lan -> Header
                   -> (Header -> a -> IO ())
                   -> STM ()
wrapAndRegister st hdr cb = registerCallback st hdr $ wrapCallback cb

newHdrAndCallback :: (MessageType a, Binary a)
                     => Lan
                     -> (Header -> a -> IO ())
                     -> STM Header
newHdrAndCallback st cb = do
  hdr <- newHdr st
  wrapAndRegister st hdr cb
  return hdr

newHdrAndCbDiscovery :: Lan
                        -> (Bulb -> IO ())
                        -> STM Header
newHdrAndCbDiscovery st cb = do
  hdr <- newHdr st
  registerCallback st hdr $ wrapStateService cb
  return hdr

runCallback :: Lan -> SockAddr -> L.ByteString -> IO ()
runCallback st sa bs =
  case decodeOrFail bs of
   Left (_, _, msg) -> stLog st msg
   Right (bs', _, hdr) ->
     let hsz = fromIntegral (hdrSize hdr)
         len = L.length bs
         hsrc = hdrSource hdr
         ssrc = stSource st
         seq = hdrSequence hdr
         cbacks = stCallbacks st
         frm = strFrom sa
     in if hsz /= len
        then stLog st $ "length mismatch: " ++ show hsz
             ++ " ≠ " ++ show len ++ frm
        else if hsrc /= ssrc
             then stLog st $ "source mismatch: " ++ show hsrc
                  ++ " ≠ " ++ show ssrc ++ frm
             else runIt seq cbacks hdr bs'
  where runIt seq cbacks hdr bs' = do
          cb <- atomically $ readArray cbacks seq
          cb st sa hdr bs'

sendMsg :: (MessageType a, Binary a)
           => Bulb -> Header -> a
           -> IO ()
sendMsg (Bulb st sa targ) hdr payload =
  sendManyTo (stSocket st) (L.toChunks pkt) sa
  where hdr' = hdr { hdrTarget = targ }
        pkt = serializeMsg hdr' payload

discovery :: Lan -> (Bulb -> IO ()) -> STM L.ByteString
discovery st cb = do
  hdr <- newHdrAndCbDiscovery st cb
  let hdr' = hdr { hdrTagged = True }
  return $ serializeMsg hdr' GetService

-- | Call the callback for each 'Bulb' on the 'Lan'.  There is no way to
-- know when you have all the bulbs, so you have to just decide how long
-- you are willing to wait.
discoverBulbs :: Lan                -- ^ 'Lan' to discover bulbs on
                 -> (Bulb -> IO ()) -- ^ Callback called for each bulb
                 -> IO ()
discoverBulbs st cb = do
  pkt <- atomically $ discovery st cb
  sendManyTo (stSocket st) (L.toChunks pkt) (stBcast st)

-- | Return a 'Lan' which can be used to communicate with bulbs on the
-- local network.
openLan :: Maybe Interface -- ^ Name of network interface to use (such as
                           -- "wlan0" or "en1") or 'Nothing' to let the OS
                           -- choose an interface
           -> IO Lan
openLan ifname = openLan' ifname Nothing Nothing

-- | Return a 'Lan' which can be used to communicate with bulbs on the
-- local network.
openLan' :: Maybe Interface -- ^ Name of network interface to use (such as
                            -- "wlan0" or "en1") or 'Nothing' to let the OS
                            -- choose an interface
            -> Maybe Word16 -- ^ Port number that bulbs are listening on.
                            -- The only reason to change this is if you want
                            -- to use simulated bulbs.  'Nothing' will use
                            -- the correct port for real hardware.
            -> Maybe (T.Text -> IO()) -- ^ A function to log warning messages.
                                      -- 'Nothing' will not log anything.
            -> IO Lan
openLan' ifname mport mlog = do
  hostAddr <- ifaceAddr $ fmap T.unpack ifname
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ SockAddrInet aNY_PORT hostAddr
  when (isSupportedSocketOption Broadcast) (setSocketOption sock Broadcast 1)
  hostPort <- socketPort sock
  let port = 56700 `fromMaybe` mport
      bcast = SockAddrInet (fromIntegral port) 0xffffffff -- 255.255.255.255
      source = mkSource hostAddr (fromIntegral hostPort)
  tmv <- newEmptyTMVarIO
  thr <- forkFinally (dispatcher tmv) (\_ -> close sock)
  wthr <- mkWeakThreadId thr
  atomically $ do
    st <- newState ifname source sock bcast wthr mlog
    putTMVar tmv st
    return st

-- | Destroy a 'Lan' when it is no longer needed.  This is required, because
-- each 'Lan' has a background thread which needs to be terminated.
closeLan :: Lan -> IO ()
closeLan lan = endThread (stLogText lan) "dispatch" (stThread lan)

ethMtu = 1500

dispatcher :: TMVar Lan -> IO ()
dispatcher tmv = do
  st <- atomically $ takeTMVar tmv
  untilKilled (stLogText st) "discovery" $ do
    (bs, sa) <- recvFrom (stSocket st) ethMtu
    runCallback st sa $ L.fromStrict bs

{-
   LIFX requires each client on the network to have a unique,
   nonzero 32-bit value to identify it.  We'll use our IP address
   and port number, since that should be unique on our network.
   Unfortunately, that's 48 bits, so we hash it to get 32.
-}
mkSource :: Word32 -> Word16 -> Word32
mkSource ip port = nonzero $ murmur64 $ (ip' `shiftL` 16) .|. port'
  where ip' = fromIntegral ip :: Word64
        port' = fromIntegral port :: Word64
        -- use Murmur3's 64-bit finalizer as an integer hash function
        murmur64 n =
          let h1 = n `xor` (n `shiftR` 33)
              h2 = h1 * 0xff51afd7ed558ccd
              h3 = h2 `xor` (h2 `shiftR` 33)
              h4 = h3 * 0xc4ceb9fe1a85ec53
          in h4 `xor` (h4 `shiftR` 33)
        -- 32-bit source can't be 0, so check to see if either half
        -- of the 64-bit result is nonzero
        nonzero m64 =
          let lo = fromIntegral m64 :: Word32
              hi = fromIntegral (m64 `shiftR` 32) :: Word32
          in if lo /= 0 then lo
             else if hi /= 0 then hi
                  else 0xdeadbeef

ifaceAddr :: Maybe String -> IO Word32
ifaceAddr Nothing = return 0
ifaceAddr (Just ifname) = do
  ifaces <- NI.getNetworkInterfaces
  let miface = find (\x -> ifname == NI.name x) ifaces
      ifnames = map NI.name ifaces -- FIXME: ifnames is unused
  iface <- case miface of
    Just x -> return x
    Nothing -> undefined -- FIXME throw $ NoSuchInterface (T.pack ifname) (map T.pack ifnames)
  let (NI.IPv4 addr) = NI.ipv4 iface
  return addr

-- | Parameters describing how frequently to retry sending a message,
-- if no acknowledgment has been received.  The first retry will be after
-- 'rpMinInterval' seconds.  Then the retry interval will be multiplied by
-- 'rpMultiplier' after each retry, but it will be capped at 'rpMaxInterval'.
-- The whole process ends when an acknowledgment has been received, or when
-- the total elapsed time exceeds 'rpTimeLimit'.
data RetryParams =
  RetryParams
  { rpMinInterval :: !Float -- ^ seconds
  , rpMaxInterval :: !Float -- ^ seconds
  , rpMultiplier  :: !Float
  , rpTimeLimit   :: !Float -- ^ seconds
  }

-- | Default values for 'RetryParams'.
defaultRetryParams =
  RetryParams
  { rpMinInterval = 0.1
  , rpMaxInterval = 0.5
  , rpMultiplier = 2
  , rpTimeLimit = 5
  }

microsPerSecond = 1000000

-- | A helper function which lets you wrap an unreliable action, such as
-- 'setColor', so that it is reliable, with a specified timeout.
reliableAction :: RetryParams    -- ^ How frequently and how long to retry.
                  -> (IO () -> IO ()) -- ^ Action which accepts a callback which
                                      -- does not take an argument.
                  -> IO ()       -- ^ Callback to be called if action succeeds.
                  -> IO ()       -- ^ Callback to be called if action times out.
                  -> IO ()
reliableAction rp query cbSucc cbFail =
  reliableQuery rp query' cbSucc' cbFail
  where query' cb = query $ cb ()
        cbSucc' _ = cbSucc

-- | A helper function which lets you wrap an unreliable query, such as
-- 'getLocation', so that it is reliable, with a specified timeout.
reliableQuery :: RetryParams      -- ^ How frequently and how long to retry.
                 -> ((a -> IO ()) -> IO ()) -- ^ Query which accepts a callback
                                            -- which takes an argument.
                 -> (a -> IO ())  -- ^ Callback to be called if query succeeds.
                 -> IO ()         -- ^ Callback to be called if query times out.
                 -> IO ()
reliableQuery rp query cbSucc cbFail = do
  v <- newTVarIO False
  forkIO $ rq v (rpMinInterval rp) 0 $ round $ rpTimeLimit rp * microsPerSecond
  return ()
  where rq v interval totalµs limitµs = do
          let exceeded = totalµs >= limitµs
          done <- atomically $ do
            d <- readTVar v
            when exceeded $ writeTVar v True
            return d
          case (done, exceeded) of
           (True, _ ) -> return ()
           (False, True) -> cbFail
           (False, False) -> do
             query $ \x -> do
               done' <- atomically $ do
                 d <- readTVar v
                 writeTVar v True
                 return d
               unless done' $ cbSucc x
             let remainingµs = limitµs - totalµs
                 intervalµs = round $ interval * microsPerSecond
                 delayµs = min remainingµs intervalµs
             threadDelay delayµs
             let newInterval =
                   min (rpMaxInterval rp) (interval * rpMultiplier rp)
             rq v newInterval (totalµs + delayµs) limitµs

