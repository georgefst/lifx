{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

import Control.Concurrent
import Control.Monad ( when, forM_, void )
import qualified Control.Exception as E (catch)
import Data.Hourglass
import Data.Int ( Int64 )
import Data.List hiding (insert)
import Data.Monoid hiding (Product)
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Format hiding (print)
import qualified Data.Text.IO as TIO
import Data.Version
import qualified Network.Info as NI
import System.Exit

import Lifx
import Lifx.Cloud
import Lifx.Connection
import Lifx.Internal
import Lifx.Lan

import qualified Lifx.Program.CmdParser as C
import Lifx.Program.Column
import Lifx.Program.ProductShortName
import Lifx.Program.TargetMatch

import Paths_lifx_programs

pkg_name :: T.Text
pkg_name = "lifx-programs"

pkg_url :: T.Text
pkg_url = "+https://github.com/ppelleti/hs-lifx"

nsToDuration :: NanoSeconds -> (Int64, Duration)
nsToDuration (NanoSeconds ns) =
  (days, Duration (Hours hours) (Minutes minutes)
                  (Seconds seconds) (NanoSeconds nanos))
  where (s, nanos) = ns `quotRem` 1000000000
        (m, seconds) = s `quotRem` 60
        (h, minutes) = m `quotRem` 60
        (days, hours) = h `quotRem` 24

l2 = left 2 ' '
l3 = left 3 ' '
l4 = left 4 ' '
l5 = left 5 ' '

fmtLabel :: Maybe Label -> T.Text
fmtLabel Nothing = ""
fmtLabel (Just lbl) = toText lbl

fmtPower :: Bool -> Maybe Power -> T.Text
fmtPower False _ = "???"
fmtPower True Nothing = "???"
fmtPower True (Just pwr) = T.pack $ show pwr

ii :: Double -> Int
ii = round

fmtColor :: PartialColor -> T.Text
fmtColor (HSBK (Just h) (Just s) (Just b) (Just k)) =
  fmt "{} {} {} {}K" (l3 $ ii h, l3 $ ii s', l3 $ ii b', l4 $ ii k)
  where s' = s * 100
        b' = b * 100
fmtColor _ = ""

fmtTemperature :: Maybe Double -> [T.Text]
fmtTemperature Nothing = [""]
fmtTemperature (Just temp) =
  [ fmt "{}°C" (Only $ l2 $ fixed 0 temp)
  , fmt "{}°C" (Only $ l4 $ fixed 1 temp)
  , fmt "{}°C" (Only $ l5 $ fixed 2 temp)
  ]

fmtUptime :: Maybe FracSeconds -> [T.Text]
fmtUptime Nothing = [""]
fmtUptime (Just uptime) =
  fmtDur $ nsToDuration $ NanoSeconds $ round $ uptime * 1e9
  where fmtDur (days, Duration (Hours hours) (Minutes minutes)
                      (Seconds seconds) _ ) =
          let xs = [(days, 'd'), (hours, 'h'), (minutes, 'm'), (seconds, 's')]
              xs' = dropWhile (\(n, u) -> n == 0 && u /= 's') xs
              txts = map (\(n, s) -> fmt "{}{}" (n, s)) xs'
              choices = tail $ inits txts
          in map mconcat choices

fmtFirmware :: Maybe Version -> T.Text
fmtFirmware Nothing = ""
fmtFirmware (Just vers) = T.pack $ showVersion vers

fmtProduct :: Maybe Product -> Maybe Int -> [T.Text]
fmtProduct Nothing _ = [""]
fmtProduct (Just pr) Nothing = [productShortName $ pProductName pr]
fmtProduct pr (Just vers) = name ++ map (\x -> fmt "{}v{}" (x, vers)) name
  where name = fmtProduct pr Nothing

tr :: T.Text -> IO ()
tr = TIO.putStrLn . T.stripEnd

data LightRow =
  LightRow
  { lrLabel    :: [T.Text]
  , lrPower    :: [T.Text]
  , lrColor    :: [T.Text]
  , lrTemp     :: [T.Text]
  , lrUptime   :: [T.Text]
  , lrDevId    :: [T.Text]
  , lrFirmware :: [T.Text]
  , lrHardware :: [T.Text]
  , lrGroup    :: [T.Text]
  , lrLocation :: [T.Text]
  , lrLastSeen :: [T.Text]
  }

type Cols = [Column (LightRow -> [T.Text])]

columns :: Cols
columns =
  [ Column Lft Lft 15 32  40 ["Label"]               lrLabel
  , Column Lft Lft  3  3   0 ["Pwr", "Power"]        lrPower
  , Column Lft Lft 17 17   0 ["Color"]               lrColor
  , Column Lft Lft  6  7  30 ["Temp", "Temperature"] lrTemp
  , Column Rgt Lft 11 11  90 ["Uptime"]              lrUptime
  , Column Lft Rgt  6 12 100 ["DevID", "Device ID"]  lrDevId
  , Column Lft Lft  4  4   0 ["FW", "Firmware"]      lrFirmware
  , Column Lft Lft  5  7  50 ["HW", "Hardware"]      lrHardware
  , Column Lft Lft  8 32  36 ["Group"]               lrGroup
  , Column Lft Lft  8 32  34 ["Location"]            lrLocation
  , Column Rgt Lft 11 11  90 ["Last Seen"]           lrLastSeen
  ]

lanOnly :: [T.Text]
lanOnly = [ "Temp", "Uptime", "FW" ]

cloudOnly :: [T.Text]
cloudOnly = [ "Last Seen" ]

type FixedCols = [FixedColumn (LightRow -> [T.Text])]

mkRow :: LightInfo -> LightRow
mkRow li =
  LightRow [label] [power] [color] temp uptime [devid] [fw] vers [group] [loc] seen
  where label = fmtLabel $ lLabel li
        power = fmtPower (lConnected li) (lPower li)
        color = fmtColor $ lColor li
        temp = fmtTemperature $ lTemperature li
        uptime = fmtUptime $ lUptime li
        devid = toText $ lId li
        fw = fmtFirmware $ lFirmwareVersion li
        vers = fmtProduct (lProduct li) (lHardwareVersion li)
        group = maybe "" toText $ lGroup li
        loc   = maybe "" toText $ lLocation li
        seen  = fmtUptime $ Just $ lSecondsSinceSeen li

listLite :: FixedCols -> LightInfo -> IO ()
listLite fc li = tr $ displayRow' fc $ mkRow li

listLites :: FixedCols -> [LightInfo] -> IO ()
listLites fc li = mapM_ (listLite fc) li

-- This won't do much good for Lan, since lights are listed as they
-- are discovered.  But for Cloud, we can nicely arrange them by
-- location and group.
orderLites :: LightInfo -> LightInfo -> Ordering
orderLites x y =
  comparing lLocation x y <>
  comparing lGroup x y <>
  comparing lLabel x y

cmdList :: Connection c => FixedCols -> c -> [Selector] -> IO Bool
cmdList fc conn sels = do
  li <- listLights conn sels needEverything
  listLites fc (sortBy orderLites li)
  return True

resultsColumns =
  [ Column Lft Lft  8 32  0 ["Label"]     (replicate 1 . fmtLabel . rLabel)
  , Column Lft Rgt 12 12 10 ["Device ID"] (replicate 1 . toText . rId)
  , Column Lft Lft  8  8 20 ["Status"]    (replicate 1 . T.pack . show . rStatus)
  ]

resultsFixedCols = fixColumns 80 resultsColumns

prResults :: [Result] -> IO Bool
prResults results = do
  forM_ results $ tr . displayRow' resultsFixedCols
  return True

cmdPower :: Connection c => Power -> FracSeconds -> c -> [Selector] -> IO Bool
cmdPower pwr dur conn sels = do
  setState conn sels (StateTransition (Just pwr) emptyColor dur) >>= prResults


cmdColor :: Connection c
            => PartialColor
            -> FracSeconds
            -> c
            -> [Selector]
            -> IO Bool
cmdColor pc dur conn sels =
  setState conn sels (StateTransition Nothing pc dur) >>= prResults



cmdWave :: Connection c
           => EffectType
           -> PartialColor
           -> C.PulseArg
           -> c
           -> [Selector]
           -> IO Bool
cmdWave et ca pa conn sels =
  effect conn sels eff >>= prResults
  where eff = defaultEffect
              { eType = et
              , eColor = ca
              , ePeriod = C.paPeriod pa
              , eCycles = C.paCycles pa
              , ePersist = C.paPersist pa
              , ePowerOn = C.paPowerOn pa
              , ePeak = C.paPeak pa
              }

cmdSetLabel :: Connection c
               => T.Text
               -> c
               -> [Selector]
               -> IO Bool
cmdSetLabel txt conn [SelDevId dev] = do
  let lbl = either error id $ fromText txt
  result <- setLabel conn dev lbl
  prResults [result]
  return False
cmdSetLabel _ _ [] = return True
cmdSetLabel _ _ _ = do
  TIO.putStrLn "Need just one Device ID"
  return False


cmd2func :: Connection c
         => C.LiteCmd
         -> Cols
         -> FracSeconds
         -> c
         -> [Selector]
         -> IO Bool
cmd2func (C.CmdList w)     cols _ = cmdList (fixColumns w cols)
cmd2func C.CmdOn            _ dur = cmdPower On dur
cmd2func C.CmdOff           _ dur = cmdPower Off dur
cmd2func (C.CmdColor ca)    _ dur = cmdColor ca dur
cmd2func (C.CmdPulse ca pa)   _ _ = cmdWave Pulse ca pa
cmd2func (C.CmdBreathe ca pa) _ _ = cmdWave Breathe ca pa
cmd2func (C.CmdSetLabel lbl)  _ _ = cmdSetLabel lbl

lsHeader :: [FixedColumn a] -> IO ()
lsHeader fc = do
  tr $ displayHeader fc
  tr $ displaySep    fc

hdrIfNeeded :: C.LiteCmd -> Cols -> IO ()
hdrIfNeeded (C.CmdList w) cols = lsHeader $ fixColumns w cols
hdrIfNeeded _ _ = lsHeader resultsFixedCols

linfoToLiteIds :: LightInfo -> LiteIds
linfoToLiteIds li = LiteIds
  { liDevId   = lId li
  , liLabel   = lLabel li
  , liGroupId = lGroupId li
  , liGroup   = lGroup li
  , liLocId   = lLocationId li
  , liLoc     = lLocation li
  }

ifaceColumns =
  [ Column Lft Lft  4 10 50 ["Iface", "Interface"] (\n -> [T.pack $ NI.name n])
  , Column Lft Lft 15 15  0 ["IPv4"]       (\n -> [T.pack $ show $ NI.ipv4 n])
  , Column Lft Lft 17 17  0 ["MAC"]        (\n -> [T.pack $ show $ NI.mac n])
  ]

ifaceFixedCols = fixColumns 80 ifaceColumns

fmtIfaceRow :: NI.NetworkInterface -> T.Text
fmtIfaceRow = displayRow' ifaceFixedCols

nonZeroIpv4 :: NI.NetworkInterface -> Bool
nonZeroIpv4 ni = 0 /= getv4 (NI.ipv4 ni)
  where getv4 (NI.IPv4 v4) = v4

fmtInterfaces :: [NI.NetworkInterface] -> T.Text
fmtInterfaces ifaces =
  let rows = map fmtIfaceRow $ filter nonZeroIpv4 ifaces
      rows' = displayHeader ifaceFixedCols : displaySep ifaceFixedCols : rows
  in T.unlines rows'

prInterfaces :: IO ()
prInterfaces = do
  ifaces <- NI.getNetworkInterfaces
  TIO.putStr $ fmtInterfaces ifaces

prLifxException :: LifxException -> IO a
prLifxException (NoSuchInterface ifname _ ) = do
  TIO.putStrLn $ fmt "No such interface \"{}\".  Try instead:" (Only ifname)
  TIO.putStrLn T.empty
  prInterfaces
  exitFailure


matchesTarget :: Targets -> LiteIds -> Bool
matchesTarget TargAll _ = True
matchesTarget (TargSome tm) lids = True `S.member` S.map (`tmatch` lids) tm

findAndRun :: Connection c
              => c
              -> (c -> [Selector] -> IO Bool)
              -> Targets
              -> Int
              -> S.Set DeviceId
              -> Bool
              -> IO ()
findAndRun _ _ _ 0 _ _ = return ()
findAndRun conn func TargAll _ _ True = void $ func conn [SelAll]
findAndRun conn func targs n s isCloud = do
  when (not isCloud) $ threadDelay 100000
  li <- listLights conn [SelAll] [NeedLabel, NeedGroup, NeedLocation]
  let lids = map linfoToLiteIds li
      selected = S.fromList $ map liDevId $ filter (matchesTarget targs) lids
      selected' = selected `S.difference` s
      s' = selected `S.union` s
      sels = map SelDevId $ S.toList selected'
  more <- func conn sels
  when more $ findAndRun conn func targs (n - 1) s' isCloud

stripCols :: [T.Text] -> Cols
stripCols strip = filter f columns
  where f col =
          let name = head (cName col)
          in not $ name `elem` strip

prependUserAgent :: CloudSettings -> CloudSettings
prependUserAgent cs = cs { csUserAgent = uac : csUserAgent cs }
  where uac = UserAgentComponent pkg_name version (Just pkg_url)

useCloud = True

main = do
  args <- C.parseCmdLine
  let ifname = C.aInterface args
      cmd = C.aCmd args
      cols = stripCols (if useCloud then lanOnly else cloudOnly)
      func = cmd2func cmd cols (C.aDuration args)
  hdrIfNeeded cmd cols
  if useCloud
    then do
    let settings = prependUserAgent defaultCloudSettings
    conn <- openCloudConnection settings `E.catch` prLifxException
    findAndRun conn func (C.aTarget args) 1 S.empty True
    closeConnection conn
    else do
    let settings = defaultLanSettings
                   { lsUnknownSelectorBehavior = IgnoreUnknownSelector }
    conn <- openLanConnection settings `E.catch` prLifxException
    findAndRun conn func (C.aTarget args) 20 S.empty False
    closeConnection conn
