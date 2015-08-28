{-# LANGUAGE StandaloneDeriving #-}

module Lifx.Program.Timer where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char
import Data.Hourglass
import Data.Int
import qualified Data.Set as S
import qualified Data.Text as T

import Lifx.Types
import Lifx.Program.CmdParser as C

deriving instance Show Duration
deriving instance Read TimeOfDay

data Command = On | Off | OnOff | Cycle deriving (Eq, Ord, Show, Read)

data Event =
  Event
  { evCommand :: !Command
  , evTarget :: Selector
  , evTime :: (TimeOfDay, TimeOfDay)
  , evDay :: S.Set WeekDay
  , evDuration :: (Duration, Duration)
  , evOnPeriod :: (Duration, Duration)
  , evOffPeriod :: (Duration, Duration)
  , evColor :: MaybeColor
  } deriving (Eq, Ord, Show, Read)

parseCommand :: T.Text -> Either String Command
parseCommand txt = C.readEither' $ C.capitalize $ T.unpack txt

parseWeekDays :: T.Text -> Either String (S.Set WeekDay)
parseWeekDays txt = do
  daze <- mapM (wd . toLower) (T.unpack txt)
  return $ S.fromList daze
  where
    wd 'u' = return Sunday
    wd 'm' = return Monday
    wd 't' = return Tuesday
    wd 'w' = return Wednesday
    wd 'r' = return Thursday
    wd 'f' = return Friday
    wd 's' = return Saturday
    wd c = Left $ '\'' : c : "' is not in \"umtwrfs\""

duration :: Parser Duration
duration = do
  hours <- option 0 $ do
    h <- decimal
    skipSpace
    char 'h'
    skipSpace
    return h
  minutes <- option 0 $ do
    m <- decimal
    skipSpace
    char 'm'
    skipSpace
    return m
  seconds <- option 0 $ do
    s <- decimal
    skipSpace
    char 's'
    skipSpace
    return s
  return $ Duration (Hours hours)     (Minutes minutes)
                    (Seconds seconds) (NanoSeconds 0)

durationRange :: Parser (Duration, Duration)
durationRange = fromTo <|> plusMinus <|> singleTime
  where
    singleTime = do
      t <- duration
      return (t, t)
    fromTo = do
      t1 <- duration
      skipSpace
      char '-'
      skipSpace
      t2 <- duration
      return (t1, t2)
    plusMinus = do
      t <- duration
      skipSpace
      char '±'
      skipSpace
      d <- duration
      return (t `durSub` d, t `durAdd` d)

durAdd = durOp (+)
durSub = durOp (-)

durOp :: TimeInterval i
         => (Seconds -> Seconds -> Seconds)
         -> i -> i -> i
durOp op d1 d2 =
  let newSecs = toSeconds d1 `op` toSeconds d2
  in fst $ fromSeconds newSecs

timeOfDay :: Parser TimeOfDay
timeOfDay = do
  hour <- decimal
  char ':'
  minute <- decimal
  second <- option 0 $ do
    char ':'
    decimal
  adjHour <- option hour $ do
    skipSpace
    ap <- satisfy (inClass "AaPp")
    satisfy (inClass "Mm")
    when (hour < 1) $ fail "hour cannot be less than 1"
    when (hour > 12) $ fail "hour cannot be more than 12"
    let hour' = if hour == 12 then 0 else hour
    return $ if inClass "Pp" ap then hour' + 12 else hour'
  when (adjHour > 23) $ fail "hour cannot be more than 23"
  when (minute > 59) $ fail "minute cannot be more than 59"
  when (second > 59) $ fail "second cannot be more than 59"
  return $ TimeOfDay (Hours adjHour)  (Minutes minute)
                     (Seconds second) (NanoSeconds 0)

timeOfDayRange :: Parser (TimeOfDay, TimeOfDay)
timeOfDayRange = fromTo <|> plusMinus <|> singleTime
  where
    singleTime = do
      t <- timeOfDay
      return (t, t)
    fromTo = do
      t1 <- timeOfDay
      skipSpace
      char '-'
      skipSpace
      t2 <- timeOfDay
      return (t1, t2)
    plusMinus = do
      t <- timeOfDay
      skipSpace
      char '±'
      skipSpace
      d <- duration
      return (t `todSub` d, t `todAdd` d)

todAdd = todOp (+)
todSub = todOp (-)

todOp :: TimeInterval i
         => (Int64 -> Int64 -> Int64)
         -> TimeOfDay
         -> i
         -> TimeOfDay
todOp op (TimeOfDay (Hours h) (Minutes m) (Seconds s) (NanoSeconds ns)) ti =
  let todSecs = h * 3600 + m * 60 + s
      (Seconds iSecs) = toSeconds ti
      newSecs = todSecs `op` iSecs
      -- saturate at beginning or end of day
      secInDay = 24 * 3600
      satSecs = if newSecs < 0 then 0
                else if newSecs >= secInDay then secInDay - 1 else newSecs
      (q, s') = satSecs `quotRem` 60
      (h', m') = q `quotRem` 60
  in TimeOfDay (Hours h') (Minutes m') (Seconds s') (NanoSeconds ns)
