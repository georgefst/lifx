module Lifx.Cloud.Util where

import Data.Hourglass
import Data.Int

data MyISO8601_DateAndTime = MyISO8601_DateAndTime
    deriving (Show,Eq)

-- Modified version of Hourglass's ISO8601_DateAndTime, to add milliseconds.
instance TimeFormat MyISO8601_DateAndTime where
    toFormat _ = TimeFormatString
        [Format_Year,dash,Format_Month2,dash,Format_Day2 -- date
        ,Format_Text 'T'
        ,Format_Hour,colon,Format_Minute,colon,Format_Second,dot,Format_MilliSecond -- time
        ,Format_TzHM_Colon -- timezone offset with colon +HH:MM
        ]
      where dash = Format_Text '-'
            colon = Format_Text ':'
            dot = Format_Text '.'

-- drop 3 "Sun, 06 Nov 1994 08:49:37 GMT"
-- this expects ", 06 Nov 1994 08:49:37 GMT"
data MyRFC1123_DateAndTime = MyRFC1123_DateAndTime
                           deriving (Show, Eq)

instance TimeFormat MyRFC1123_DateAndTime where
  toFormat _ = TimeFormatString
               [ Format_Text ','
               , Format_Spaces
               , Format_Day2
               , Format_Spaces
               , Format_MonthName_Short
               , Format_Spaces
               , Format_Year4
               , Format_Spaces
               , Format_Hour
               , Format_Text ':'
               , Format_Minute
               , Format_Text ':'
               , Format_Second
               , Format_Spaces
               , Format_Text 'G'
               , Format_Text 'M'
               , Format_Text 'T'
               ]

ununix :: Int64 -> DateTime
ununix s = timeFromElapsed $ Elapsed $ Seconds s
