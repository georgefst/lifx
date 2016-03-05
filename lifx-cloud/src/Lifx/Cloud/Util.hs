module Lifx.Cloud.Util where

import Data.Hourglass

data MyISO8601_DateAndTime = MyISO8601_DateAndTime
    deriving (Show,Eq)

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
