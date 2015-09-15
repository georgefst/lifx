module Lifx.Util where

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Control.Arrow (first)
import Control.Monad
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Parser)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as L
import Data.Hourglass
import Data.Int
import Data.List (find)
import Data.Maybe
import Data.Monoid (Monoid(..))
import qualified Data.Set as S
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Text.Format
import Data.Text.Format.Params
import qualified Data.Text.Lazy as LT
import qualified Data.UUID.Types as U
import Data.Version
import Data.Word
import Debug.Trace
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadPrec (readPrec_to_S)
import Text.Read hiding (String)

---- Utilities: move elsewhere?

-- readEither has been in Text.Read since base 4.6,
-- but we have our own copy here to work with base 4.5.
-- BSD3, (c) The University of Glasgow 2001
readEither' :: Read a => String -> Either String a
readEither' s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift skipSpaces
       return x

fmt :: Params ps => Format -> ps -> T.Text
fmt f p = LT.toStrict $ format f p

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

