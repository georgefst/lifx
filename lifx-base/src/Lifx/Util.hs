module Lifx.Util where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Data.Attoparsec.Text
import Data.Binary
import qualified Data.ByteString as B
import Data.Int
import Data.List (find)
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

-- version of 'format' that returns a strict 'T.Text'
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

-- attoparsec error messages are so comically bad, we just return
-- Nothing instead of an error message.  This function also requires
-- that all input be consumed.
parseAllMaybe :: Parser a -> T.Text -> Maybe a
parseAllMaybe p txt = case parseOnly (p <* endOfInput) txt of
                       (Left _ ) -> Nothing
                       (Right x) -> Just x
