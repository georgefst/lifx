module System.Hardware.Lifx.Util where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Format
import Data.Text.Format.Params
import qualified Data.Text.Lazy as LT

---- Utilities: move elsewhere?

-- version of 'format' that returns a strict 'T.Text'
fmt :: Params ps => Format -> ps -> T.Text
fmt f p = LT.toStrict $ format f p

-- attoparsec error messages are so comically bad, we just return
-- Nothing instead of an error message.  This function also requires
-- that all input be consumed.
parseAllMaybe :: Parser a -> T.Text -> Maybe a
parseAllMaybe p txt = case parseOnly (p <* endOfInput) txt of
                       (Left _ ) -> Nothing
                       (Right x) -> Just x
