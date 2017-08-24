module System.Hardware.Lifx.Lan.LowLevel.BaseTypes where

import Control.Applicative
import Control.Arrow
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

-- | The power state of a bulb.
data Power = Off | On deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | The name of a network interface, such as @en1@ or @eth0@.
type Interface = T.Text

-- | Color of light, specified as
-- <https://en.wikipedia.org/wiki/HSL_and_HSV hue, saturation, brightness>,
-- and <https://en.wikipedia.org/wiki/Color_temperature kelvin>.
data HSBK a =
  HSBK
  { hue :: a
  , saturation :: a
  , brightness :: a
  , kelvin :: a
  } deriving (Show, Read, Eq, Ord)

-- | The 6-byte ID of a single light, which is also its
-- <https://en.wikipedia.org/wiki/MAC_address MAC address>.
newtype DeviceId   = DeviceId B.ByteString   deriving (Eq, Ord)

-- | The 16-byte ID of a group.
newtype GroupId    = GroupId B.ByteString    deriving (Eq, Ord)

-- | The 16-byte ID of a location.
newtype LocationId = LocationId B.ByteString deriving (Eq, Ord)

-- | The human-readable label of a light, group, or location.
-- May be up to 32 bytes of UTF-8 encoded text.
newtype Label      = Label B.ByteString      deriving (Eq, Ord)

-- | This class contains methods for encoding and decoding the
-- various ID types.  For most of the ID types, which are binary,
-- the 'T.Text' representation is a base16 string encoding of the
-- 'B.ByteString' representation.  For the 'Label' type, which is text,
-- the 'B.ByteString' representation is the UTF-8 encoding of the
-- 'T.Text' representation, padded with @0@ bytes to be 32 bytes long.
class LifxId t where
  -- | Convert an ID to a 'B.ByteString'.
  toByteString :: t -> B.ByteString
  -- | Create an ID from a 'B.ByteString'.  Returns an error message
  -- if the input is invalid.
  fromByteString :: B.ByteString -> Either String t
  -- | Convert an ID to 'T.Text'.
  toText :: t -> T.Text
  -- | Create an ID from 'T.Text'.  Returns an error message if the
  -- input is invalid.
  fromText :: T.Text -> Either String t

checkLength :: String -> Int -> B.ByteString -> Either String B.ByteString
checkLength tname len bs
  | bslen == len = Right bs
  | otherwise =
      Left ("when constructing " ++ tname
            ++ " from ByteString, expected "
            ++ show len ++ " bytes, but got " ++ show bslen)
  where bslen = B.length bs

idToText :: B.ByteString -> T.Text
idToText bs = TE.decodeUtf8 $ B16.encode bs

textToId :: String -> Int -> T.Text -> Either String B.ByteString
textToId tname len txt
  | extralen /= 0 =
      Left ("Got crud " ++ show extra ++ " after " ++ tname)
  | bslen == len = Right bs
  | otherwise =
      Left ("when constructing " ++ tname
            ++ " from Text, expected "
            ++ show len ++ " bytes, but got " ++ show bslen)
  where bslen = B.length bs
        (bs, extra) = B16.decode $ TE.encodeUtf8 txt
        extralen = B.length extra

implShow :: B.ByteString -> String -> String
implShow bs post = B8.unpack (B16.encode bs) ++ post

implRead :: (B.ByteString -> a) -> Int -> String -> [(a, String)]
implRead c len s =
  let digs = len * 2
      digs' = digs + 2
      (bs, _ ) = B16.decode (B8.pack $ take digs' s)
  in if B.length bs == len
     then [(c bs, drop digs s)]
     else []

deviceIdLen = 6

instance LifxId DeviceId where
  toByteString (DeviceId bs) = bs
  fromByteString bs = DeviceId <$> checkLength "DeviceId" deviceIdLen bs
  toText (DeviceId bs) = idToText bs
  fromText txt = DeviceId <$> textToId "DeviceId" deviceIdLen txt

instance Show DeviceId where
  showsPrec _ (DeviceId bs) post = implShow bs post

instance Read DeviceId where
  readsPrec _ s = implRead DeviceId deviceIdLen s

instance Binary DeviceId where
  put (DeviceId bs) = putByteString bs
  get = DeviceId <$> getByteString deviceIdLen

groupIdLen = 16

instance LifxId GroupId where
  toByteString (GroupId bs) = bs
  fromByteString bs = GroupId <$> checkLength "GroupId" groupIdLen bs
  toText (GroupId bs) = idToText bs
  fromText txt = GroupId <$> textToId "GroupId" groupIdLen txt

instance Show GroupId where
  showsPrec _ (GroupId bs) pre = implShow bs pre

instance Read GroupId where
  readsPrec _ s = implRead GroupId groupIdLen s

instance Binary GroupId where
  put (GroupId bs) = putByteString bs
  get = GroupId <$> getByteString groupIdLen

locationIdLen = 16

instance LifxId LocationId where
  toByteString (LocationId bs) = bs
  fromByteString bs = LocationId <$> checkLength "LocationId" locationIdLen bs
  toText (LocationId bs) = idToText bs
  fromText txt = LocationId <$> textToId "LocationId" locationIdLen txt

instance Show LocationId where
  showsPrec _ (LocationId bs) pre = implShow bs pre

instance Read LocationId where
  readsPrec _ s = implRead LocationId locationIdLen s

instance Binary LocationId where
  put (LocationId bs) = putByteString bs
  get = LocationId <$> getByteString locationIdLen

labelLen = 32

labelFromText txt = Label $ textToPaddedByteString labelLen txt

instance LifxId Label where
  toByteString (Label bs) = bs
  fromByteString bs = Label <$> checkLength "Label" labelLen bs
  toText (Label bs) =
    TE.decodeUtf8With TEE.lenientDecode $ B.takeWhile (/= 0) bs
  fromText txt = Right $ labelFromText txt

instance Show Label where
  showsPrec p lbl pre = showsPrec p (toText lbl) pre

instance Read Label where
  readsPrec p s = map (first labelFromText) $ readsPrec p s

instance Binary Label where
  put (Label bs) = putByteString bs
  get = Label <$> getByteString labelLen

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
