{-# LANGUAGE OverloadedStrings #-}

module Lifx.ToText where

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Buildable

import Lifx.Types
import Lifx.Util

textualize :: Buildable a => (T.Text, Maybe a) -> Maybe T.Text
textualize (_, Nothing) = Nothing
textualize (key, Just value) = Just $ fmt "{}:{}" (key, value)

-- | When given 'emptyColor', returns 'Nothing'.  Otherwise, returns
-- 'Just' a non-empty string which, when fed to 'Lifx.parseColor', would
-- parse as the given color.
colorToText :: PartialColor -> Maybe T.Text
colorToText c@(HSBK h s b k)
  | isEmptyColor c = Nothing
  | otherwise =
      -- kelvin has to go before saturation because of weird behavior:
      -- https://community.lifx.com/t/interpolating-colors-whites/573/8
      -- Also, kelvin has to be an Int and not a Double like the others,
      -- or else we get RemoteError "Unable to parse color: kelvin:5000.00"
      let k' = ("kelvin", fmap round k :: Maybe Int)
          hsb = zip ["hue", "saturation", "brightness"] [h, s, b]
          components1 = mapMaybe textualize [k']
          components2 = mapMaybe textualize hsb
      in Just $ T.intercalate " " (components1 ++ components2)

-- | Renders a 'Selector' in
-- <http://api.developer.lifx.com/docs/selectors the same format> accepted
-- by the LIFX Cloud API.
selectorToText :: Selector -> T.Text
selectorToText SelAll = "all"
selectorToText (SelLabel x)      = "label:"       <> toText x
selectorToText (SelDevId x)      = "id:"          <> toText x
selectorToText (SelGroup x)      = "group:"       <> toText x
selectorToText (SelGroupId x)    = "group_id:"    <> toText x
selectorToText (SelLocation x)   = "location:"    <> toText x
selectorToText (SelLocationId x) = "location_id:" <> toText x
selectorToText (SelSceneId x)    = "scene_id:"    <> toText x

-- | Renders a list of 'Selector's as a comma-separated string.
selectorsToText :: [Selector] -> Either LifxException T.Text
selectorsToText sels = do
  let illegalChars = ",/" :: String
      checkChars t = checkChars' t $ T.find (`elem` illegalChars) t
      checkChars' t Nothing = Right t
      checkChars' _ (Just c) = Left $ IllegalCharacter c
  txts <- mapM (checkChars . selectorToText) sels
  return $ T.intercalate "," txts
