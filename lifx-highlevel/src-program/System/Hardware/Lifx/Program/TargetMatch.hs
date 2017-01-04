module System.Hardware.Lifx.Program.TargetMatch where

import qualified Data.Set as S
import qualified Data.Text as T

import System.Hardware.Lifx

data Targets = TargAll | TargSome (S.Set TargetMatch)
               deriving (Show, Read, Eq, Ord)

data TargetMatch = TmLabel      T.Text
                 | TmDevId      T.Text
                 | TmGroup      T.Text
                 | TmGroupId    T.Text
                 | TmLocation   T.Text
                 | TmLocationId T.Text
                   deriving (Show, Read, Eq, Ord)

data LiteIds =
  LiteIds
  { liDevId    :: DeviceId
  , liLabel    :: Maybe Label
  , liGroupId  :: Maybe GroupId
  , liGroup    :: Maybe Label
  , liLocId    :: Maybe LocationId
  , liLoc      :: Maybe Label
  } deriving (Show, Read, Eq, Ord)

mkLiteIds :: DeviceId -> LiteIds
mkLiteIds d = LiteIds
  { liDevId   = d
  , liLabel   = Nothing
  , liGroupId = Nothing
  , liGroup   = Nothing
  , liLocId   = Nothing
  , liLoc     = Nothing
  }

tmatch :: TargetMatch -> LiteIds -> Bool
tmatch (TmDevId t)      (LiteIds { liDevId   =       x  }) = matchId  t x
tmatch (TmLabel t)      (LiteIds { liLabel   = (Just x) }) = matchLab t x
tmatch (TmGroupId t)    (LiteIds { liGroupId = (Just x) }) = matchId  t x
tmatch (TmGroup t)      (LiteIds { liGroup   = (Just x) }) = matchLab t x
tmatch (TmLocationId t) (LiteIds { liLocId   = (Just x) }) = matchId  t x
tmatch (TmLocation t)   (LiteIds { liLoc     = (Just x) }) = matchLab t x
tmatch _ _ = False

matchId :: LifxId i => T.Text -> i -> Bool
matchId t1 x = t1 `T.isSuffixOf` t2
  where t2 = toText x

matchLab :: T.Text -> Label -> Bool;
matchLab t1 x = t1 `T.isPrefixOf` t2
  where t2 = toText x
