module Lifx.Program.Column
       ( Direction (..)
       , Column (..)
       , FixedColumn (..)
       , fixColumns
       , displayRow
       , displayHeader
       , displaySep
       ) where

import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

data Direction = Lft | Rgt deriving (Eq, Ord, Read, Show)

data Column =
  Column
  { cJustify :: !Direction
  , cTruncate :: !Direction
  , cMinWidth :: !Int
  , cMaxWidth :: !Int
  , cPriority :: !Int -- width is allocated to columns by priority
  , cName :: [T.Text] -- list of possible column names of different widths
  } deriving (Eq, Ord, Read, Show)

data FixedColumn =
  FixedColumn
  { rJustify :: !Direction
  , rTruncate :: !Direction
  , rWidth :: !Int
  , rName :: T.Text
  } deriving (Eq, Ord, Read, Show)

pickBest :: [T.Text] -> Int -> T.Text
pickBest names' width' = fromMaybe T.empty $ pb names' width'
  where pb [] _ = Nothing
        pb (name:names) width = Just $ better width name $ pb names width
        better _ n Nothing = n
        better w n1 (Just n2)
          | T.length n1 <= w && T.length n2 > w = n1
          | T.length n2 <= w && T.length n1 > w = n2
          | T.length n1 <= w && T.length n1 > T.length n2 = n1
          | T.length n1 <= w && T.length n1 <= T.length n2 = n2
          | T.length n1 < T.length n2 = n1
          | otherwise = n2

convertCol :: Column -> Int -> FixedColumn
convertCol col width =
  FixedColumn
  { rJustify = cJustify col
  , rTruncate = cTruncate col
  , rWidth = width
  , rName = pickBest (cName col) width
  }

expandCols :: Int -> [(Int, Column)] -> [(Int, FixedColumn)]
expandCols _ [] = []
expandCols budget ((orig, col) : rest) =
  (orig, convertCol col (cMinWidth col + moreWidth))
  : expandCols (budget - moreWidth) rest
  where desired = cMaxWidth col - cMinWidth col
        moreWidth | desired > budget = budget
                  | otherwise = desired

truncateCols :: Int -> [Column] -> [FixedColumn]
truncateCols _ [] = []
truncateCols budget (col : rest)
  | budget <= 0 = []
  | otherwise = convertCol col width : truncateCols (budget - width - 1) rest
  where minWidth = cMinWidth col
        width | minWidth < budget = minWidth
              | otherwise = budget

fixColumns :: Int -> [Column] -> [FixedColumn]
fixColumns width cols =
  if minWidth >= width
  then truncateCols width cols
  else map snd $ sortBy byFst $ expandCols (width - minWidth) pcols
  where minWidth = sum (length cols - 1 : map cMinWidth cols)
        ocols = zip [1..] cols
        pcols = sortBy byPriority ocols
        -- sort in descending order of priority (hence c1 and c2 are swapped)
        byPriority c1 c2 = compare (cPriority $ snd c2) (cPriority $ snd c1)
        -- used to restore columns to their original order
        byFst c1 c2 = compare (fst c1) (fst c2)

displayCol :: FixedColumn -> T.Text -> T.Text
displayCol col txt
  | txtLen == width = txt
  | txtLen < width = pad (rJustify col)
  | otherwise = trunc (rTruncate col)
  where txtLen = T.length txt
        width = fromIntegral $ rWidth col
        pad Lft = T.justifyLeft width ' ' txt
        pad Rgt = T.justifyRight width ' ' txt
        trunc Lft = T.take width txt
        trunc Rgt = T.takeEnd width txt

displayRow :: [FixedColumn] -> [T.Text] -> T.Text
displayRow cols txts =
  T.intercalate spc $ map (uncurry displayCol) $ zip cols txts
  where spc = T.singleton ' '

displayHeader :: [FixedColumn] -> T.Text
displayHeader cols = displayRow cols $ map rName cols

dashes :: T.Text
dashes = T.replicate 100 $ T.singleton '-'

displaySep :: [FixedColumn] -> T.Text
displaySep cols = displayRow cols $ repeat $ dashes
