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
  , cName :: (Int -> T.Text) -- render column name, given a width
  }

namePair :: Column -> (T.Text, T.Text)
namePair col = (cName col (cMinWidth col), cName col (cMaxWidth col))

colTuple :: Column -> (Direction, Direction, Int, Int, Int, T.Text, T.Text)
colTuple col =
  (cJustify col, cTruncate col, cMinWidth col, cMaxWidth col,
   cPriority col, shortName, longName)
  where (shortName, longName) = namePair col

instance Eq Column where
  c1 == c2 = colTuple c1 == colTuple c2

instance Ord Column where
  compare c1 c2 = compare (colTuple c1) (colTuple c2)

instance Show Column where
  showsPrec _ col pre =
    shows (namePair col)
    $ shows (cPriority col)
    $ shows (cMaxWidth col)
    $ shows (cMinWidth col)
    $ shows (cTruncate col)
    $ shows (cJustify col)
    $ pre ++ "Column "

data FixedColumn =
  FixedColumn
  { rJustify :: !Direction
  , rTruncate :: !Direction
  , rWidth :: !Int
  , rName :: T.Text
  } deriving (Eq, Ord, Read, Show)

convertCol :: Column -> Int -> FixedColumn
convertCol col width =
  FixedColumn
  { rJustify = cJustify col
  , rTruncate = cTruncate col
  , rWidth = width
  , rName = (cName col) width
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

displayCol :: FixedColumn -> LT.Text -> LT.Text
displayCol col txt
  | txtLen == width = txt
  | txtLen < width = pad (rJustify col)
  | otherwise = trunc (rTruncate col)
  where txtLen = LT.length txt
        width = fromIntegral $ rWidth col
        pad Lft = LT.justifyLeft width ' ' txt
        pad Rgt = LT.justifyRight width ' ' txt
        trunc Lft = LT.take width txt
        trunc Rgt = LT.takeEnd width txt

displayRow :: [FixedColumn] -> [LT.Text] -> LT.Text
displayRow cols txts =
  LT.intercalate spc $ map (uncurry displayCol) $ zip cols txts
  where spc = LT.singleton ' '

displayHeader :: [FixedColumn] -> LT.Text
displayHeader cols = displayRow cols $ map (LT.fromStrict . rName) cols

displaySep :: [FixedColumn] -> LT.Text
displaySep cols = displayRow cols $ repeat $ LT.repeat '-'
