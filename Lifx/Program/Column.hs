import Data.List
import qualified Data.Text as T

data Direction = Lft | Rgt deriving (Eq, Ord, Read, Show)

data Column =
  Column
  { cJustify :: !Direction
  , cTruncate :: !Direction
  , cMinWidth :: !Int
  , cMaxWidth :: !Int
  , cPriority :: !Double -- width is allocated to columns by priority
  , cName :: (Int -> T.Text) -- render column name, given a width
  }

data RenderedColumn =
  RenderedColumn
  { rJustify :: !Direction
  , rTruncate :: !Direction
  , rWidth :: !Int
  , rName :: T.Text
  } deriving (Eq, Ord, Read, Show)

convertCol :: Column -> Int -> RenderedColumn
convertCol col width =
  RenderedColumn
  { rJustify = cJustify col
  , rTruncate = cTruncate col
  , rWidth = width
  , rName = (cName col) width
  }

expandCols :: Int -> [(Int, Column)] -> [(Int, RenderedColumn)]
expandCols extra ((pri, col) : rest) =
  foo
  where something

renderColumns :: Int -> [Column] -> [RenderedColumn]
renderColumns width cols =
  if minWidth >= width
  then truncateCols
  else map snd $ sortBy byFst $ expandCols (width - minWidth) pcols
  where minWidth = sum (length cols - 1 : map cMinWidth cols)
        ocols = zip [1..] cols
        pcols = sortBy byPriority ocols
        -- sort in descending order of priority (hence c1 and c2 are swapped)
        byPriority c1 c2 = compare (cPriority $ snd c2) (cPriority $ snd c1)
        -- used to restore columns to their original order
        byFst c1 c2 = compare (fst c1) (fst c2)
