-- | This library is for drawing text tables.
--
-- Pass a 2D-list of strings and get a single string with table contents.
--
-- @
-- makeDefaultSimpleTable :: [[String]] -> String
-- @
module Text.SimpleTableGenerator (
  makeSimpleTable,
  makeDefaultSimpleTable,
  --
  SimpleTableConfig (..),
  --
  simpleTableConfig,
  --
  simpleTableLeftPad,
  simpleTableCenterPad,
  simpleTableRightPad,
  --
  simpleTableBottomPad,
  simpleTableMiddlePad,
  simpleTableTopPad,
  ) where

import Data.List.Split (splitOn)
import Data.List (transpose)

-- types:
type CellLine = String  -- cells can be multiline
type Cell  = [CellLine]
type Row   = [Cell]
type Table = [Row]
type CellSize  = (Int, Int)
type CellSizeTable = [[CellSize]]
type TextTable = [[String]]

-- wouldn't be exported
data CellWrapper =
  CellWrapper {
    cell :: Cell,
    rowNum :: Int, colNum :: Int, cellWidth :: Int, cellHeight :: Int,
    topLeft :: String, top :: String, topRight :: String, right :: String, bottomRight :: String,
    bottom :: String, bottomLeft :: String, left :: String
  } deriving (Show)

-- | Data type that represents table configuration.
data SimpleTableConfig =
  SimpleTableConfig {
  -- | String containing table border characters, in order like this: "┌┬┐├┼┤└┴┘─│".
  -- Must be exactly 11 characters long, otherwise error will be thrown.
  tableBorders :: String,
  -- | Minimum widths for each column, from left to right. Padding size is not counted.
  -- List's length may not match the actual count of columns in a table.
  colMinWidths :: [Int],
  -- | Minimum heights for each row, from top to bottom. Padding size is not counted.
  -- List's length may not match the actual count of rows in a table.
  rowMinHeights :: [Int],
  padFunction :: String -> Int -> String -> String,
  cellPadFunction :: String -> Int -> [String] -> [String],
  -- | Width of left and right margins.
  horizontalPadding :: Int,
  -- | Height of top and bottom margins.
  verticalPadding :: Int,
  -- | String used as padding. \" \" (space) by default.
  paddingStr :: String,
  -- | String used to fill in empty cells. \"\" by default.
  -- Empty cells will be padded with 'paddingStr' after placing 'emptyCellStr' into each them.
  emptyCellStr :: String
  }

-- | Default table config.
simpleTableConfig =
  SimpleTableConfig {
  tableBorders = "┌┬┐├┼┤└┴┘─│",
  colMinWidths  = [],
  rowMinHeights = [],
  padFunction   = simpleTableRightPad,
  cellPadFunction = simpleTableBottomPad,
  horizontalPadding = 1,
  verticalPadding = 0,
  paddingStr = " ",
  emptyCellStr = ""
  }

-- | Create table using 'simpleTableConfig'.
-- Example usage:
--
-- @
-- putStrLn $ makeDefaultSimpleTable [["1","2","3"], [\"One\",\"Two\",\"Three\"], [\"First\", \"Second\"]]
-- @
makeDefaultSimpleTable table =
  makeSimpleTable simpleTableConfig table

-- | Example usage:
--
-- @
-- putStrLn $ makeSimpleTable simpleTableConfig {
--   tableBorders = "+++++++++-|",
--   colMinWidths  = [3, 4],
--   rowMinHeights = [2],
--   padFunction   = 'simpleTableLeftPad',
--   cellPadFunction = 'simpleTableBottomPad',
--   horizontalPadding = 0,
--   verticalPadding = 1,
--   paddingStr = ".,`"
-- } [["a"], ["b", "c"]]
-- @
makeSimpleTable :: SimpleTableConfig -> [[String]] -> String
makeSimpleTable config table =
    showTable $
    map2d cell $
    appendBorders $
    normalizeBorderLengths $
    wrapTable processedConfig $
    padTableCells processedConfig $
    makeCells $
    normalizeColumnCount processedConfig $ table
    where
      processedConfig =
        constructPaddingFunctions $ validateConfig config

-- | Put something in empty cells
normalizeColumnCount :: SimpleTableConfig -> TextTable -> TextTable
normalizeColumnCount config = normalizeColumnCountWithStr (emptyCellStr config)

normalizeColumnCountWithStr :: String -> TextTable -> TextTable
normalizeColumnCountWithStr emptyCellStr textTable =
    map (\row -> addExtraCells row) textTable
    where
        addExtraCells row
            | length row < columnCount = row ++ (take (columnCount - length row)
                $ repeat emptyCellStr)
            | otherwise = row
        columnCount = fst $ get2DListSize textTable

-- | convert TextTable to Table by splitting each cell
-- line by line
makeCells :: TextTable -> Table
makeCells textTable =
    map (\rowStr  -> map
            (\cellStr -> splitOn "\n" cellStr) rowStr) textTable
    where
        splitCell :: String -> Cell
        splitCell cellStr = splitOn "\n" cellStr

get2DListSize :: [[a]] -> (Int, Int)
get2DListSize list2d = (maximum $ map length list2d, length list2d)

padTableCells :: SimpleTableConfig -> Table -> Table
padTableCells config table = (padCellLines . addCellLines) table
  where
    -- add empty `CellLine`s to each `Cell`
    addCellLines table = zipWith addExtraCellLines table realRowHeights
    addExtraCellLines row height = map
      (\cell -> (cellPadFunction config) "\n" height cell) row
    -- adds extra spaces to each `CellLine`
    padCellLines table = transpose $
      zipWith padCellList (transpose table) realColWidths

    padCellList col width = map (\cell -> map
                                  (\celLine ->
                                   (padFunction config)
                                   (paddingStr config)
                                   width celLine)
                                  cell) col

    -- Calculate real column widths.
    -- Minimum column widths are limited by values from config.
    realColWidths = zipWith max (colWidths cellSizeTable) ((colMinWidths config) ++ (repeat 0))
    realRowHeights = zipWith max (rowHeights cellSizeTable) ((rowMinHeights config) ++ (repeat 0))
    cellSizeTable = map2d get2DListSize table

    -- list of heights for each row
    rowHeights :: CellSizeTable -> [Int]
    rowHeights sizeTable = maxOfMap2 snd sizeTable

    -- list of widths for each column
    colWidths :: CellSizeTable -> [Int]
    colWidths sizeTable  = maxOfMap2 fst $ transpose sizeTable

    maxOfMap2 :: (a -> Int) -> [[a]] -> [Int]
    maxOfMap2 f = map (\sth -> maximum $ map f sth)

-- | Join 'CellLine's
wrapTable config table = wrapCells $ addCellCoords table
  where
    -- Add cell coordinates for each cell.
    -- e.g. [["a", "b"]] will be transformed to [[(1,1,"a"), (1,2,"b")]]
    addCellCoords :: Table -> [[(Int, Int, Cell)]]
    addCellCoords table = zipWith
      (\ rowNum list ->
       -- Insert row numbers.
        map (\ (colNum, cell) ->
                 (rowNum, colNum, cell)) list) [1..] $
      -- Enumerate columns
      map (zip [1..]) table
    wrapCells :: [[(Int, Int, Cell)]] -> [[CellWrapper]]
    wrapCells = map2d wrapCell
    wrapCell :: (Int, Int, Cell) -> CellWrapper
    wrapCell (rowNum, colNum, cell) =
      (CellWrapper cell rowNum colNum
       -- cell width
       (maximum $ map length cell)
       -- cell height
       (length cell)
       topLeft top topRight right bottomRight bottom bottomLeft left)
      where
        (width, height) = get2DListSize table
        borders =  tableBorders config
        topLeft
          | rowNum == 1 && colNum == 1 = [borders !! 0]          -- ┌
          | colNum == 1 = [borders !! 3]                        -- ├
          | rowNum == 1 = [borders !! 1]                        -- ┬
          | otherwise = [borders !! 4]                          -- ┼
        topRight
          | rowNum == 1 && colNum == width = [borders !! 2]      -- ┐
          | rowNum /= 1 && colNum == width = [borders !! 5]      -- ┤
          | otherwise = ""
        right
          | colNum == width = [borders !! 10]
          | otherwise = ""
        bottomRight
          | rowNum == height && colNum == width = [borders !! 8] -- ┘
          | rowNum == height = [borders !! 7]                   -- ┴
          | colNum == width = [borders !! 5]                    -- ┤
          | otherwise = ""
        bottom
          | rowNum == height = [borders !! 9]                   -- ─
          | otherwise = ""
        bottomLeft
          | rowNum == height && colNum == 1 = [borders !! 6]     -- └
          | rowNum == height = [borders !! 7]                   -- └
          | otherwise  = ""
        top = [borders !! 9]                                    -- ─
        left = [borders !! 10]                                  -- │

normalizeBorderLengths :: [[CellWrapper]] -> [[CellWrapper]]
normalizeBorderLengths =
  map2d normalizeBorderLength
  where
    normalizeBorderLength :: CellWrapper -> CellWrapper
    normalizeBorderLength
      (CellWrapper cell rowNum colNum cellWidth cellHeight topLeft
       top topRight right bottomRight bottom bottomLeft left) =
      (CellWrapper cell rowNum colNum cellWidth cellHeight topLeft
       (takeOf cellWidth top) topRight
       right bottomRight
       (takeOf cellWidth bottom) bottomLeft
       left)
      where
        takeOf n sth =
          take n $ concat $ repeat sth

appendBorders :: [[CellWrapper]] -> [[CellWrapper]]
appendBorders table =
  map2d appendAll table
  where
    appendAll
      (CellWrapper cell rowNum colNum cellWidth cellHeight topLeft
       top topRight right bottomRight bottom bottomLeft left) =
      (CellWrapper
       ((
         -- append bottomLeft
         (\ cell ->
           if rowNum == height then
             (init cell) ++ [bottomLeft ++ last cell]
           else
             cell) .
         -- append bottomRight
         (\ cell ->
          if colNum == width && rowNum == height then
            (init cell) ++ [last cell ++ bottomRight]
            else
          cell) .
         -- append bottom
         (\ cell ->
          if rowNum == height then
            cell ++ [bottom]
            else
            cell) .
         -- append right
         (\ cell ->
          if colNum == width then
             [head cell] ++ (zipWith (++) (tail cell) (repeat right))
           else
             cell) .
         -- append topRight
         (\ cell ->
           (head cell ++ topRight):(tail cell)) .
         -- appendt left
         (\ cell ->
           [head cell] ++ zipWith (++) (repeat left) (tail cell)) .
         -- append topLeft
         (\ cell -> (concat (topLeft : [head cell])):(tail cell)) .
         -- appent top
         (\ cell -> top:cell))
         cell)
       rowNum colNum cellWidth cellHeight topLeft
       top topRight right bottomRight bottom bottomLeft left)
    width  = fst $ get2DListSize table
    height = snd $ get2DListSize table


-- part 5: join cells & rows

showTable :: [[[String]]] -> String
showTable textTable = strJoin "\n" $
  map (strJoin "\n") $
  map2d (strJoin "") $
  map transpose textTable
    where
      strJoin :: String -> [String] -> String
      strJoin separator lst = if null lst then
                                ""
                              else
                                foldr1 (\x y -> x ++ separator ++ y) lst

-- | Horizontal padding function.
-- Appends padding string (first argument) to the left of the given string to make
-- it's length equal to the second argument.
simpleTableLeftPad :: String -> Int -> String -> String
simpleTableLeftPad paddingStr width str
  | length str > width = error "SimpleTableGenerator: String's length is greater than maximum!"
  | otherwise = padding ++ str
    where
      padding = take (width - length str) $ concat $ repeat paddingStr

-- | Horizontal padding function.
-- Appends padding string (first argument) to the right of the given string to make
-- it's length equal to the second argument.
simpleTableRightPad :: String -> Int -> String -> String
simpleTableRightPad paddingStr width str
  | length str > width = error "SimpleTableGenerator: String's length is greater than maximum!"
  | otherwise = str ++ padding
    where
      padding = take (width - length str) $ concat $ repeat paddingStr

-- | Horizontal padding function.
-- Appends padding string (first argument) both to the right and left of the given string to make
-- it's length equal to the second argument.
simpleTableCenterPad :: String -> Int -> String -> String
simpleTableCenterPad paddingStr width str
  | length str > width = error "SimpleTableGenerator: String's length is greater than maximum!"
  | even (width - length str) = halfPadding ++ str ++ halfPadding
  | otherwise = halfPadding ++ str ++ take (halfWidth + 1) padding
    where
      halfWidth = ((width - length str) `div` 2)
      padding = concat $ repeat paddingStr
      halfPadding = take halfWidth padding

-- | Vertical padding function.
-- Appends padding to the bottom of given 'Cell'
simpleTableBottomPad :: String -> Int -> Cell -> [String]
simpleTableBottomPad cellStr height cell
  | length cell > height = error "SimpleTableGenerator: Cell's height is greater than maximum!"
  | length cell == height = cell
  | otherwise = cell ++ padding
    where
      padding = replicate (height - length cell) ""

-- | Vertical padding function.
-- Appends padding to the top of given 'Cell'
simpleTableTopPad :: String -> Int -> Cell -> [String]
simpleTableTopPad cellStr height cell
  | length cell > height = error "SimpleTableGenerator: Cell's height is greater than maximum!"
  | length cell == height = cell
  | otherwise = padding ++ cell
    where
      padding = replicate (height - length cell) ""

-- | Vertical padding function.
-- Appends padding both to top and bottom of given 'Cell'
simpleTableMiddlePad :: String -> Int -> Cell -> [String]
simpleTableMiddlePad cellStr height cell
  | length cell > height = error "SimpleTableGenerator: Cell's height is greater than maximum!"
  | length cell == height = cell
  | even (height - length cell) = halfPadding ++ cell ++ halfPadding
  | otherwise = halfPadding ++ cell ++ halfPadding ++ [""]
    where
      halfPadding = replicate ((height - length cell) `div` 2) ""

constructPaddingFunctions :: SimpleTableConfig -> SimpleTableConfig
constructPaddingFunctions config = config {
  padFunction = (\ f padStr width ->
                     let padding = take (horizontalPadding config) $
                               concat $ repeat (paddingStr config) in
                       padding  ++ (f padStr width) ++ padding)
                . (padFunction config),
  cellPadFunction =
      (\ f cellStr height ->
           let padding = (concat
                          $ take (verticalPadding config)
                                $ repeat [""]) in
           padding ++ (f cellStr height) ++ padding) .
      (cellPadFunction config),
  horizontalPadding = 0,
  verticalPadding = 0
  }

validateConfig config
  | 0 == length (paddingStr config) = error "SimpleTableGenerator: paddingStr is empty!"
  | 11 /= length (tableBorders config) = error "SimpleTableGenerator: tableBorders must be a string of 11 characters!"
  | 0 > horizontalPadding config = error "SimpleTableGenerator: horizontalPadding must be >= 0!"
  | 0 > verticalPadding config = error "SimpleTableGenerator: verticalPadding must be >= 0!"
  | otherwise = config

-- misc functions

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d =  map . map
