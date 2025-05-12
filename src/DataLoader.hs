{-# LANGUAGE OverloadedStrings #-}

module DataLoader (
    StockPricesRow(..),
    StockEntry(..),
    loadStockCsv,
    expandRows,
    calculateReturns,
    returnsToMatrix,
    loadData
) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.List (groupBy, nub, concatMap, sortOn)
import Data.Function (on)
import Data.Csv (decodeByName, parseField, FromNamedRecord(..), FromField(..), (.:), Parser)
import Data.Text (Text, unpack)
import Data.HashMap.Strict (toList)
import Prelude hiding (concatMap)

-- | A row of stock prices for multiple tickers at a given date
data StockPricesRow = StockPricesRow
  { date   :: Day
  , prices :: Map.Map Text Double
  } deriving (Show)

-- | Parse a day from a ByteString
parseDay :: ByteString -> Parser Day
parseDay bs =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (unpack $ TE.decodeUtf8 bs) of
    Just day -> pure day
    Nothing  -> fail $ "Could not parse date: " ++ unpack (TE.decodeUtf8 bs)

-- Custom parsing for Day since Data.Csv.Instances is unavailable
newtype CsvDay = CsvDay Day
instance FromField CsvDay where
  parseField bs = CsvDay <$> parseDay bs

instance FromNamedRecord StockPricesRow where
  parseNamedRecord m = do
    CsvDay d <- m .: "Date"
    let kvs = [(TE.decodeUtf8 k, v) | (k, v) <- toList m, k /= "Date"]
    priceMap <- traverse (\(k, v) -> (,) k <$> parseField v) kvs
    pure $ StockPricesRow d (Map.fromList priceMap)

-- | Long-format entry: one (Date, Symbol, Price) per record
data StockEntry = StockEntry
  { entryDate   :: Day
  , entrySymbol :: Text
  , entryPrice  :: Double
  } deriving (Show)

-- | Convert wide-format rows into long-format entries (safe, no head)
expandRows :: V.Vector StockPricesRow -> [StockEntry]
expandRows = concatMap toEntries . V.toList
  where
    toEntries (StockPricesRow d mp)
      | Map.null mp = []  -- Defensive: skip empty price maps
      | otherwise = [ StockEntry d sym price | (sym, price) <- Map.toList mp ]

-- | Load and parse the CSV file (handle errors gracefully)
loadStockCsv :: FilePath -> IO (V.Vector StockPricesRow)
loadStockCsv fp = do
  csvData <- BL.readFile fp
  case decodeByName csvData of
    Left err     -> do
      putStrLn $ "CSV parse error: " <> err
      putStrLn "Exiting. Please check your CSV file."
      pure V.empty
    Right (_, v) -> pure v

-- | Calculate daily returns from a list of stock entries
calculateReturns :: [StockEntry] -> [(Day, Map.Map Text Double)]
calculateReturns entries = 
  let sortedEntries = sortOn entryDate entries
      groupedByDate = groupBy ((==) `on` entryDate) sortedEntries
      priceMapByDate = [ case group of
                            (e:_) -> (entryDate e, Map.fromList [(entrySymbol e', entryPrice e') | e' <- group])
                            []    -> error "Empty group in calculateReturns (should not happen)"
                       | group <- groupedByDate ]
  in calculateDailyReturns priceMapByDate
  where
    calculateDailyReturns [] = []
    calculateDailyReturns [_] = []
    calculateDailyReturns (p1:p2:ps) = 
      let (_, prices1) = p1
          (d2, prices2) = p2
          -- Only calculate returns for stocks that have data in both periods
          returns = Map.intersectionWith (\p2' p1' -> 
            if p1' > 0 && p2' > 0  -- Only calculate return if both prices are positive
            then (p2' / p1') - 1  -- This is correct, but we need to ensure data is in correct order
            else 0.0) prices2 prices1
      in (d2, returns) : calculateDailyReturns (p2:ps)

-- | Convert returns to a matrix format (stocks × dates)
returnsToMatrix :: [(Day, Map.Map Text Double)] -> (V.Vector Day, V.Vector Text, V.Vector (V.Vector Double))
returnsToMatrix returns =
  let dates = V.fromList $ map fst returns
      allStocks = V.fromList $ nub $ concatMap (Map.keys . snd) returns
      -- Create a matrix where each row is a stock and each column is a date
      matrix = V.fromList $ map (\stock -> 
        V.fromList [Map.findWithDefault 0.0 stock (snd dateReturn) | dateReturn <- returns]) 
        (V.toList allStocks)
      -- Calculate mean returns for each stock
      meanReturns = V.map (\row -> 
        let validReturns = V.filter (/= 0.0) row
        in if V.null validReturns then 0.0 else V.sum validReturns / fromIntegral (V.length validReturns)) matrix
      -- Replace missing returns with the stock's mean return
      filledMatrix = V.zipWith (\row mean -> 
        V.map (\r -> if r == 0.0 then mean else r) row) matrix meanReturns
  in (dates, allStocks, filledMatrix)

-- | Load and process data efficiently
loadData :: FilePath -> IO (V.Vector (V.Vector Double))
loadData filePath = do
    -- Load and parse the CSV file
    stockRows <- loadStockCsv filePath
    
    -- Sort rows by date to ensure chronological order
    let sortedRows = V.modify (\v -> VAI.sortBy (\a b -> compare (date a) (date b)) v) stockRows
    
    -- Convert to entries and calculate returns
    let entries = expandRows sortedRows
        returns = calculateReturns entries
    
    -- Convert returns to matrix format
    let (_, _, matrix) = returnsToMatrix returns
    
    return matrix 