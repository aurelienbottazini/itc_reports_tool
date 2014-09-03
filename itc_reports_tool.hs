-- monthly reports available only for last 12 month
-- daily reports only available for last 30 days
import Codec.Compression.GZip as GZip
import Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List
import Data.String.Utils
import Data.Time
import System.Cmd
import System.Directory
import System.Environment
import System.Locale
import Text.Regex.Posix
import qualified Data.Map as Map

regionCodes :: [(String, String)]
regionCodes = [("AE", "United Arab Emirates"),
               ("AU", "Australia"),
               ("CA", "Canada"),
               ("CH", "Switzerland"),
               ("DK", "Denmark"),
               ("EU", "Euro-Zone"),
               ("GB", "United Kingdom"),
               ("HK", "Hong Kong"),
               ("ID", "Indonesia"),
               ("IL", "Israel"),
               ("IN", "India"),
               ("JP", "Japan"),
               ("MX", "Mexico"),
               ("NO", "Norway"),
               ("NZ", "New Zealand"),
               ("RU", "Russia"),
               ("SA", "Saudi Arabia"),
               ("SE", "Sweden"),
               ("SG", "Singapore"),
               ("TR", "Turkey"),
               ("TW", "Taiwan"),
               ("US", "United States"),
               ("WW", "Rest of World"),
               ("ZA", "South Africa")]

-- approximative conversion to euro. Might need a refactoring to
-- update rates from internet
convertToEuros :: String -> Float -> Float
convertToEuros currency money
    | "AED" == currency = money / 4.84
    | "AUD" == currency = money / 1.41
    | "CAD" == currency = money / 1.43
    | "CHF" == currency = money / 1.21
    | "CNY" == currency = money / 8.11
    | "DKK" == currency = money / 7.45
    | "EUR" == currency = money
    | "GBP" == currency = money / 0.79
    | "HKD" == currency = money / 10.22
    | "IDR" == currency = money / 15435.3
    | "ILS" == currency = money / 4.71
    | "INR" == currency = money / 79.8
    | "JPY" == currency = money / 137.05
    | "MXN" == currency = money / 17.26
    | "NOK" == currency = money / 8.16
    | "NZD" == currency = money / 1.57
    | "RUB" == currency = money / 48.61
    | "SAR" == currency = money / 4.95
    | "SEK" == currency = money / 9.17
    | "SGD" == currency = money / 1.65
    | "TRY" == currency = money / 2.85
    | "TWD" == currency = money / 39.42
    | "USD" == currency = money / 1.32
    | "ZAR" == currency = money / 14.1
    | otherwise = 1

-- apple started reports in 2009
fiscalYears :: IO [String]
fiscalYears = do
  current_year <- getCurrentYear
  return(map show [2009..read current_year])

getCurrentMonth :: IO String
getCurrentMonth = do
  now <- getCurrentTime
  return(formatTime defaultTimeLocale "%m" now)

getCurrentYear :: IO String
getCurrentYear = do
  now <- getCurrentTime
  return(formatTime defaultTimeLocale "%Y" now)

getCurrentDay :: IO String
getCurrentDay = do
  now <- getCurrentTime
  return(formatTime defaultTimeLocale "%d" now)

getPreviousMonth :: (String, String) -> (String, String)
getPreviousMonth (month, year)
    | read month == 1 = ("12" , show(read year - 1))
    | read month <= 10 = ("0" ++ show(read month - 1), year)
    | otherwise = (show(read month - 1), year)

getLast12Months :: [(String, String)] -> [(String, String)]
getLast12Months months
                | length months >= 12 = months
                | length months < 12 = getLast12Months([getPreviousMonth(head months)] ++ months)

getLast30Days :: [(String, String, String)] -> [(String, String, String)]
getLast30Days days
              | length days >= 30 = days
              | length days < 30 = getLast30Days([getPreviousDay(head days)] ++ days)

getPreviousDay :: (String, String, String) -> (String, String, String)
getPreviousDay (day, month, year)
    | d < 10 && m < 10 = ("0" ++ show d, "0" ++ show m, show y)
    | d < 10 = ("0" ++ show d, show m, show y)
    | m < 10 = (show d, "0" ++ show m, show y)
    | otherwise = (show d, show m, show y)
    where (y, m, d) = toGregorian $ addDays (-1) $ fromGregorian (read year) (read month) (read day)

compareMonthsAndYears :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareMonthsAndYears (a1, b1) (a2, b2)
                      | b1 < b2 = LT
                      | b1 > b2 = GT
                      | b1 == b2 = compare a1 a2

getShortTimeFrame :: String -> String
getShortTimeFrame timeFrame
    | timeFrame == "Monthly" = "M"
    | otherwise = "D"

downloadFinancialReport vendor_id timeframe date = do
  system("cd Autoingestion")
  putStrLn(working_string)
  alreadyDownloaded <- doesFileExist $ "./Autoingestion/S_" ++ getShortTimeFrame(timeframe) ++ "_" ++ vendor_id ++ "_" ++ date ++ ".txt.gz"
  if alreadyDownloaded then
      system("echo 'Already Downloaded'")
  else
      system("cd Autoingestion && java Autoingestion autoingestion.properties " ++ vendor_id ++ " Sales " ++ timeframe ++ " Summary " ++ date ++ " && cd ..")
    where working_string = "working on " ++ date

-- If report from previous is not available, apple autoingestion
-- tools may download report from previous month. For example it may
-- download the report for july even if we specified a date for
-- august.
downloadAllMonthlyFinancialReports :: String -> IO ()
downloadAllMonthlyFinancialReports vendor_id = do
  current_month <- getCurrentMonth
  current_year <- getCurrentYear
  mapM_ (downloadFinancialReport vendor_id "Monthly") [snd(months) ++ fst(months) | months <- getLast12Months([getPreviousMonth(current_month, current_year)])]
  return ()

downloadAllDailyFinancialReports vendor_id = do
  current_month <- getCurrentMonth
  current_year <- getCurrentYear
  current_day <- getCurrentDay
  let days =  map (\(x, y ,z) -> z ++ y ++ x) [d | d <- getLast30Days([getPreviousDay(current_day, current_month, current_year)])]
  mapM_ (downloadFinancialReport vendor_id "daily") days
  return ()

getReportContent :: String -> IO String
getReportContent path = do
  contents <- fmap GZip.decompress $ L.readFile path
  return(unpack contents)

parseProceeds :: String -> String -> Float
parseProceeds currency proceeds
              | (startswith "." proceeds) == True = parseProceeds currency ("0" ++ proceeds)
              | otherwise = convertToEuros currency (read proceeds)

parseReportLine :: [String] -> (String, Float)
-- proceeds (!! 8) are converted to euros then multiplied by the number of app solds for current report line ( !!7)
parseReportLine report_line = (report_line !! 4, (parseProceeds (report_line !! 11) (report_line !! 8) * (read (report_line !! 7))))

-- only months for now
reportDateIsInInterval reportFilePath startDate endDate
    | diffDays (fromGregorian reportYear reportMonth reportDay) (fromGregorian startDateYear startDateMonth startDateDay) >= 0  &&
        diffDays (fromGregorian endDateYear endDateMonth endDateDay) (fromGregorian reportYear reportMonth reportDay) >= 0 = True
    | otherwise = False
    where dateString = head . split "." . last $ split "_" reportFilePath
          reportYear = read $ take 4 dateString :: Integer
          startDateYear = read $ take 4 startDate :: Integer
          endDateYear = read $ take 4 endDate :: Integer
          startDateMonth = if length startDate < 8 then read $ drop 4 startDate else read $ take 2 $ drop 4 startDate :: Int
          reportMonth = if length dateString < 8 then read $ drop 4 dateString else read $ take 2 $ drop 4 dateString :: Int
          endDateMonth = if length endDate < 8 then read $ drop 4 endDate else read $ take 2 $ drop 4 endDate :: Int
          startDateDay = if length startDate < 8 then 1 else read $ drop 6 startDate :: Int
          reportDay = if length dateString < 8 then 15 else read $ drop 6 dateString :: Int
          endDateDay = if length endDate < 8 then 31 else read $ drop 6 endDate :: Int


parseReports start_date end_date = do
  directory_contents <-  getDirectoryContents "./Autoingestion"
  let filepaths_and_report_date = map (\x -> ("./Autoingestion/" ++ x, head . split "." . last $ split "_" x)) $ filter (startswith (if length start_date == 8 then "S_D" else "S_M")) directory_contents
  let report_between_dates_filepath = map (fst) $ filter (\(x, y) -> reportDateIsInInterval y start_date end_date) filepaths_and_report_date
  report_data <- fmap concat $ mapM getReportContent report_between_dates_filepath
  let report_data_lines = map (split "\t") $ filter (\x -> startswith "Provider\tProvider Country" x == False) $ lines report_data
  let sorted = sortBy (\(x1,y1) (x2, y2) -> y2 `compare` y1) $ Map.toList $ Map.fromListWith (+) $ map (parseReportLine) $ report_data_lines

  return (report_between_dates_filepath, sorted)

outputReportFilePaths filepaths = do
  putStrLn "=============================="
  putStrLn "Working on following reports:"
  putStrLn "=============================="
  mapM_ putStrLn $ map (\x -> last $ split "/" x) $ filepaths
  return ()

outputAppsSum report_filepaths sorted_reports = do
  putStrLn "=============================="
  putStrLn $ "Total: " ++ show (foldl (\acc (x,y) -> acc + y) 0 sorted_reports)
  putStrLn "=============================="
  putStrLn "App Sums:"
  putStrLn "=============================="
  mapM_ putStrLn $ map (\(x,y) -> x ++ " -> " ++ (show y)) sorted_reports
  return ()

outputfilteredReportData report_data filterString = do
  putStrLn $ filterString ++ " -> " ++ show (foldl (\acc (x,y) -> acc + y) 0 $ filter (\(x,y) -> x =~ filterString) report_data)
  return ()

main = do
  (command:arglist) <- getArgs
  dispatch command arglist
  return()

dispatch :: String -> [String] -> IO ()
dispatch "download" = download_reports
dispatch "filter" = filter_report
dispatch "help" = help
dispatch "sales" = sales
dispatch command = unknown command

filter_report args = do
  putStrLn "=============================="
  report_data <- parseReports (args !! 0) (args !! 1)
  outputfilteredReportData (snd(report_data)) (args !! 2)
  putStrLn "=============================="

  return ()

download_reports args = do
  putStrLn "=============================="
  putStrLn "Downloading Montly Reports"
  putStrLn "=============================="
  downloadAllMonthlyFinancialReports (args !! 0)

  putStrLn "=============================="
  putStrLn "Downloading Daily Reports"
  putStrLn "=============================="
  downloadAllDailyFinancialReports (args !! 0)
  return ()

unknown command _ = do
  putStrLn $ "Command <" ++ command ++ "> not available, type <help> to get a list of commands"
  return ()

help _ = do
  putStrLn "Usage:"
  putStrLn "$ ./itc_reports_tool download <vendor_id>"
  putStrLn "$ ./itc_reports_tool sales <startdate> <enddate>"
  putStrLn "$ ./itc_reports_tool filter <startdate> <enddate> <filter>"
  putStrLn "<filter> is a regular expression"
  putStrLn "Dates can be in the form YEARMONTHDAY or YEARMONTH. Day is optional. i.e. 20140701 and 201408 are both valid"
  return ()

sales args = do
  report_data <- parseReports (startDate) (endDate)
  outputReportFilePaths (fst(report_data))
  outputAppsSum (fst(report_data)) (snd(report_data))
  return ()
    where startDate = (args !! 0)
          endDate = (args !! 1)
