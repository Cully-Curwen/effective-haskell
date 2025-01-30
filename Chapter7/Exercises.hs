module Exercises where

import System.Environment

-- Thinking :: IO (IO String)
-- Thinking = return $ return "string"

printNestedIO :: IO (IO String) -> IO ()
printNestedIO nestedIO = nestedIO >>= go
  where
    go :: IO String -> IO ()
    go ioString = ioString >>= putStrLn

unIO :: IO (IO a) -> IO a
unIO nestedIO = nestedIO >>= id

sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x:xs) = do
  x' <- x
  xs' <- sequenceIO xs
  return $ x' : xs'

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ actions = sequenceIO actions >> return ()


clCalc :: IO ()
clCalc = getArgs >>= print . sum . map read 

clCalc' :: IO()
clCalc' = do
  args <- getArgs
  let sumArgs = sum $ map read args
  print sumArgs

calc :: IO ()
calc = do
  args <- getArgs
  print $ result args
  where
    result args = case head args of
      "+" -> sum $ map read $ tail args
      "-" -> foldr (-) 0 $ map read $ tail args
      "*" -> product $ map read $ tail args
      _ -> sum $ map read args

wordReplace :: String -> String -> String -> String
wordReplace fileContent needle replacement = unwords . map (replace needle replacement) $ words fileContent
  where replace needle replacement word = if word == needle then replacement else word

replacePrint :: IO ()
replacePrint = do
  (path : needle : replacement : _) <- getArgs
  fileContent <- readFile path
  print $ wordReplace fileContent needle replacement
