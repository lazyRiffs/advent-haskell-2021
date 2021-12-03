module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List.HT

readInt :: String -> Integer
readInt s = read s

notEmpty :: String -> Bool
notEmpty [] = False
notEmpty _  = True

countIncrements :: [Integer] -> Int
countIncrements xs = countIncrements' xs 0

countIncrements' :: [Integer] -> Int -> Int
countIncrements' [] result = result
countIncrements' (x:[]) result = result

countIncrements' (x:rest@(y:xs)) result
  | y > x     = countIncrements' rest (result + 1)
  | otherwise = countIncrements' rest result

main :: IO ()
main = do
  contents <-  BLC.unpack <$> BL.readFile "./inputs"
  let result = countIncrements . fmap readInt . filter notEmpty $ lines contents
  putStrLn (show result)

