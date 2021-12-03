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

mapTrips :: [Integer] -> [Integer] -> [Integer]
mapTrips trips [] = trips
mapTrips trips (x:[]) = trips
mapTrips trips (x:y:[]) = trips
mapTrips trips (x:y:z:[]) = (x+y+z:trips)
mapTrips trips (x:rest@(y:z:xs)) = (x+y+z : mapTrips trips rest)

countTrips :: [Integer] -> Int
countTrips ns =
  countIncrements trips
  where trips = mapTrips [] ns

main :: IO ()
main = do
  contents <-  BLC.unpack <$> BL.readFile "./inputs"
  -- let result = countIncrements . fmap readInt . filter notEmpty $ lines contents
  let result = countTrips . fmap readInt . filter notEmpty $ lines contents
  putStrLn (show result)

