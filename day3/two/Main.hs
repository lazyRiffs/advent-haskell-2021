module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

binToDec :: String -> Int
binToDec []       = 0
binToDec "0"      = 0
binToDec "1"      = 1
binToDec ('0':xs) = binToDec xs
binToDec ('1':xs) = (2 ^ (length xs)) + (binToDec xs)
binToDec xs       = error $ "invalid character : " ++ (show xs)

getRatings :: [String] -> (Int,Int,Int)
getRatings ss =
  (o2, cO2, o2 * cO2)
  where o2 = binToDec $ getRatings' ss "" Oxygen
        cO = binToDec $ getRatings' ss "" CO2

getRatings' :: [String] -> String -> Rating -> String
getRatings' [] _   _   = []
getRatings' (l:[]) _ _ = l
getRatings' ls r   x   =
  getRatings' matches (winner:r) x
  where i            = length r
        (winner, cp) = getCompare ls i x
        matches      = filter cp ls

getCompare :: [String] -> Int -> Rating -> (Char, (String -> Bool))
getCompare [] _ _ = ('_', \s -> False)
getCompare ls i x =
  (winner, (\s -> nextChar s == winner))
  where nextChar = \s -> s !! i
        winner   = getChoice ls nextChar x

getChoice :: [String] -> (String -> Char) -> Rating -> Char
getChoice [] _ _ = '_'
getChoice ls nextChar Oxygen = getChoice' ls nextChar (\z o -> if z > o then '0' else '1')
getChoice ls nextChar CO2    = getChoice' ls nextChar (\z o -> if z <= o then '0' else '1')

getChoice' :: [String] -> (String -> Char) -> (Int -> Int -> Char) -> Char
getChoice' [] _ _ = '_'
getChoice' ls nextChar o2OrCo2 =
  o2OrCo2 zeros ones
  where chars = fmap nextChar ls
        zeros    = length $ filter (=='0') chars
        ones     = length $ filter (=='1') chars

data Rating = Oxygen | CO2

main :: IO ()
main = do
  contents <- lines <$> BLC.unpack <$> BL.readFile "./inputs"
  putStrLn . show $ getRatings contents
