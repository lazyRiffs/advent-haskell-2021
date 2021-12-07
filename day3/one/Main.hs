module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

joinZip :: (a,[a]) -> [a]
joinZip (a,list) = (a:list)

countGammas :: [String] -> [String] -> [String]
countGammas [] result       = result
countGammas (x:xs) result   = countGammas xs next
  where next = fmap joinZip $ x `zip` result

getGamma :: String -> Char
getGamma bits = if zeros > ones then '0' else '1'
  where zeros = length $ filter (=='0') bits
        ones  = length $ filter (=='1') bits

getEpsilon :: String -> String
getEpsilon [] = []
getEpsilon ('0':xs) = ('1':getEpsilon xs)
getEpsilon ('1':xs) = ('0':getEpsilon xs)

binToDec :: String -> Int
binToDec []       = 0
binToDec "0"      = 0
binToDec "1"      = 1
binToDec ('0':xs) = binToDec xs
binToDec ('1':xs) = (2 ^ (length xs)) + (binToDec xs)
binToDec xs       = error $ "invalid character : " ++ (show xs)

main :: IO ()
main = do
  contents <- lines <$> BLC.unpack <$> BL.readFile "./inputs"
  -- start with an empty list of length equal to the length of each line
  -- so that zip works properly
  let base   = take (length $ contents !! 0) $ repeat ""
  let counts = countGammas contents base
  let g      = fmap getGamma counts
  let g'     = binToDec g
  let e      = getEpsilon g
  let e'     = binToDec e
  putStrLn . show $ g' * e'

