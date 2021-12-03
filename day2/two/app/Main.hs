{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T

import Lib

type Parser = Parsec Void Text

data Direction = Forward | Down | Up deriving (Eq, Show)
data Command = Command Direction Int deriving (Eq, Show)
--                        H   V  A
data Position = Position Int Int Int deriving (Eq, Show)

readNum :: String -> Int
readNum s = read s

pDirection :: Parser Direction
pDirection = do
  (string "forward" >> return Forward)
  <|> (string "down" >> return Down)
  <|> (string "up" >> return Up)

pNum :: Parser Int
pNum = do
  n <-  some digitChar
  return $ readNum n

pCommand :: Parser Command
pCommand = do
  d <- space *> pDirection <* space
  m <- pNum
  return $ Command d m

pCommands :: Parser [Command]
pCommands = do
  c    <- pCommand
  _    <- newline
  rest <- pCommands <|> (eof >> return [])
  return (c:rest)

applyCommand :: Position -> Command -> Position
applyCommand (Position h v a) (Command Forward m) =
  Position (h+m) v'  a
  where v' = v + m * a
applyCommand (Position h v a) (Command Down m)    = Position  h    v    (a+m)
applyCommand (Position h v a) (Command Up m)      = Position  h    v    (a-m)

main :: IO ()
main = do
  contents <- T.pack . BLC.unpack <$> BL.readFile "./inputs"
  let startingPosition = Position 0 0 0
  let (Position h v a) = foldl applyCommand startingPosition (runParse contents)
  putStrLn (show $ h * v)

-- helper functions
runParse :: Text -> [Command]
runParse contents =
  case (runParser pCommands "" contents) of
    Left e         -> error $ "parse error:\n" ++ (show e)
    Right commands -> commands

