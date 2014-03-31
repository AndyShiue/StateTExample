module Main where

import Prelude hiding (lookup)

import Data.Char

import Data.Map.Lazy
import Control.Monad.State

import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = void $ (runStateT $ sequence_ . repeat $ variableStoring) empty

data Command = Get String | Set String String

variableStoring :: StateT (Map String String) IO ()
variableStoring = do lift . putStr $ "> "
                     str <- lift getLine
                     table <- get
                     let parsed = parseCommand str
                     case parsed of
                       Left err -> error $ show err
                       Right command ->
                         case command of
                           Get key -> lift . putStrLn $ case lookup key table of
                                                          Just val -> val
                                                          Nothing -> "Cannot find " ++ key ++ "."
                           Set key value -> modify $ insert key value
                     return ()

parseCommand ::　String -> Either ParseError Command
parseCommand = parse commandParser ""

commandParser :: Parser Command
commandParser = spaces >> (getParser <|> setParser)

nonSpaces :: Parser String
nonSpaces = many . satisfy $ not . isSpace

trimmed :: Parser String
trimmed = do spaces
             str <- nonSpaces
             spaces
             return str

getParser :: Parser Command
getParser = do string "get"
               key <- trimmed
               return $ Get key
               
setParser :: Parser Command
setParser = do string "set"
               key <- trimmed
               string "to"
               value <- trimmed
               return $ Set key value