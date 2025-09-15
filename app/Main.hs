{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Time
import Data.Time.LocalTime
import Options.Applicative

data App = BClock
  { vertical :: Bool
  , showSeconds :: Bool
  , zeroStr :: String
  , oneStr :: String
  }

parser :: Parser App
parser =
  BClock
    <$> switch
      ( long "vertical"
          <> short 'v'
          <> help "Use vertical layout"
      )
    <*> switch
      ( long "seconds"
          <> short 's'
          <> help "Show seconds"
      )
    <*> strOption
      ( long "zero"
          <> short '0'
          <> help "Zero string"
          <> showDefault
          <> value "🭹"
      )
    <*> strOption
      ( long "one"
          <> short '1'
          <> help "One string"
          <> showDefault
          <> value "█"
      )

main :: IO ()
main = runApp =<< execParser opts
 where
  opts =
    info
      (parser <**> helper)
      ( fullDesc
          <> header "bclk - binary clock in your terminal"
      )

toBinary :: Int -> String
toBinary n
  | n < 0 = error "Negative to binary is not supported"
  | n == 0 = []
  | otherwise = toBinary (n `div` 2) ++ show (n `mod` 2)

setSize :: Int -> Int -> String -> String
setSize sp z s = concat [replicate sp ' ', replicate (z - length s) '0', s, "\n"]

toStr :: String -> String -> Char -> String
toStr zS oS = \case
  '0' -> zS
  '1' -> oS
  x -> [x]

runApp :: App -> IO ()
runApp BClock{zeroStr, oneStr, showSeconds} = do
  putStr "\ESC[?25l"
  forever $ do
    putStr "\ESC[2J"
    putStr "\ESC[H\n"

    now <- getZonedTime
    let list' = words $ formatTime defaultTimeLocale "%H %M %S" now
        list = if not showSeconds then drop 1 list' else list'

    mapM_ (putStrLn . concatMap (toStr zeroStr oneStr) . setSize 2 6 . toBinary . read) list
    threadDelay 500000
