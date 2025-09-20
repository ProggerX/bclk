{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Monad hiding (when)
import Data.List
import Data.Time
import Options.Applicative
import System.Exit
import System.Posix.Signals

data App = BClock
  { vertical :: Bool
  , zeroStr :: String
  , oneStr :: String
  , newLines :: Bool
  , format :: String
  }

parser :: Parser App
parser =
  BClock
    <$> switch
      ( long "vertical"
          <> short 'v'
          <> help "Use vertical layout"
      )
    <*> strOption
      ( long "zero"
          <> short '0'
          <> help "Zero string"
          <> showDefault
          <> value "·"
      )
    <*> strOption
      ( long "one"
          <> short '1'
          <> help "One string"
          <> showDefault
          <> value "█"
      )
    <*> switch
      ( long "newlines"
          <> short 'n'
          <> help "Place additional newlines"
      )
    <*> strOption
      ( long "format"
          <> short 'f'
          <> help "Format"
          <> showDefault
          <> value "%H %M %S"
      )

main :: IO ()
main = do
  tid <- myThreadId
  !_ <-
    installHandler
      keyboardSignal
      (Catch (putStrLn "\ESC[?25h" >> throwTo tid ExitSuccess))
      Nothing

  runApp =<< execParser opts
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

setSize :: Int -> String -> String
setSize z s = replicate (z - length s) '0' ++ s

toStr :: String -> String -> Char -> String
toStr zS oS = \case
  '0' -> zS
  '1' -> oS
  x -> [x]

when :: Bool -> (a -> a) -> (a -> a)
when c f = if c then f else id

runApp :: App -> IO ()
runApp BClock{..} = do
  putStr "\ESC[?25l"
  forever $ do
    putStr "\ESC[2J"
    putStr "\ESC[H\n"

    now <- getZonedTime
    let list = words $ formatTime defaultTimeLocale format now

    let final =
          when newLines (intersperse [])
            . map (concatMap (toStr zeroStr oneStr) . ("  " ++))
            $ when vertical (map (intersperse ' ') . transpose . map reverse)
            $ map (setSize 6 . toBinary . read) list

    mapM_ putStrLn final
    threadDelay 500000
