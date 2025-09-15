{-# LANGUAGE LambdaCase #-}

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
  , showSeconds :: Bool
  , zeroStr :: String
  , oneStr :: String
  , newLines :: Bool
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
runApp BClock{vertical, zeroStr, oneStr, showSeconds, newLines} = do
  putStr "\ESC[?25l"
  forever $ do
    putStr "\ESC[2J"
    putStr "\ESC[H\n"

    now <- getZonedTime
    let list' = words $ formatTime defaultTimeLocale "%S %M %H" now
        list = reverse $ if not showSeconds then drop 1 list' else list'

    let final =
          when newLines (intersperse [])
            . map (concatMap (toStr zeroStr oneStr) . ("  " ++))
            $ when vertical (map (intersperse ' ') . transpose . map reverse)
            $ map (setSize 6 . toBinary . read) list

    mapM_ putStrLn final
    threadDelay 500000
