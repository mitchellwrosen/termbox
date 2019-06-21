module Main where

import qualified Termbox

import Control.Monad
import Data.Foldable
import Data.Word
import GHC.Clock
import Text.Printf

import qualified Data.Array.MArray as Array

n :: Int
n = 1000

main :: IO ()
main = do
  timed "1000x drawing over entire terminal, one cell at a time" main1
  timed "1000x drawing over entire terminal using backbuffer" main2

timed :: String -> IO () -> IO ()
timed name action = do
  putStrLn name
  t0 <- getMonotonicTimeNSec
  Termbox.main action
  t1 <- getMonotonicTimeNSec
  printf "%.2fms\n" (fromIntegral (t1 - t0) / (1e6 :: Double))

main1 :: IO ()
main1 = do
  (w, h) <- Termbox.size

  replicateM_ n $ do
    Termbox.clear mempty mempty
    for_ [0 .. h-1] $ \r ->
      for_ [0 .. w-1] $ \c ->
        Termbox.set c r (Termbox.Cell 'X' Termbox.red Termbox.green)
    Termbox.flush

main2 :: IO ()
main2 = do
  replicateM_ n $ do
    Termbox.clear mempty mempty
    buffer <- Termbox.buffer
    ((r0,c0), (rn,cn)) <- Array.getBounds buffer
    for_ [r0 .. rn] $ \r ->
      for_ [c0 .. cn] $ \c ->
        Array.writeArray buffer (r, c) (Termbox.Cell 'X' Termbox.red Termbox.green)
    Termbox.flush
