-- | Something like https://github.com/nsf/termbox/blob/8b72969ff4bba120d8b8e4a29bae07102ed71055/src/demo/output.c

{-# language LambdaCase #-}

import Control.Monad
import Data.Foldable

import qualified Termbox as Tb

main :: IO ()
main = do
  Tb.main $ do
    clear

    do
      let
        rectangles :: [(Int, Int, Int, Int)]
        rectangles = do
          y0 <- [0, 4 ..]
          x0 <- [0, 8 .. 24]
          pure (x0, y0, x0+7, y0+3)

      let
        colors :: [(String, Tb.Attr, Tb.Attr)]
        colors =
          [ ("black", Tb.black, Tb.white)
          , ("red", Tb.red, Tb.black)
          , ("green", Tb.green, Tb.black)
          , ("yellow", Tb.yellow, Tb.black)
          , ("blue", Tb.blue, Tb.black)
          , ("magenta", Tb.magenta, Tb.black)
          , ("cyan", Tb.cyan, Tb.black)
          , ("white", Tb.white, Tb.black)
          ]

      zipWithM_
        (\(x0, y0, x1, y1) (name, bg, fg) -> do
          rectangle x0 y0 x1 y1 (Tb.Cell ' ' mempty bg)
          string x0 y0 fg bg name)
        rectangles
        colors

    Tb.flush
    _ <- Tb.pollEvent

    clear
    Tb.setOutputMode Tb.OutputModeGrayscale

    do
      let
        rectangles :: [(Int, Int, Int, Int)]
        rectangles = do
          y0 <- [0, 4 ..]
          x0 <- [0, 8 .. 40]
          pure (x0, y0, x0+7, y0+3)

      zipWithM_
        (\(x0, y0, x1, y1) n -> do
          rectangle x0 y0 x1 y1 (Tb.Cell ' ' mempty (fromInteger n))
          string x0 y0 12 (fromInteger n) (show n))
        rectangles
        [1..23]

    Tb.flush
    _ <- Tb.pollEvent

    clear
    Tb.setOutputMode Tb.OutputMode216

    do
      let
        rectangles :: [(Int, Int, Int, Int)]
        rectangles = do
          y0 <- [0, 2..]
          x0 <- [0, 4 .. 40]
          pure (x0, y0, x0+3, y0+1)

      zipWithM_
        (\(x0, y0, x1, y1) n -> do
          rectangle x0 y0 x1 y1 (Tb.Cell ' ' mempty (fromInteger n))
          string x0 y0 2 (fromInteger n) (show n))
        rectangles
        [1..216]

    Tb.flush
    _ <- Tb.pollEvent

    pure ()

clear :: IO ()
clear = do
  mode <- Tb.getOutputMode
  Tb.setOutputMode Tb.OutputModeNormal
  Tb.clear mempty mempty
  Tb.flush
  Tb.setOutputMode mode

string :: Int -> Int -> Tb.Attr -> Tb.Attr -> [Char] -> IO ()
string x0 y fg bg cs =
  zipWithM_
    (\x c -> Tb.setCell x y (Tb.Cell c fg bg))
    [x0..]
    cs

rectangle :: Int -> Int -> Int -> Int -> Tb.Cell -> IO ()
rectangle x0 y0 x1 y1 c =
  for_ ((,) <$> [x0..x1] <*> [y0..y1]) $ \(x, y) ->
    Tb.setCell x y c
