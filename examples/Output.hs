-- | Something like https://github.com/nsf/termbox/blob/8b72969ff4bba120d8b8e4a29bae07102ed71055/src/demo/output.c

{-# language LambdaCase #-}

import Data.Foldable (for_)

import qualified Termbox as Tb

main :: IO ()
main = do
  Tb.main $ do
    let
      loop :: [Tb.OutputMode] -> IO ()
      loop = \case
        [] ->
          pure ()

        mode:modes -> do
          draw mode
          Tb.pollEvent >>= \case
            Tb.EventKey Tb.KeyEsc _ ->
              pure ()
            Tb.EventResize ->
              loop (mode:modes)
            _ ->
              loop modes

    loop
      [ Tb.OutputModeNormal
      , Tb.OutputModeGrayscale
      , Tb.OutputMode216
      ]

draw :: Tb.OutputMode -> IO ()
draw mode = do
  Tb.clear

  w <- Tb.width
  h <- Tb.height

  let
    coords :: [(Int, Int)]
    coords =
      (,) <$> [0..h-1] <*> [0..w-1]

  Tb.selectOutputMode mode

  case mode of
    Tb.OutputModeNormal -> do
      let
        colors :: [Tb.Color]
        colors =
          (<>) <$> allAttrs <*> allColors

      for_ (zip coords ((,) <$> colors <*> colors)) $ \((y, x), (bg, fg)) ->
        Tb.putCell x y 'x' fg bg

    Tb.OutputModeGrayscale -> do
      let
        colors :: [Tb.Color]
        colors =
          (<>) <$> [mempty] <*> map fromInteger [0..23]

      for_ (zip coords ((,) <$> colors <*> colors)) $ \((y, x), (bg, fg)) ->
        Tb.putCell x y 'x' fg bg

    _ -> do
      pure ()

  Tb.present

allAttrs :: [Tb.Color]
allAttrs =
  [ mempty
  , Tb.bold
  , Tb.underline
  , Tb.bold <> Tb.underline
  ]

allColors :: [Tb.Color]
allColors =
  [ mempty
  , Tb.black
  , Tb.red
  , Tb.green
  , Tb.yellow
  , Tb.blue
  , Tb.magenta
  , Tb.cyan
  , Tb.white
  ]
