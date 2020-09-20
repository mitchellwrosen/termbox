import Control.Monad
import Data.Foldable
import Data.Functor (void)
import qualified Termbox

main :: IO (Either Termbox.InitError ())
main = do
  Termbox.run Termbox.defaultInputMode Termbox.defaultOutputMode $ do
    let rectangles :: [(Int, Int, Int, Int)]
        rectangles = do
          y0 <- [0, 4 ..]
          x0 <- [0, 8 .. 24]
          pure (x0, y0, x0 + 7, y0 + 3)

    let colors :: [(String, Termbox.Attr, Termbox.Attr)]
        colors =
          [ ("black", Termbox.black, Termbox.white),
            ("red", Termbox.red, Termbox.black),
            ("green", Termbox.green, Termbox.black),
            ("yellow", Termbox.yellow, Termbox.black),
            ("blue", Termbox.blue, Termbox.black),
            ("magenta", Termbox.magenta, Termbox.black),
            ("cyan", Termbox.cyan, Termbox.black),
            ("white", Termbox.white, Termbox.black)
          ]

    Termbox.render
      ( mconcat
          ( zipWith
              ( \(x0, y0, x1, y1) (name, bg, fg) ->
                  rectangle x0 y0 x1 y1 (Termbox.Cell ' ' mempty bg) <> string x0 y0 fg bg name
              )
              rectangles
              colors
          )
      )
      Termbox.NoCursor

    void Termbox.poll

  Termbox.run Termbox.defaultInputMode Termbox.OutputModeGrayscale $ do
    let rectangles :: [(Int, Int, Int, Int)]
        rectangles = do
          y0 <- [0, 4 ..]
          x0 <- [0, 8 .. 40]
          pure (x0, y0, x0 + 7, y0 + 3)

    Termbox.render
      ( mconcat
          ( zipWith
              ( \(x0, y0, x1, y1) n ->
                  rectangle x0 y0 x1 y1 (Termbox.Cell ' ' mempty (fromInteger n))
                    <> string x0 y0 12 (fromInteger n) (show n)
              )
              rectangles
              [0 .. 23]
          )
      )
      Termbox.NoCursor

    void Termbox.poll

  Termbox.run Termbox.defaultInputMode Termbox.OutputMode216 $ do
    let rectangles :: [(Int, Int, Int, Int)]
        rectangles = do
          y0 <- [0, 2 ..]
          x0 <- [0, 4 .. 40]
          pure (x0, y0, x0 + 3, y0 + 1)

    Termbox.render
      ( mconcat
          ( zipWith
              ( \(x0, y0, x1, y1) n ->
                  rectangle x0 y0 x1 y1 (Termbox.Cell ' ' mempty (fromInteger n))
                    <> string x0 y0 2 (fromInteger n) (show n)
              )
              rectangles
              [0 .. 215]
          )
      )
      Termbox.NoCursor

    void Termbox.poll

  Termbox.run Termbox.defaultInputMode Termbox.OutputMode256 $ do
    let rectangles :: [(Int, Int, Int, Int)]
        rectangles = do
          y0 <- [0, 2 ..]
          x0 <- [0, 4 .. 48]
          pure (x0, y0, x0 + 3, y0 + 1)

    Termbox.render
      ( mconcat
          ( zipWith
              ( \(x0, y0, x1, y1) n ->
                  rectangle x0 y0 x1 y1 (Termbox.Cell ' ' mempty (fromInteger n))
                    <> string x0 y0 1 (fromInteger n) (show n)
              )
              rectangles
              [0 .. 255]
          )
      )
      Termbox.NoCursor

    void Termbox.poll

string :: Int -> Int -> Termbox.Attr -> Termbox.Attr -> [Char] -> Termbox.Cells
string x0 y fg bg =
  mconcat . zipWith (\x c -> Termbox.set x y (Termbox.Cell c fg bg)) [x0 ..]

rectangle :: Int -> Int -> Int -> Int -> Termbox.Cell -> Termbox.Cells
rectangle x0 y0 x1 y1 c =
  foldMap (\(x, y) -> Termbox.set x y c) ((,) <$> [x0 .. x1] <*> [y0 .. y1])
