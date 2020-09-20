import Control.Monad
import Data.Foldable
import Data.Functor (void)
import qualified Termbox

main :: IO (Either Termbox.InitError ())
main =
  Termbox.run Termbox.defaultInputMode $ do
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
  mconcat . zipWith (\x c -> Termbox.set x y (Termbox.Cell c 0 bg)) [x0 ..]

rectangle :: Int -> Int -> Int -> Int -> Termbox.Cell -> Termbox.Cells
rectangle x0 y0 x1 y1 c =
  foldMap (\(x, y) -> Termbox.set x y c) ((,) <$> [x0 .. x1] <*> [y0 .. y1])
