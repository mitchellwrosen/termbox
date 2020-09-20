import Data.Semigroup ((<>))
import qualified Termbox

main :: IO ()
main =
  Termbox.run $ \_w _h render poll -> do
    let rectangles :: [(Int, Int, Int, Int)]
        rectangles = do
          y0 <- [0, 2 ..]
          x0 <- [0, 4 .. 48]
          pure (x0, y0, x0 + 3, y0 + 1)

    let cells :: Maybe Termbox.Event -> Termbox.Cells
        cells lastEvent =
          mconcat
            ( zipWith
                ( \(x0, y0, x1, y1) n ->
                    rectangle x0 y0 x1 y1 (Termbox.Cell ' ' mempty (fromInteger n))
                      <> string x0 y0 mempty (fromInteger n) (show n)
                )
                rectangles
                [0 .. 255]
            )
            <> string 54 1 mempty mempty "Press Esc to quit."
            <> string 54 3 mempty mempty ("Last event: " ++ show lastEvent)

    let loop :: Maybe Termbox.Event -> IO ()
        loop lastEvent = do
          render (cells lastEvent) Termbox.NoCursor
          poll >>= \case
            Termbox.EventKey Termbox.KeyEsc -> pure ()
            event -> loop (Just event)

    loop Nothing

string :: Int -> Int -> Termbox.Attr -> Termbox.Attr -> [Char] -> Termbox.Cells
string x0 y fg bg =
  mconcat . zipWith (\x c -> Termbox.set x y (Termbox.Cell c fg bg)) [x0 ..]

rectangle :: Int -> Int -> Int -> Int -> Termbox.Cell -> Termbox.Cells
rectangle x0 y0 x1 y1 c =
  foldMap (\(x, y) -> Termbox.set x y c) ((,) <$> [x0 .. x1] <*> [y0 .. y1])
