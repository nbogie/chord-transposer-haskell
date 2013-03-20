module LineSplitter where

import Data.List (groupBy, foldl')
import Data.Function (on)
import Data.Char (isSpace)

sampleInput :: String
sampleInput = " Intro:  Cm7   F#dim    EM7  (I think?) "

demo :: IO ()
demo = print $ findWordPositions sampleInput
-- main = demo
findWordPositions :: String -> [(String, Int)]
findWordPositions input = filter (not . isSpace . head . fst) $ zip ws positions
  where
    ws = groupBy ((==) `on` (==' ')) input
    positions= reverse . snd . foldl' f (0,[]) $ ws
    f (pos,cs) e = (pos + length e, pos:cs)
