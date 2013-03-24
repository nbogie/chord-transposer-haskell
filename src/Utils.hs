module Utils where
import Data.List (nub)
-- bool fold
bool :: a -> a -> Bool -> a
bool t f b = if b then t else f

remRoot (c1:c2:rest) | c2 `elem` "b#" = rest
                     | otherwise            = c2:rest
remRoot _other = [] -- either single char or empty str
switchRoot newRoot word = newRoot : remRoot word
-- (map ('A':). Data.List.nub . map remRoot . words) $ readFile "foo"

-- give it the name of a file full only of chords.  it will give them all the same root and print with no dupes.
readAndPrintUniqueChords fname = fmap (Data.List.nub . map (switchRoot 'A'). words) (readFile fname) >>= mapM_ putStrLn

