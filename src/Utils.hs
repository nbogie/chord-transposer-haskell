module Utils where
import qualified Data.IntMap as IntMap
import Data.List (nub)
import Data.Char (ord)

-- bool fold
bool :: a -> a -> Bool -> a
bool t f b = if b then t else f

remRoot (_c1:c2:rest) | c2 `elem` "b#" = rest
                      | otherwise            = c2:rest
remRoot _other = [] -- either single char or empty str
switchRoot newRoot word = newRoot : remRoot word
-- (map ('A':). Data.List.nub . map remRoot . words) $ readFile "foo"

-- give it the name of a file full only of chords.  it will give them all the same root and print with no dupes.
readAndPrintUniqueChords fname = fmap (Data.List.nub . map (switchRoot 'A'). words) (readFile fname) >>= mapM_ putStrLn


-- Lifted directly from Neil Mitchell's Tag Soup
-- | Replace the four characters @&\"\<\>@ with their HTML entities (the list from 'xmlEntities').
escapeHTML ::  String -> String
escapeHTML = concatMap esc1
    where esc = IntMap.fromList [(b, "&"++a++";") | (a,b) <- xmlEntities]
          esc1 x = IntMap.findWithDefault [x] (ord x) esc

-- Lifted directly from Neil Mitchell's Tag Soup
-- | A table mapping XML entity names to code points.
--   Does /not/ include @apos@ as Internet Explorer does not know about it.
xmlEntities :: [(String, Int)]
xmlEntities = let a*b = (a,ord b) in
    ["quot" * '"'
    ,"amp"  * '&'
    -- ,"apos" * '\''    -- Internet Explorer does not know that
    ,"lt"   * '<'
    ,"gt"   * '>'
    ]
