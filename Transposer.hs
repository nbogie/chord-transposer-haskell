{-# LANGUAGE DeriveDataTypeable #-}
module Transposer where
import Data.Either (lefts, rights)
import Data.List (foldl')
-- import Player
import ChordParser
import LineSplitter
import Notes

main :: IO ()
main = transposeStdin 3


transposeStdin :: Transposition -> IO () 
transposeStdin amt = 
    interact $ printChordSheet . transposeChordSheet amt . parseChordSheet

transposeChordSheetStr :: Transposition -> String -> String
transposeChordSheetStr amt = 
    printChordSheet . transposeChordSheet amt . parseChordSheet

type Transposition = Int

type ChordSheetItem = (Either String Chord, (String, Int))

data ChordSheet = ChordSheet {csLines :: [ChordSheetLine]}

data ChordSheetLine = ChordSheetLine {cslItems :: [ChordSheetItem], 
                                      cslOrigText :: String}

parseChordSheet :: String -> ChordSheet
parseChordSheet input = ChordSheet $ map parseChordSheetLine (lines input)

parseChordSheetLine :: String -> ChordSheetLine
parseChordSheetLine line = ChordSheetLine (map buildItem posns) line
  where 
    buildItem x = (parseStringToChord (fst x), x)
    posns = findWordPositions line


transposeChordSheet :: Transposition -> ChordSheet -> ChordSheet
transposeChordSheet trans cs = cs{csLines = map (transposeChordSheetLine trans) (csLines cs)}

transposeChordSheetLine :: Transposition -> ChordSheetLine -> ChordSheetLine
transposeChordSheetLine trans csl = csl{cslItems = map (transposeChordSheetItem trans) (cslItems csl) }

transposeChordSheetItem :: Transposition -> ChordSheetItem -> ChordSheetItem
transposeChordSheetItem trans (Right c, z) = (Right c{rootNote = upSemitones (rootNote c) trans,
                                                      bassNote = newBassNote}, z)
 where newBassNote = case bassNote c of
                        Just b -> Just (upSemitones b trans)
                        Nothing -> Nothing
transposeChordSheetItem _trans (Left e, z)  = (Left e, z)

printChordSheet :: ChordSheet -> String
printChordSheet ls = unlines $  map printChordSheetLine (csLines ls)

printChordSheetLine :: ChordSheetLine -> String
printChordSheetLine (ChordSheetLine items orig) = 
  if length (rights (map fst items)) < length (lefts (map fst items))
    then orig
    else printToPositions items

printToPositions :: [ChordSheetItem] -> String
printToPositions items = posPrint $ map f items
  where f (Left _, (text, pos)) = (text, pos)
        f (Right c, (_, pos))   = (chordToSym c, pos)

posPrint :: [(String, Int)] -> String
posPrint = foldl' f ""
  where f acc (word, pos) = acc ++ replicate padLen ' ' ++ word
          where 
             padLen = max 1 (pos - length acc)

