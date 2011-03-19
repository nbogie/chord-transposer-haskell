module Main where
import Data.Either (lefts, rights)
import Player
import ChordParser
import LineSplitter

main = interact (printChordSheet . transposeChordSheet . parseChordSheet)

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

transposeChordSheet :: ChordSheet -> ChordSheet
transposeChordSheet = id

printChordSheet :: ChordSheet -> String
printChordSheet ls = unlines $  map printChordSheetLine (csLines ls)

printChordSheetLine :: ChordSheetLine -> String
printChordSheetLine (ChordSheetLine items orig) = 
  if length (rights (map fst items)) < length (lefts (map fst items))
    then orig
    else printToPositions printChordSheetItem items

printToPositions :: (ChordSheetItem -> String) -> [ChordSheetItem] -> String
printToPositions fn items = 
 concatMap fn items

printChordSheetItem :: ChordSheetItem -> String
printChordSheetItem (csi, (text, pos)) = 
  case csi of
    Left err -> text
    Right c  -> chordToSym c

