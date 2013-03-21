{-# LANGUAGE DeriveDataTypeable #-}
module Transposer where
import Debug.Trace
import Data.Either (lefts, rights)
import Data.Char (toLower)
import Data.List (foldl', isPrefixOf)
-- import Player
import ChordParser hiding (main)
import LineSplitter
import Notes
-- import Control.Arrow ((>>^))
-- import Data.Either

main :: IO ()
main = transposeStdin 3


transposeStdin :: Transposition -> IO () 
transposeStdin amt = 
    interact $ printChordSheet . transposeChordSheet amt . parseChordSheet

transposeChordSheetStr :: Transposition -> String -> String
transposeChordSheetStr amt = 
    printChordSheet . transposeChordSheet amt . parseChordSheet

type Transposition = Int

data ChordSheet = ChordSheet {csLines :: [ChordSheetLine]} deriving (Show)

data ChordSheetLine = ChordSheetLine {cslItems :: [ChordSheetItem], 
                                      cslOrigText :: String} deriving (Show)

type ChordSheetItem = (Either String Chord, (String, Int))


modifyLines :: ([ChordSheetLine] -> [ChordSheetLine]) -> ChordSheet -> ChordSheet
modifyLines fn cs = cs { csLines = fn $ csLines cs }

parseChordSheet :: String -> ChordSheet
parseChordSheet input = ChordSheet $ map parseChordSheetLine (lines input)

parseChordSheetLine :: String -> ChordSheetLine
parseChordSheetLine line = ChordSheetLine (map buildItem posns) line
  where 
    buildItem x = (parseStringToChord (fst x), x)
    posns = findWordPositions line

transposeChordSheet :: Transposition -> ChordSheet -> ChordSheet
transposeChordSheet trans = modifyLines $ map (transposeChordSheetLine trans)

transposeChordSheetLine :: Transposition -> ChordSheetLine -> ChordSheetLine
transposeChordSheetLine trans csl = csl{cslItems = map (transposeChordSheetItem trans) (cslItems csl) }

-- we don't transpose nonchords (Left items)
transposeChordSheetItem :: Transposition -> ChordSheetItem -> ChordSheetItem
transposeChordSheetItem trans (c, z) = (fmap (transposeChord trans) c, z)

transposeChord trans c = c{rootNote = upSemitones (rootNote c) trans, bassNote = newBassNote}
 where newBassNote = case bassNote c of
                        Just b -> Just (upSemitones b trans)
                        Nothing -> Nothing

printChordSheet :: ChordSheet -> String
printChordSheet ls = unlines $  map printChordSheetLine (csLines ls)

printChordSheetLine :: ChordSheetLine -> String
printChordSheetLine line@(ChordSheetLine items orig) = 
  if (lineLooksLikeChords line)
    then printToPositions items
    else orig

-- Todo: Improve decision-making over whether a line is chords or not.
-- a single line of text like "Coda:" will wrongly be parsed as a C chord with unrecognised, but carried, detail.
-- This is because we wish to be so accommodating with allowing unrecognised chords past.
-- Neither "Coda:", nor "Coro", nor "Bridge" are likely to be legal chords under anyone's notation!
-- Ah, what to do for: "Repeat Gm7 for outro"  - The chord won't get transposed under current rules.
lineLooksLikeChords (ChordSheetLine items orig) = 
  length (rights chordAttempts) >= length (lefts chordAttempts)
  && (not (lineContainsKeywords orig))
    where 
      chordAttempts = (map fst items)

lineContainsKeywords str = any (\kw -> any (isPrefixOf kw . lower) (words str)) keywords
  where
    keywords = map (map toLower) ["Bridge", "Chorus", "Intro", "Verse", "Coro", "Coda"] -- ugh!
    lower = map toLower

printToPositions :: [ChordSheetItem] -> String
printToPositions items = posPrint $ map f items
  where f (Left _, (text, pos)) = (text, pos)
        f (Right c, (_, pos))   = (chordToSym c, pos)

posPrint :: [(String, Int)] -> String
posPrint = foldl' f ""
  where f acc (word, pos) = acc ++ replicate padLen ' ' ++ word
          where 
             padLen = max 1 (pos - length acc)

