module Transposer where
import Debug.Trace
import Data.Either (lefts, rights)
import Data.Char (toLower)
import Data.List (foldl', isPrefixOf, nub, sort)
import ChordParser hiding (main, tests)
import LineSplitter
import Notes
import Test.HUnit
import Utils (bool, escapeHTML)
import Data.Maybe (listToMaybe, fromMaybe)

main :: IO ()
main = tests >> transposeStdin PlainText 0 True-- flag is to romanize 

type Pos = Int

type Transposition = Int

data ChordSheet a = ChordSheet {csLines :: [ChordSheetLine a]} deriving (Show)

data ChordSheetLine a = ChordSheetLine {cslItems :: [ChordSheetItem a], 
                                        cslOrigText :: String} deriving (Show)

type ChordSheetItem a = (Either String (Chord a), (String, Pos))

-- currently, a chord sheet may have many "chords" optimistically partially parsed.
-- Ignore these by asking only for the chords from lines that look like chords
chordsInSheet ::  ChordSheet a -> [Chord a]
chordsInSheet = rights . map fst . concatMap cslItems . filter lineLooksLikeChords . csLines

transposeStdin :: PrintFormat -> Transposition -> Bool -> IO () 
transposeStdin format amt shouldRomanize = 
    interact $ bool (printChordSheet format . romanizeSheet) 
                     (printChordSheet format . id)
                     shouldRomanize 
               . transposeChordSheet amt . traceInline "chord summary" summarizeChords . parseChordSheet

traceInline ::  String -> (a -> String) -> a -> a
traceInline hdr fn a = trace (hdr ++ "\n" ++ fn a) a

showAllChords :: (Symmable a) => ChordSheet a -> String
showAllChords   = unlines . map chordToSym . chordsInSheet
summarizeChords ::  (Symmable a, Eq a, Ord a) => ChordSheet a -> String
summarizeChords = unlines . map chordToSym . sort . nub . chordsInSheet  

romanizeSheet :: ChordSheet Note -> ChordSheet RomanNote
romanizeSheet cs = withEachChordInSheet (romanizeInKey key) cs
  where key = guessKey cs

guessKey :: ChordSheet Note -> Key -- TODO: return probability, or perhaps alternatives
guessKey cs = fromMaybe (error "couldn't guess key") (keyFromFirstChord cs) --TODO: handle unguessable key (e.g. no chords)

keyFromFirstChord :: ChordSheet Note -> Maybe Key
keyFromFirstChord cs = fmap keyFromChord firstChord
  where firstChord = listToMaybe $ chordsInSheet cs

-- relative major if the chord is minor, otherwise, blindly, the major key in that root.
keyFromChord :: Chord Note -> Key
keyFromChord c = (rn, MajorScale)
  where rn = (if cQuality c == CCMinor then relativeMajor else id) $ rootNote c
        relativeMajor = upSemitones 3

chordQualityToKeyType ::  ChordQuality -> ScaleType
chordQualityToKeyType CCMinor = MinorScale
chordQualityToKeyType CCMajor = MajorScale
chordQualityToKeyType CCDiminished = MajorScale -- ?
chordQualityToKeyType CCAugmented = MajorScale  -- ?

modifyLines :: ([ChordSheetLine a] -> [ChordSheetLine b]) -> ChordSheet a -> ChordSheet b
modifyLines fn cs = cs { csLines = fn $ csLines cs }

withEachChordInSheet :: (Chord a -> Chord b) -> ChordSheet a -> ChordSheet b
withEachChordInSheet f    = modifyLines $ map (withEachChordInLine f)

withEachChordInLine :: (Chord t -> Chord a) -> ChordSheetLine t -> ChordSheetLine a
withEachChordInLine f csl = csl { cslItems = map (withOneCSLI f) origItems } 
  where origItems = cslItems csl
-- we don't transpose nonchords (Left items)
withOneCSLI :: (Chord a -> Chord b) -> ChordSheetItem a -> ChordSheetItem b 
withOneCSLI f (c,z)       = (fmap f c, z)

transposeChordSheetStr :: PrintFormat -> Transposition -> String -> String
transposeChordSheetStr fmt amt = 
    printChordSheet fmt . transposeChordSheet amt . parseChordSheet

parseChordSheet :: String -> ChordSheet Note
parseChordSheet input = ChordSheet $ map parseChordSheetLine (lines input)

parseChordSheetLine :: String -> ChordSheetLine Note
parseChordSheetLine line = ChordSheetLine (map buildItem posns) line
  where 
    buildItem x = (parseStringToChord (fst x), x)
    posns = wordsAndPositions line

transposeChordSheet :: Transposition -> ChordSheet Note -> ChordSheet Note
transposeChordSheet trans = withEachChordInSheet (transposeChord trans)

transposeChord ::  Transposition -> Chord Note -> Chord Note
transposeChord trans c = c { rootNote = upSemitones trans (rootNote c)
                           , bassNote = fmap (upSemitones trans) (bassNote c) 
                           }

printChordSheet :: (Symmable a) => PrintFormat -> ChordSheet a -> String
printChordSheet PlainText ls  = unlines $ map (printChordSheetLine PlainText) (csLines ls)
printChordSheet TaggedText ls = prependStylesheet $ wrapInPre $unlines $ map (printChordSheetLine TaggedText) (csLines ls)
  where 
    wrapInPre text = "<pre class='chordsheet'>" ++ text ++ "</pre>"
    prependStylesheet text = "<style>span { color : blue; font-weight: bold}</style>" ++ text

printChordSheetLine :: (Symmable a) => PrintFormat -> ChordSheetLine a -> String
printChordSheetLine format line@(ChordSheetLine items orig) = 
  if lineLooksLikeChords line
    then printCSIsAtPositions format items
    else printNonChordLine    format orig
  where printNonChordLine PlainText = id
        printNonChordLine TaggedText = escapeHTML

-- Todo: Improve decision-making over whether a line is chords or not.
-- a single line of text like "Coda:" will wrongly be parsed as a C chord with unrecognised, but carried, detail.
-- This is because we wish to be so accommodating with allowing unrecognised chords past.
-- Neither "Coda:", nor "Coro", nor "Bridge" are likely to be legal chords under anyone's notation!
-- Ah, what to do for: "Repeat Gm7 for outro"  - The chord won't get transposed under current rules.
lineLooksLikeChords ::  ChordSheetLine t -> Bool
lineLooksLikeChords (ChordSheetLine items orig) = 
  length (rights chordAttempts) >= length (lefts chordAttempts)
  && not (containsKeywords orig)
    where 
      chordAttempts = map fst items

      containsKeywords str = any (\kw -> any (isPrefixOf kw . lower) (words str)) keywords
        where
          keywords = map (map toLower) ["Instr", "Break", "Bridge", "Chorus", "Intro", "Verse", "Coro", "Coda"] -- ugh!
          lower = map toLower

data PrintFormat = TaggedText | PlainText deriving (Show, Eq)

printCSIsAtPositions :: (Symmable a) => PrintFormat-> [ChordSheetItem a] -> String
printCSIsAtPositions printFormat items = 
  printStringsAtPositions printFormat $ map f items
  where f (Left _, (text, pos)) = (PlainElem, text, pos)
        f (Right c, (_, pos))   = (ChordElem, chordToSym c, pos)

data MarkupElement = PlainElem | ChordElem deriving (Show, Eq)


printStringsAtPositions :: PrintFormat -> [(MarkupElement, String, Pos)] -> String
printStringsAtPositions TaggedText = snd. generateTextAndHTML
printStringsAtPositions PlainText = fst . generateTextAndHTML

type HtmlString  = String
type PlainString = String
generateTextAndHTML ::  [(MarkupElement, String, Pos)] -> (PlainString, HtmlString)
generateTextAndHTML = foldl' f ("","")
  where 
        f :: (PlainString,HtmlString) -> (MarkupElement, String, Pos) -> (PlainString, HtmlString)
        f (accText,accHtml) (elemType, word, pos) = (text, html)
          where text = accText ++ replicate padLen ' ' ++ word
                html = accHtml ++ replicate padLen ' ' ++ spanIt elemType (escapeHTML word)
                padLen = if pos == 0
                         then 0
                         else max 1 (pos - length accText)
                spanIt PlainElem x = "" ++ x ++ ""
                spanIt ChordElem x = "<span class='pc'>" ++ x ++ "</span>"
         
tests ::  IO Counts
tests = runTestTT $ TestList [printStringsTests, romanizeTests]

romanizeTests ::  Test
romanizeTests = TestList 
  $ map (testRomanizeInKey C  MajorScale) [("Em9", "iii9")
  ,("Am7", "vi7"), ("D9", "II9"), ("Gsus4/F", "Vsus4/IV")]
data ScaleType = MajorScale | MinorScale deriving (Show) -- TODO: multiple minor keys

type Key = (Note, ScaleType)
rootOfKey :: Key -> Note
rootOfKey = fst

romanizeInKey ::  Key -> Chord Note -> Chord RomanNote
romanizeInKey (keyRoot,_) chord = Chord { 
  rootNote = newRoot, 
  bassNote = newBassNote,
  cQuality = cQuality chord,
  cDecorations = cDecorations chord }
  where 
    newRoot     = toRoman keyRoot (rootNote chord)
    newBassNote = fmap (toRoman keyRoot) $ bassNote chord

testRomanizeInKey ::  Note -> ScaleType -> (String, String) -> Test
testRomanizeInKey keyRoot keyScale (inp, expectedSym) = 
  label ~: expectedSym ~=? chordToSym (romanizeInKey (keyRoot, keyScale) chord)
  where 
    label = "in "++show(keyRoot, keyScale) ++ " with input " ++ inp
    chord = case parseStringToChord inp of
                Right c -> c
                Left err -> error err -- todo: test failure
  
printStringsTests ::  Test
printStringsTests = TestList 
  [ "Hi" ~=? printStringsAtPositions PlainText [(ChordElem, "Hi", 0)] -- no initial padding
  , " Foo" ~=? printStringsAtPositions PlainText [(ChordElem, "Foo", 1)] -- but original leading whitespace should be preserved
  ]
