module Player where
import ChordParser
import LineSplitter

import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec -- todo: remove knowledge from here

data  Third    =  MajorThird    | MinorThird  deriving (Show, Eq)
data  Seventh  =  MajorSeventh  | FlatSeventh deriving (Show, Eq)
data  Fifth    =  PerfectFifth  | FlatFifth | SharpFifth deriving (Show, Eq)

class Interval a where
  -- toAbs :: Note -> Interval -> Note
  semitones :: a -> Int

instance Interval Third where
  semitones  MinorThird  =  3
  semitones  MajorThird  =  4

instance Interval Fifth where
  semitones  FlatFifth     =  6
  semitones  PerfectFifth  =  7
  semitones  SharpFifth    =  8

instance Interval Seventh where
  semitones  FlatSeventh   =  10
  semitones  MajorSeventh  =  11

playChord :: Chord -> [Note]
playChord c = 
         let leftHand = fromMaybe (rootNote c) (bassNote c)
             r= rootNote c
             (thirdR, fifthR) = 
               case cColor c of
                 CCMajor -> (MajorThird, PerfectFifth)
                 CCMinor -> (MinorThird, PerfectFifth)
                 CCDiminished -> (MinorThird, FlatFifth)
                 CCAugmented -> (MajorThird, SharpFifth)
             -- TODO: add the root note, if it is not already present.
             thirdST = semitones thirdR
             third = upSemitones r thirdST
             isFlat5 = "-5" `elem` cDecorations c
             fifthST = if isFlat5 then semitones FlatFifth else semitones fifthR
             fifth = upSemitones r fifthST
         in [leftHand, r, third, fifth]

-- TODO: this needs an enumerable instance of Note which can wrap around
upSemitones :: Note -> Int -> Note
upSemitones n i = fromPure $ toEnum tag
  -- todo: handle loop around
  where tag = (fromEnum (toPure n) + i) `mod` 12

data PureNote = PA | PASharp | PB | PC | PCSharp | PD | PDSharp | PE | PF | PFSharp | PG | PGSharp deriving (Show, Eq, Ord, Enum)

toPure :: Note -> PureNote
toPure  A       =  PA
toPure  ASharp  =  PASharp
toPure  BFlat   =  PASharp
toPure  B       =  PB
toPure  C       =  PC
toPure  CSharp  =  PCSharp
toPure  DFlat   =  PCSharp
toPure  D       =  PD
toPure  DSharp  =  PDSharp
toPure  EFlat   =  PDSharp
toPure  E       =  PE
toPure  F       =  PF
toPure  FSharp  =  PFSharp
toPure  GFlat   =  PFSharp
toPure  G       =  PG
toPure  GSharp  =  PGSharp
toPure  AFlat   =  PGSharp

fromPure :: PureNote -> Note
fromPure  PA = A       
fromPure  PASharp = ASharp  
fromPure  PB = B       
fromPure  PC = C       
fromPure  PCSharp = CSharp  
fromPure  PD = D       
fromPure  PDSharp = DSharp  
fromPure  PE = E       
fromPure  PF = F       
fromPure  PFSharp = FSharp  
fromPure  PG = G       
fromPure  PGSharp = GSharp  
 
-- Question: how to modify the already derived Enum of PureNote to cycle from top note to bottom note?

-- data ChordColor = CCMajor | CCMinor | CCDiminished| CCAugmented deriving (Eq, Show, Ord)
-- data Chord = Chord {rootNote::Note, bassNote::Maybe Note, cColor::ChordColor, 
--                    cDecorations ::[String] } deriving (Eq, Show, Ord)
-- main = maina

maina = parseAndPlay (unwords sampleInputs)

parseAndPlay :: String -> IO ()
parseAndPlay input = do
  print input
  case parse pChords "foo"input of
    Left err -> print $ "error: " ++ show err
    Right chords -> print $ zip chords (map playChord chords)

mainb = do
  let c = Chord {rootNote = F, bassNote = Just E
                 , cDecorations = ["sus2"], cColor = CCAugmented} 
  print c
  let notes = playChord c
  print "NOTES: "
  print notes
