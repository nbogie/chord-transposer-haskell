module Notes where
import Data.Char (toLower)

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

-- TODO: this needs an enumerable instance of Note which can wrap around
upSemitones :: Int -> Note -> Note
upSemitones i n = fromPure $ toEnum tag
  where tag = (fromEnum (toPure n) + i) `mod` 12

-- Question: how to modify the already derived Enum of PureNote to cycle from top note to bottom note?

data Note = AFlat | A | ASharp | BFlat |  B | C | 
            CSharp | DFlat |  D | DSharp | EFlat |  E | 
            F | FSharp | GFlat |  G | GSharp deriving (Eq, Show, Ord)
data Chord a = Chord 
           { rootNote::a
           , bassNote::Maybe a
           , cQuality::ChordQuality
           , cDecorations ::[String] 
           } deriving (Eq, Show, Ord)

data ChordQuality = CCMajor | CCMinor | CCDiminished| CCAugmented deriving (Eq, Show, Ord)

isMinorQuality ::  ChordQuality -> Bool
isMinorQuality CCMinor = True
isMinorQuality _ = False

chordToSym :: (Symmable a) => Chord a -> String
chordToSym Chord{bassNote = bn, rootNote = rn, cQuality = qual, cDecorations  = decs } = 
  let slashBass = case bn of
              Just n -> '/':noteToSym n
              Nothing -> ""
      rnSym = if isMinorQuality qual 
               then noteToSymForMinorChord rn
               else noteToSym rn
      qualSym = if reflectsQuality rn && isMinorQuality qual
                  then "" else qualityToSym qual 
  in rnSym ++ qualSym ++ concat decs ++ slashBass

qualityToSym :: ChordQuality -> String
qualityToSym CCMajor = ""
qualityToSym CCMinor = "m"
qualityToSym CCDiminished = "dim"
qualityToSym CCAugmented = "aug"

data RomanNote = I | IIFlat | II | IIIFlat | III | IV | VFlat | V | VIFlat | VI | VIIFlat | VII deriving (Eq, Show, Ord)

class Symmable a where
   noteToSym :: a -> String
   -- the Bool is "is minor".  I'd rather not have notes knowing about chord qualities. That we do here is a smell.
   noteToSymForMinorChord :: a -> String
   fromPure  :: Int -> a
   toPure    :: a -> Int
   reflectsQuality :: a -> Bool

instance Symmable RomanNote where
   reflectsQuality = const True -- in a chord, IV is major, iv is minor
   noteToSym I = "I"
   noteToSym IIFlat = "IIb"
   noteToSym II = "II"
   noteToSym IIIFlat = "IIIb"
   noteToSym III = "III"
   noteToSym IV = "IV"
   noteToSym VFlat = "Vb"
   noteToSym V = "V"
   noteToSym VIFlat = "VIb"
   noteToSym VI = "VI"
   noteToSym VIIFlat = "VIIb"
   noteToSym VII = "VII"

   noteToSymForMinorChord = map toLower .  noteToSym

   toPure I = 0
   toPure IIFlat = 1
   toPure II = 2
   toPure IIIFlat = 3
   toPure III = 4
   toPure IV = 5
   toPure VFlat = 6
   toPure V = 7
   toPure VIFlat = 8
   toPure VI = 9
   toPure VIIFlat = 10
   toPure VII = 11

   fromPure 0 = I
   fromPure 1 = IIFlat
   fromPure 2 = II
   fromPure 3 = IIIFlat 
   fromPure 4 = III 
   fromPure 5 = IV 
   fromPure 6 = VFlat 
   fromPure 7 = V 
   fromPure 8 = VIFlat 
   fromPure 9 = VI 
   fromPure 10 = VIIFlat 
   fromPure 11 = VII 
   fromPure other = error $ "BUG: fromPure no such note: "++show other 

semitoneDiff :: Note -> Note -> Int
semitoneDiff k n = (12+ toPure n - toPure k) `mod` 12

toRoman :: Note -> Note -> RomanNote
toRoman k n = fromPure $ semitoneDiff k n

instance Symmable Note where
   reflectsQuality = const False -- in a chord, F could be major or minor
   noteToSym AFlat = "Ab"
   noteToSym A = "A"
   noteToSym ASharp = "A#"
   noteToSym B = "B"
   noteToSym BFlat = "Bb"
   noteToSym C = "C"
   noteToSym CSharp = "C#"
   noteToSym D = "D"
   noteToSym DSharp = "D#"
   noteToSym DFlat = "Db"
   noteToSym E = "E"
   noteToSym EFlat = "Eb"
   noteToSym F = "F"
   noteToSym FSharp = "F#"
   noteToSym G = "G"
   noteToSym GSharp = "G#"
   noteToSym GFlat = "Gb"

   noteToSymForMinorChord = noteToSym

   toPure  A       =  0
   toPure  ASharp  =  1
   toPure  BFlat   =  1
   toPure  B       =  2
   toPure  C       =  3
   toPure  CSharp  =  4
   toPure  DFlat   =  4
   toPure  D       =  5
   toPure  DSharp  =  6
   toPure  EFlat   =  6
   toPure  E       =  7
   toPure  F       =  8
   toPure  FSharp  =  9
   toPure  GFlat   =  9
   toPure  G       =  10
   toPure  GSharp  =  11
   toPure  AFlat   =  11

   fromPure  0 = A       
   fromPure  1 = ASharp  
   fromPure  2 = B       
   fromPure  3 = C       
   fromPure  4 = CSharp  
   fromPure  5 = D       
   fromPure  6 = DSharp  
   fromPure  7 = E       
   fromPure  8 = F       
   fromPure  9 = FSharp  
   fromPure  10 = G       
   fromPure  11 = GSharp  
   fromPure other = error $ "BUG: fromPure no such note: "++show other 
   -- TODO: don't use an Int - they can have values constructed which we consider illegal
