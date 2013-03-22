module Notes where


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
upSemitones :: Note -> Int -> Note
upSemitones n i = fromPure $ toEnum tag
  where tag = (fromEnum (toPure n) + i) `mod` 12

-- This type has only enharmonic equivalents.  one constructor per semitone.  Used internally only.
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

data Note = AFlat | A | ASharp | BFlat |  B | C | 
            CSharp | DFlat |  D | DSharp | EFlat |  E | 
            F | FSharp | GFlat |  G | GSharp deriving (Eq, Show, Ord)
data ChordQuality = CCMajor | CCMinor | CCDiminished| CCAugmented deriving (Eq, Show, Ord)
data Chord a = Chord 
           { rootNote::a
           , bassNote::Maybe a
           , cQuality::ChordQuality
           , cDecorations ::[String] 
           } deriving (Eq, Show, Ord)

chordToSym :: (Symmable a) => Chord a -> String
chordToSym Chord{bassNote = bn, rootNote = rn, cQuality = c, cDecorations  = decs } = 
  let slashBass = case bn of
              Just n -> '/':noteToSym n
              Nothing -> ""
  in noteToSym rn ++ colorToSym c ++ concat decs ++ slashBass

colorToSym :: ChordQuality -> String
colorToSym CCMajor = ""
colorToSym CCMinor = "m"
colorToSym CCDiminished = "dim"
colorToSym CCAugmented = "aug"

class Symmable a where
   noteToSym :: a -> String

data RomanNote = I | IIFlat | II | IIIFlat | III | IV | Vb | V | VIFlat | VI | VIIFlat | VII deriving (Eq, Show, Ord)

instance Symmable RomanNote where
   noteToSym I = "I"
   noteToSym IIFlat = "IIb"
   noteToSym II = "II"
   noteToSym IIIFlat = "IIIb"
   noteToSym III = "III"
   noteToSym IV = "IV"
   noteToSym Vb = "Vb"
   noteToSym V = "V"
   noteToSym VIFlat = "VIb"
   noteToSym VI = "VI"
   noteToSym VIIFlat = "VIIb"
   noteToSym VII = "VII"

instance Symmable Note where
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
