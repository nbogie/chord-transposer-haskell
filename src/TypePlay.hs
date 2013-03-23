data ChordQuality = CCMajor | CCMinor | CCDiminished| CCAugmented deriving (Eq, Show, Ord)
data Chord a = Chord a ChordQuality deriving (Show)

chordToSym :: (Symmable a) => Chord a -> String
chordToSym (Chord root qual) = noteToSym root ++ show qual

data Note = AFlat | A | ASharp | BFlat |  B | C | CSharp | DFlat |  D | DSharp 
                  | EFlat |  E | F | FSharp | GFlat |  G | GSharp deriving (Eq, Show, Ord)

main = do
  print $ map chordToSym [Chord EFlat CCMajor, Chord BFlat CCMajor]
  print $ map chordToSym [Chord IIFlat CCMajor]

                
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
