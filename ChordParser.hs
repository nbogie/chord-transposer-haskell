module ChordParser where

import Text.ParserCombinators.Parsec
import Notes



-- This could be useful as the core of a service which could take any ascii sheet music and change key
-- or re-write relative to the root, using roman numerals (to analyze progressions more purely).
-- it could find and add guitar chord symbols, or make midi of the piano part, etc (but it doesn't know timing)
-- must be able to support chords it doesn't understand (strange notations, human mistakes)
-- We want to preserve position in the chord sheet so we can do substitutions (and extra annotations) in place
-- main :: IO ()
-- main    = parseAndPrint (unwords sampleInputs)
main2 :: IO ()
main2    = interact parseAndShow

parseAndPrint :: String -> IO ()
parseAndPrint input = do
  print input
  case parse pChords "foo" input of
    Left err -> print $ "error: " ++ (show err)
    Right chords -> print chords

parseAndShow :: String -> String
parseAndShow input = do
       let ls = lines input
       let eitherChords = map (parse pChordLines "foo") ls
       show eitherChords

parseStringToChord :: String -> Either String Chord
parseStringToChord xs = case parse pChord ("Input: "++xs) xs of
                          Left err -> Left $ show err
                          Right c -> Right c

-- can't get this to terminate correctly - eof is always unexpected
pChordLines :: Parser [[Chord]]
pChordLines = do 
  cs <- endBy pChords newline
  eof
  return cs

sampleInputs :: [String]
sampleInputs = [ "A"
  , "A#7-9"
  , "Aaug"
  , "A9"
  , "Bbm7"
  , "Ab"
  , "F#dim"
  , "A/F#"
  , "Am"
  , "Am7"
  , "AM7"
  , "Gsus2"
  , "Gsus4"
  , "A#m7-5"
  , "Am/C"
  ]

pChords :: Parser [Chord]
pChords = sepBy pChord spaces

-- can't do these yet - confusion with major seventh
-- Amaj7
-- Amaj7/G

pChord :: Parser Chord
-- Root [m | maj7] [dim] [7] [9] [-5 (secondary decorations)]        [/BassNote]
pChord = do
  r <- pNote
  c <- pColor
  ds <- many pDecoration <|> return []
  bn <- pBaseNote <|> return Nothing
  return $ Chord r bn c ds

pDecoration :: Parser String
pDecoration = do 
  d <- string "7" <|> string "M7" <|> string "9" <|> try (string "11") <|> try (string "13") <|> try (string "sus2") <|> try (string "sus4") <|> try (string "-5") <|> string "-9" 
  return d

pBaseNote :: Parser (Maybe Note)
pBaseNote = fmap Just (string "/" >> pNote)

pNote :: Parser Note
pNote = do
  rRaw <- oneOf "ABCDEFG"
  rshp <- string "#" <|> string "b" <|> return ""
  return $ case rRaw:rshp of
    "Ab" -> AFlat
    "A" -> A
    "A#" -> ASharp
    "Bb" -> BFlat
    "B" -> B
    "C" -> C
    "C#" -> CSharp
    "Db" -> DFlat
    "D" -> D
    "D#" -> DSharp
    "Eb" -> EFlat
    "E" -> E
    "F" -> F
    "F#" -> FSharp
    "Gb" -> GFlat
    "G" -> G
    "G#" -> GSharp
    other -> error $ "BUG: unrecognised reformed note " ++ other

-- m (not followed by an a for "maj") (peeking - don't consume)
-- or "dim"
-- anything else is major
pColor :: Parser ChordColor
pColor = do
  s <- string "dim" <|>string "aug" <|> string "m" <|> return ""
  return $ case s of
            "dim" -> CCDiminished
            "aug" -> CCAugmented
            "m"   -> CCMinor
            _     -> CCMajor

