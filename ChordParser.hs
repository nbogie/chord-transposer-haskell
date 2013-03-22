module ChordParser where

import Data.Either
import Text.ParserCombinators.Parsec

import Notes

import Test.HUnit
-- http://www.haskell.org/haskellwiki/HUnit_1.0_User%27s_Guide#Getting_Started


-- This could be useful as the core of a service which could take any ascii sheet music and change key
-- or re-write relative to the root, using roman numerals (to analyze progressions more purely).
-- it could find and add guitar chord symbols, or make midi of the piano part, etc (but it doesn't know timing)
-- must be able to support chords it doesn't understand (strange notations, human mistakes)
-- We want to preserve position in the chord sheet so we can do substitutions (and extra annotations) in place
main :: IO ()

main    = do
  tests
  return ()

main2 :: IO ()
main2    = interact parseAndShow

parseAndPrint :: String -> IO ()
parseAndPrint input = do
  print input
  case parse pChords "foo" input of
    Left err -> print $ "error: " ++ (show err)
    Right chords -> mapM_ print $ zip (words input) chords

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

pChords :: Parser [Chord]
pChords = sepBy pChord spaces

-- can't do these yet - confusion with major seventh
-- Amaj7
-- Amaj7/G

pChord :: Parser Chord
-- Root [m | maj7] [dim] [7] [9] [-5 (secondary decorations)]        [/BassNote]
pChord = do
  r <- pNote
  c <- pQuality
  ds <- many pDecoration <|> return []
  bn <- pSlashBassNote <|> return Nothing
  return $ Chord r bn c ds

pDecoration :: Parser String
pDecoration = do 
  d <- string "7" <|> string "M7" <|> 
       string "9" <|> try (string "11") <|> try (string "13") <|> 
       try (string "sus2") <|> string "sus4" <|> 
       try (string "-5") <|> string "-9" <|>
       try (string "#5") <|> string "#9" 
  return d

pSlashBassNote :: Parser (Maybe Note)
pSlashBassNote = fmap Just (string "/" >> pNote)

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
pQuality :: Parser ChordQuality
pQuality = do
  s <- string "dim" <|>string "aug" <|> string "+" <|> string "m" <|> return ""
  return $ case s of
            "dim" -> CCDiminished
            "aug" -> CCAugmented
            "+"   -> CCAugmented
            "m"   -> CCMinor
            _     -> CCMajor





-- tests = runTestTT $ TestList [ 4 ~=? 10 ]
{- 
-}
tests = runTestTT $ TestList $ map testIt testData
  where 
    testIt :: (String, Chord) -> Test
    testIt (inp, exp) = ("When input is " ++ inp) ~: 
                           exp ~=? either (error "(no parse)") id (parse pChord "" inp)

initChord :: Note -> ChordQuality -> Chord
initChord n color = Chord { rootNote = n, bassNote = Nothing, cQuality = color, cDecorations = [] }

-- conveniences for qualities
maj = CCMajor
mnr = CCMinor
aug = CCAugmented
dim = CCDiminished

sus c i = c { cDecorations = cDecorations c ++ ["sus" ++ show i] }

testData = 
  [ ("A"     , crd A      maj                   )
  , ("A#7-9" , crd ASharp maj `with` ["7","-9"] )
  , ("Aaug"  , crd A      aug                   )
  , ("A9"    , crd A      maj `with` ["9"]      )
  , ("Bbm7"  , crd BFlat  mnr `with` ["7"]      )
  , ("Ab"    , crd AFlat  maj                   )
  , ("F#dim" , crd FSharp dim                   )
  , ("A/F#"  , crd A      maj `on` FSharp       )
  , ("Am"    , crd A      mnr                   ) 
  , ("Am7"   , crd A      mnr `with` ["7"]      )
  , ("AM7"   , crd A      maj `with` ["M7"]     )
  , ("Gsus2" , crd G      maj `sus` 2           )
  , ("Gsus4" , crd G      maj `sus` 4           )
  , ("Gsus9" , crd G      maj `sus` 4 `with` ["9"] )
  , ("G7sus4/D", crd G    maj `with` ["7"] `sus` 4 `on` D   )
  , ("A#m7-5", crd ASharp mnr `with` ["7","-5"] )
  , ("Am/C"  , crd A      mnr `on` C            )
  , ("C+"    , crd C      aug                   )
  , ("F7#5#9"  , crd F      maj `with` ["7", "#5", "#9"])
  ] 
  where crd = initChord
unsupportedTestData = 
  [ ("Bb-7"  , crd BFlat  mnr `with` ["7"]      ) -- the minus applies to the chord colour not the seventh.
  , ("Bb-/F" , crd BFlat  mnr `on` F            )
  , ("AMaj7" , crd A      maj `with` ["Maj7"]   )
  , ("A-(Maj7)", crd A    mnr `with` ["Maj7"]   ) -- w parens
  , ("A-(#5)"  , crd A    mnr `with` ["#5"]     )
  , ("E7(b9)", crd E      maj `with` ["7", "b9"])
  , ("A-Maj7", crd A      mnr `with` ["Maj7"]   )
  ]
  where crd = initChord

-- conveniences for modifying a chord 
-- normally these are used to further its specification.  Ideally reflect this in the types.
with :: Chord -> [String] -> Chord
with baseChord decorations = baseChord { cDecorations = cDecorations baseChord ++ decorations }
on :: Chord -> Note -> Chord
on   baseChord bNote       = baseChord { bassNote = Just bNote }
