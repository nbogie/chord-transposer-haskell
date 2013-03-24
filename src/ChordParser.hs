module ChordParser where

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
  _counts <- tests
  return ()

main2 :: IO ()
main2    = interact parseAndShow

parseAndPrint :: String -> IO ()
parseAndPrint input = do
  print input
  case parse pChords "foo" input of
    Left err -> print $ "error: " ++ show err
    Right chords -> mapM_ print $ zip (words input) chords

parseAndShow :: String -> String
parseAndShow input = do
       let ls = lines input
       let eitherChords = map (parse pChordLines "foo") ls
       show eitherChords

parseStringToChord :: String -> Either String (Chord Note)
parseStringToChord xs = case parse pChord ("Input: "++xs) xs of
                          Left err -> Left $ show err
                          Right c -> Right c

-- can't get this to terminate correctly - eof is always unexpected
pChordLines :: Parser [[Chord Note]]
pChordLines = do 
  cs <- endBy pChords newline
  eof
  return cs

pChords :: Parser [Chord Note]
pChords = sepBy pChord spaces

-- can't do these yet - confusion with major seventh
-- Amaj7
-- Amaj7/G

pChord :: Parser (Chord Note)
-- Root [m | maj7] [dim] [7] [9] [-5 (secondary decorations)]        [/BassNote]
pChord = do
  r <- pNote
  c <- pQuality
  ds <- many pDecoration <|> return []
  bn <- pSlashBassNote <|> return Nothing
  return $ Chord r bn c ds

pDecoration :: Parser String
pDecoration = 
       string "7"    <|> 
       try (do; m <- string "M"; i <- pInterval; return (m ++ i) ) <|> 
       string "Maj7" <|> 
       pInterval     <|> 
       pSuspended    <|>
       try (do; a <- string "add"; i <- pInterval; return (a ++ i) ) <|> 
       try (string "+5") <|> string "+9" <|> -- confusion with aug (C+)
       try (string "-5") <|> string "-9" <|>
       try (string "b5") <|> string "b9" <|>
       try (string "#5") <|> string "#9" <|>
       try pSlashInterval 
  where
    -- sus is common, means sus4.
    pSuspended :: Parser String
    pSuspended = try (string "sus2") <|> try (string "sus4") <|> string "sus"

    pSlashInterval :: Parser String
    pSlashInterval = do
      sl     <- string "/"
      optMod <- choice [string "+", string "-", return ""] 
      i      <- pInterval
      return $ sl ++ optMod ++ i

    pInterval  :: Parser String -- TODO: this should be between 1 and 15 ?
    pInterval = many1 digit


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
    other -> error $ "BUG: pNote: unrecognised reformed note " ++ other

-- m (not followed by an a for "maj") (peeking - don't consume)
-- or "dim"
-- Ca could be Caug... or Cadd...
-- anything else is major

pQuality :: Parser ChordQuality
pQuality = do
  s <- string "dim" <|>try (string "aug") <|> string "+" <|> string "m" <|> return ""
  return $ case s of
            "dim" -> CCDiminished
            "aug" -> CCAugmented
            "+"   -> CCAugmented
            "m"   -> CCMinor
            _     -> CCMajor
tests       = testASet testData
futureTests = testASet unsupportedTestData

testASet testSet = runTestTT $ TestList $ map testIt testSet
  where 
    testIt :: (String, Chord Note) -> Test
    testIt (inp, expected) = ("When input is " ++ inp) ~: 
                           expected ~=? either (error "(no parse)") id (parse pChord "" inp)

-- TEST CONVENIENCES
initChord :: (Symmable a) => a -> ChordQuality -> Chord a 
initChord n color = Chord { rootNote = n, bassNote = Nothing, cQuality = color, cDecorations = [] }

-- conveniences for qualities
maj = CCMajor
mnr = CCMinor
aug = CCAugmented
dim = CCDiminished

sus c i = c { cDecorations = cDecorations c ++ ["sus" ++ show i] }

-- conveniences for modifying a chord 
-- normally these are used to further its specification.  Ideally reflect this in the types.
with :: Chord a -> [String] -> Chord a
with baseChord decorations = baseChord { cDecorations = cDecorations baseChord ++ decorations }
on :: Chord a -> a -> Chord a
on   baseChord bNote       = baseChord { bassNote = Just bNote }


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
  , ("Am6"   , crd A      mnr `with` ["6"]      )
  , ("Am7"   , crd A      mnr `with` ["7"]      )
  , ("AM7"   , crd A      maj `with` ["M7"]     )
  , ("Gsus2" , crd G      maj `sus` 2           )
  , ("Gsus4" , crd G      maj `sus` 4           )
  , ("Gsus9" , crd G      maj `with` ["sus", "9"] ) -- we don't try to change their writing style to sus4
  , ("G7sus4/D", crd G    maj `with` ["7"] `sus` 4 `on` D   )
  , ("A#m7-5", crd ASharp mnr `with` ["7","-5"] )
  , ("Am/C"  , crd A      mnr `on` C            )
  , ("C+"    , crd C      aug                   )
  , ("F7#5#9", crd F      maj `with` ["7", "#5", "#9"])
  , ("Cadd2" , crd C      maj `with` ["add2"]   )
  , ("Cadd9" , crd C      maj `with` ["add9"]   )
  , ("C7add13", crd C      maj `with` ["7", "add13"]   ) -- indicates no 9th or 11th (not that we care).
  , ("C5"    , crd C      maj `with` ["5"]   )
  , ("AMaj7" , crd A      maj `with` ["Maj7"]   )
  , ("GM9"   , crd G      maj `with` ["M9"]     )
  -- these are strange - I think they're not indicating a bass note, just using slash as a separator between details
  -- we don't care for current purposes.
  , ("A7/+5" , crd A      maj `with` ["7", "/+5"]      )
  , ("A7/-9" , crd A      maj `with` ["7", "/-9"]      )
  , ("Am7/+5", crd A      mnr `with` ["7", "/+5"]   )
  , ("Am7/-5", crd A      mnr `with` ["7", "/-5"]   )

  , ("A6/9"  , crd A      maj `with` ["6", "/9"]    ) -- quite different from A7/C#
  , ("Am7/6" , crd A      mnr `with` ["7", "/6"]    )
  , ("Am7/9" , crd A      mnr `with` ["7", "/9"]    )
  ] 
  where crd = initChord

unsupportedTestData = 
  [ ("Bb-7"  , crd BFlat  mnr `with` ["7"]      ) -- the minus applies to the chord colour not the seventh.
  , ("Bb-/F" , crd BFlat  mnr `on` F            )


  , ("C11dim9", crd C  maj `with` ["11", "dim9"]  )  -- C11dim9
  , ("Cm11dim9", crd C  mnr `with` ["11", "dim9"]  )  -- m11dim9
  , ("C13dim11", crd C maj `with`   ["13", "dim11"])
  , ("Cm13dim11", crd C mnr `with`  ["13", "dim11"] )
  , ("C13dim9", crd C maj `with`  ["13", "dim9"])
  , ("Cm13dim9", crd C mnr `with`  ["13", "dim9"] )
  , ("C6dim5", crd C maj `with`  ["6", "dim5"] )
  , ("C7dim5", crd C maj `with`  ["7", "dim6"] )
  , ("Cm7dim5", crd C mnr `with` ["7", "dim5"]  )
  , ("C7dim9", crd C maj `with` ["7", "dim9"]  )
  , ("Cm7dim9", crd C mnr `with` ["7", "dim9"]  )
  , ("C9dim5", crd C maj `with` ["dim5"]  )
  , ("Cmdim9", crd C mnr `with` ["dim9"]  )
  , ("Cmdim11", crd C mnr `with` ["dim11"]  )
  , ("Cmdim13", crd C mnr `with` ["dim13"]  )

  , ("A-(Maj7)", crd A    mnr `with` ["Maj7"]   ) -- w parens
  , ("A-(#5)"  , crd A    mnr `with` ["#5"]     )
  , ("E7(b9)", crd E      maj `with` ["7", "b9"]) -- parens
  , ("Cmmaj7", crd C      mnr `with` ["maj7"]   )
  , ("A-Maj7", crd A      mnr `with` ["Maj7"]   )

  , ("Cadd2*", crd C      maj `with` ["add2*"]   ) -- asterisk seen marking "unusual chords" - should we preserve unknowns?
  ]
  where crd = initChord

