-- avoid: Warning: orphan instance: instance Eq ParseError
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChordParser where

-- import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error(errorMessages)


import Test.HUnit
-- http://www.haskell.org/haskellwiki/HUnit_1.0_User%27s_Guide#Getting_Started

import Notes

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

pChordLines :: Parser [[Chord Note]]
pChordLines = do 
  cs <- endBy pChords newline
  eof
  return cs

pChords :: Parser [Chord Note]
pChords = sepBy pChord spaces

pChord :: Parser (Chord Note)
pChord = do
  r <- pNote
  c <- pQuality
  ds <- many pMaybeParenthesisedDecoration <|> return []
  bn <- pSlashBassNote <|> return Nothing
  return $ Chord r bn c ds

pMaybeParenthesisedDecoration :: Parser String
pMaybeParenthesisedDecoration = 
  try (betweenNotDiscarding (string "(") (string ")") (many1 pDecoration)) <|> pDecoration

betweenNotDiscarding :: (Monad m) => m [a] -> m [a] -> m [[a]] -> m [a]
betweenNotDiscarding pA pB pX = do
  a <- pA
  x <- pX
  b <- pB
  return $ a ++ (concat x) ++ b


-- Ca.. could be Caug... or Cadd...
-- Cm.. could be Cm... or Cmaj...
-- major quality is generally not stated
pQuality :: Parser ChordQuality
pQuality = do
  s <- string "dim" <|>try (string "aug") <|> string "+" <|> string "-" <|> try mNotGreedily <|> return ""
  return $ case s of
            "dim" -> CCDiminished
            "aug" -> CCAugmented
            "+"   -> CCAugmented
            "m"   -> CCMinor
            "-"   -> CCMinor
            _     -> CCMajor
  where
   -- we want m for minor in Cm but don't want to eat the m from Cmaj7
   mNotGreedily = do{ m <- char 'm' ; notFollowedBy (char 'a'); return [m] }

pDecoration :: Parser String
pDecoration = 
       try (do; mj <- choice ([string "maj", try (string "Maj"), string "M"]) ; i<- pInterval; return (mj ++ i) ) <|> 
       pInterval     <|> 
       pSuspended    <|>
       try (do; a <- string "add"; i <- pInterval; return (a ++ i) ) <|> 
       pModifiedInterval <|>
       try pSlashInterval <|>
       pDimInterval

  where
    -- sus is common, means sus4.
    pSuspended :: Parser String
    pSuspended = try (string "sus2") <|> try (string "sus4") <|> string "sus"

    pDimInterval = do
      d <- string "dim"
      i <- pInterval
      return (d++i)

    pSlashInterval :: Parser String
    pSlashInterval = do
      sl     <- string "/"
      optMod <- choice [string "+", string "-", return ""] 
      i      <- pInterval
      optModSuffix <- choice [string "+", string "-", return ""] 
      return $ sl ++ optMod ++ i ++ optModSuffix

    pInterval  :: Parser String -- TODO: this should be between 1 and 15 ?
    pInterval = many1 digit
    
    -- e.g. -5 or b9 or #11
    pModifiedInterval  :: Parser String 
    pModifiedInterval = do
      s       <- oneOf "+-#b" 
      ds      <- many1 digit -- TODO: this should be between 1 and 13 ?
      return ( s : ds )
      -- Can there be confusion betwen Gb+5 and aug (i.e. Gb+) ?



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


instance Eq ParseError where
  a == b = errorMessages a == errorMessages b

testPQuality ::  Test
testPQuality = TestList 
  [ "maj should fail to parse as quality" ~: Right CCMajor ~=? parse pQuality "" "maj"
  , "m   should parse as quality"         ~: Right CCMinor ~=? parse pQuality "" "m"
  ]

tests ::  IO Counts
tests       = runTestTT $ TestList [ testPQuality, testFromData testData ]
futureTests ::  IO Counts
futureTests = runTestTT $ TestList [ testFromData unsupportedTestData ]

testFromData ::  [(String, Chord Note)] -> Test
testFromData d = TestList $ map testIt d
  where 
    testIt :: (String, Chord Note) -> Test
    testIt (inp, expected) = ("When input is " ++ inp) ~: 
                           expected ~=? either (error "(no parse)") id (parse pChord "" inp)

-- TEST CONVENIENCES
initChord :: (Symmable a) => a -> ChordQuality -> Chord a 
initChord n qual = Chord { rootNote = n, bassNote = Nothing, cQuality = qual, cDecorations = [] }

-- conveniences for qualities
maj,mnr,aug,dim ::  ChordQuality
maj = CCMajor
mnr = CCMinor
aug = CCAugmented
dim = CCDiminished

sus :: Chord a -> Int -> Chord a
sus c i = c { cDecorations = cDecorations c ++ ["sus" ++ show i] }

-- conveniences for modifying a chord 
-- normally these are used to further its specification.  Ideally reflect this in the types.
with :: Chord a -> [String] -> Chord a
with baseChord decorations = baseChord { cDecorations = cDecorations baseChord ++ decorations }
on :: Chord a -> a -> Chord a
on   baseChord bNote       = baseChord { bassNote = Just bNote }


testData ::  [(String, Chord Note)]
testData = 
  [ ("A"         , crd A      maj                                 )
  , ("A#7-9"     , crd ASharp maj `with` ["7","-9"]               )
  , ("A-9"       , crd A      mnr `with` ["9"]                    )
  , ("A(-9)"     , crd A    maj `with` ["(-9)"]                   )
  , ("A7-9"      , crd A      maj `with` ["7", "-9"]              )
  , ("Aaug"      , crd A      aug                                 )
  , ("A9"        , crd A      maj `with` ["9"]                    )  
  , ("Bbm7"      , crd BFlat  mnr `with` ["7"]                    )
  , ("Ab"        , crd AFlat  maj                                 )
  , ("F#dim"     , crd FSharp dim                                 )
  , ("F#dim9"    , crd FSharp dim `with` ["9"]                    )
  , ("F#dim9/A"  , crd FSharp dim `with` ["9"]         `on` A     )
  , ("F#13"      , crd FSharp maj `with` ["13"]                   )
  , ("A/F#"      , crd A      maj                      `on` FSharp)
  , ("Amaj7"     , crd A      maj `with` ["maj7"]                 )
  , ("Amaj7/G"   , crd A      maj `with` ["maj7"]      `on` G     ) 
  , ("Cmmaj7"    , crd C      mnr `with` ["maj7"]                 )
  , ("Am"        , crd A      mnr                                 ) 
  , ("Am6"       , crd A      mnr `with` ["6"]                    )
  , ("Am7"       , crd A      mnr `with` ["7"]                    )
  , ("AM7"       , crd A      maj `with` ["M7"]                   )
  , ("Gsus2"     , crd G      maj `sus` 2                         )
  , ("Gsus4"     , crd G      maj `sus` 4                         )
  , ("Gsus9"     , crd G      maj `with` ["sus", "9"]             ) -- we don't try to change their writing style to sus4
  , ("G7sus4/D"  , crd G      maj `with` ["7"] `sus` 4 `on` D     )
  , ("A#m7-5"    , crd ASharp mnr `with` ["7","-5"]               )
  , ("Am/C"      , crd A      mnr                      `on` C     )
  , ("C+"        , crd C      aug                                 )
  , ("F7#5#9"    , crd F      maj `with` ["7", "#5", "#9"]        )
  , ("Cadd2"     , crd C      maj `with` ["add2"]                 )
  , ("Cadd9"     , crd C      maj `with` ["add9"]                 )
  , ("C7add13"   , crd C      maj `with` ["7", "add13"]           ) -- indicates no 9th or 11th (not that we care).
  , ("C5"        , crd C      maj `with` ["5"]                    )
  , ("AMaj7"     , crd A      maj `with` ["Maj7"]                 )
  , ("GM9"       , crd G      maj `with` ["M9"]                   )
  , ("Ab7#11"    , crd AFlat  maj `with` ["7", "#11"]             )
  , ("Ab7(#11)"  , crd AFlat  maj `with` ["7", "(#11)"]           )
  , ("Ab7(b9)"   , crd AFlat  maj `with` ["7", "(b9)"]            )
  , ("Ab7(b9#11)", crd AFlat  maj `with` ["7", "(b9#11)"]         )
  , ("A-(#5)"    , crd A      mnr `with` ["(#5)"]                 )
  , ("A-(Maj7)"  , crd A      mnr `with` ["(Maj7)"]                 ) -- parens
  , ("A-Maj7"    , crd A      mnr `with` ["Maj7"]                 )
  , ("E7(b9)"    , crd E      maj `with` ["7", "(b9)"]            )
  , ("Bb-7"      , crd BFlat  mnr `with` ["7"]                    ) -- the minus applies to the chord quality not the seventh.
  , ("Bb-/F"     , crd BFlat  mnr                      `on` F     )
  , ("Dmaj9"     , crd D      maj `with` ["maj9"]                 ) 
  , ("Dmaj9"     , crd D      maj `with` ["maj9"]                 ) 
  , ("Dmaj9/C"   , crd D      maj `with` ["maj9"]      `on` C     ) 
  -- These are strange - I think they're not indicating a bass note,
  --   they're just using slash as a separator between details.
  -- We don't care for current purposes.
  , ("A7/+5"     , crd A      maj `with` ["7", "/+5"]             )
  , ("A7/-9"     , crd A      maj `with` ["7", "/-9"]             )
  , ("Am7/+5"    , crd A      mnr `with` ["7", "/+5"]             )
  , ("Am7/-5"    , crd A      mnr `with` ["7", "/-5"]             )
  , ("A7/5+"     , crd A      maj `with` ["7", "/5+"]             )
  , ("F#m7/5-"   , crd FSharp mnr `with` ["7", "/5-"]             )
  
  , ("A6/9"      , crd A      maj `with` ["6", "/9"]              ) -- quite different from A7/C#
  , ("Am7/6"     , crd A      mnr `with` ["7", "/6"]              )
  , ("Am7/9"     , crd A      mnr `with` ["7", "/9"]              )

  
  -- Suspicious of these, they're all from http://chordlist.brian-amberg.de/en/guitar/standard/C/
  -- I haven't seen them in the wild.  No harm in supporting them, though.
  , ("C11dim9"   , crd C      maj `with` ["11", "dim9"]           )
  , ("Cm11dim9"  , crd C      mnr `with` ["11", "dim9"]           )
  , ("C13dim11"  , crd C      maj `with` ["13", "dim11"]          )
  , ("Cm13dim11" , crd C      mnr `with` ["13", "dim11"]          )
  , ("C13dim9"   , crd C      maj `with` ["13", "dim9"]           )
  , ("Cm13dim9"  , crd C      mnr `with` ["13", "dim9"]           )
  , ("C6dim5"    , crd C      maj `with` ["6", "dim5"]            )
  , ("C7dim5"    , crd C      maj `with` ["7", "dim5"]            )
  , ("Cm7dim5"   , crd C      mnr `with` ["7", "dim5"]            )
  , ("C7dim9"    , crd C      maj `with` ["7", "dim9"]            )
  , ("Cm7dim9"   , crd C      mnr `with` ["7", "dim9"]            )
  , ("C9dim5"    , crd C      maj `with` ["9", "dim5"]            )
  , ("Cmdim9"    , crd C      mnr `with` ["dim9"]                 )
  , ("Cmdim11"   , crd C      mnr `with` ["dim11"]                )
  , ("Cmdim13"   , crd C      mnr `with` ["dim13"]                )
  ] 
  where crd = initChord



-- linesWeWouldLikeToParse = ["Intro: Fmaj7 - G13 G7 - Gm7 - C7-9 - Fmaj7  F#13"]


unsupportedTestData ::  [(String, Chord Note)]
unsupportedTestData = 
  [ ("D7+"       , crd D      aug `with` ["7", "+"]               ) -- We want, but... ugh, this breaks the rule that chord quality be indicated before 7ths.
  , ("D7+5"      , crd D      aug `with` ["7", "+5"]              )

  , ("(D7b9)"    , crd D      maj `with` ["7", "b9"]              ) -- entire chord in parens.  And how do we restore the parens?
  , ("Cadd2*"    , crd C      maj `with` ["add2*"]                ) -- asterisk seen marking "unusual chords" - should we preserve unknowns?
  ]
  where crd = initChord

