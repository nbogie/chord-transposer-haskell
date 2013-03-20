module Player where
import ChordParser
import LineSplitter
import Notes

parseAndVoice :: String -> IO ()
parseAndVoice input = do
  print input
  case parse pChords "foo"input of
    Left err -> print $ "error: " ++ show err
    Right chords -> print $ zip chords (map voiceChord chords)

mainA :: IO ()
mainA = parseAndVoice (unwords sampleInputs)
mainB :: IO ()
mainB = do
  let c = Chord {rootNote = F, bassNote = Just E
                 , cDecorations = ["sus2"], cColor = CCAugmented} 
  print c
  let notes = voiceChord c
  print "NOTES: "
  print notes


voiceChord :: Chord -> [Note]
voiceChord c = 
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
