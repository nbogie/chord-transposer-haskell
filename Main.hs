{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Transposer hiding (main)

import System.Console.CmdArgs (cmdArgsMode, cmdArgsRun, (&=), summary, help)
-- provide reflection needed for cmdArgs
import Data.Typeable
import Data.Data

main :: IO ()
main = do opts <- cmdArgsRun optsConfig
          print opts
          let t = transpose opts
          let asRoman = roman opts
          transposeStdin t asRoman

-- types for command-line args
data Prog = Prog { transpose :: Int, simplify :: Bool, roman :: Bool } deriving (Data, Typeable, Show)

-- set up cmd-line arg parsing, defaults, help
optsConfig = cmdArgsMode $ Prog 
    { transpose = 3     &= help "Set number of semitones by which to transpose the chords."
    , simplify  = False &= help "If set, will transpose to the set of chords having fewest sharps or flats." 
    , roman  = False &= help "If set, will output chords in roman numeral notation, guessing key."
    }
  &= summary ("Chord Sheet Transposer version "++ version)

version :: String
version = "0.0.1"
