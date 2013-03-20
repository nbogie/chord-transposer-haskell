{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Transposer hiding (main)

import System.Console.CmdArgs (cmdArgs, cmdArgsMode, cmdArgsRun, (&=), summary, help)
-- provide reflection needed for cmdArgs
import Data.Typeable
import Data.Data

main = do opts <- cmdArgsRun optsConfig
          let t = transpose opts
          transposeStdin t

-- types for command-line args
data Prog = Prog { transpose :: Int, simplify :: Bool } deriving (Data, Typeable, Show)

-- set up cmd-line arg parsing, defaults, help
optsConfig = cmdArgsMode $ Prog { 
    transpose = 3     &= help "Set number of semitones by which to transpose the chords.",
    simplify  = False &= help "If set, will transpose to the set of chords having fewest sharps or flats." 
    }
  &= summary ("Chord Sheet Transposer version "++ version)

version = "0.0.1"
