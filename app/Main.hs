{-# LANGUAGE DeriveDataTypeable #-}

-- I don't know how to specify the complex type sig for cmdArgs' optsConfig without
-- making it brittle, so for this module only...
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} 

module Main where
import Transposer hiding (main)
import Notes
import System.Console.CmdArgs (cmdArgsMode, cmdArgsRun, (&=), summary, help)
import Data.Maybe (listToMaybe)
-- provide reflection needed for cmdArgs
import Data.Typeable
import Data.Data

main :: IO ()
main = do opts <- cmdArgsRun optsConfig
          -- print opts
          let t = transpose opts
          let fmt = if html opts then TaggedText else PlainText
          let asRoman = roman opts
          let keyM = fmap (addSnd MajorScale) $ keyFromStr (key opts) 
          transposeStdin fmt t asRoman keyM
  where addSnd b a = (a, b)

keyFromStr :: String -> Maybe Note
keyFromStr = maybeRead

-- taken from Network.CGI.Protocol src
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- types for command-line args
data Prog = Prog { transpose :: Int, html :: Bool, key :: String, roman :: Bool } deriving (Data, Typeable, Show)

-- set up cmd-line arg parsing, defaults, help
optsConfig = cmdArgsMode $ Prog 
    { transpose = 3     &= help "Set number of semitones by which to transpose the chords."
    -- , simplify  = False &= help "If set, will transpose to the set of chords having fewest sharps or flats." 
    , html      = False &= help "If set, will output text as html with the chords marked up."
    , key       = ""    &= help "The original key. If set, used when romanizing the output chords.  Guessed otherwise."
    , roman     = False &= help "If set, will output chords in roman numeral notation, guessing key."
    }
  &= summary ("Chord Sheet Transposer version " ++ version)

version :: String
version = "0.1.0.0"
