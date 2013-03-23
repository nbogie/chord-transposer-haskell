This toy project parses informal chord sheets and outputs a modified sheet, either transposing the chords it 
finds, or presenting them in roman numeral form (or Nashville system).

This is a quick hack by a haskell newbie and contains many bad practices.
I developed it in order to practice with the parsec parsing library.

Those looking for quality music analysis might start by looking at haskore:
http://www.haskell.org/haskellwiki/Haskore

Usage:
=======

To transpose some song up 3 semitones:

    runhaskell Main.hs --transpose=3 < song.txt

To rewrite absolute chords with relative roman numerals

    runhaskell Main.hs -r < song.txt

For more help:

    runhaskell Main.hs --help