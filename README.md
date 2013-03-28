A Chord Sheet Transposer / Nashvillizer
=======================================

This toy project parses informal chord sheets and outputs a modified sheet, either transposing the chords it 
finds, or presenting them in roman numeral form (or Nashville system).

An attempt is made to keep the replacement chord symbols in their original locations on the page despite their 
neighbouring symbols growing or shrinking in length ("E/B" might become "F#/C#" or "III/V"), 
and they will never collide or overwrite each other.

This is a quick hack by a haskell newbie and contains many bad practices.
I developed it in order to practice with the parsec parsing library.

Those looking for quality music analysis might start by looking at haskore:
http://www.haskell.org/haskellwiki/Haskore

Usage:
=======

To transpose some song up 3 semitones:

    runhaskell Main.hs --transpose=3 < song.txt

To rewrite absolute chords with relative roman numerals, use -r, and specify an original key with -k

    runhaskell Main.hs -r -k GSharp < song.txt

(If you omit -k, the original key will be poorly guessed based currently on the first encountered chord.)

To output sheet in an html pre (of class chordsheet) with successfully parsed chords tagged in spans of class pc, use --html:

    runhaskell Main.hs --html --transpose=0 < song.txt

For more help:

    runhaskell Main.hs --help
