# unicode-tricks

[![Build Status of the package by GitHub actions](https://github.com/hapytex/unicode-tricks/actions/workflows/build-ci.yml/badge.svg)](https://github.com/hapytex/unicode-tricks/actions/workflows/build-ci.yml)
[![Build Status of the package by Hackage](https://matrix.hackage.haskell.org/api/v2/packages/unicode-tricks/badge)](https://matrix.hackage.haskell.org/#/package/unicode-tricks)
[![Hackage version badge](https://img.shields.io/hackage/v/unicode-tricks.svg)](https://hackage.haskell.org/package/unicode-tricks)

Based on the [**`unicode`** package](https://hackage.haskell.org/package/unicode) by *Henning Thielemann*.

This library aims to provide functionality to make working with certain blocks of
unicode characters more effective.

Currently the package contains the following modules:

 - **`Data.Char.Core`**: a module that defines common data structures;
 - **`Data.Char.BallotBox`**: rendering boxes that are empty, with a checkmark, or a cross;
 - **`Data.Char.Block`**: rendering a 2-by-2 block by a matrix of `Bool`s;
 - **`Data.Char.Brackets`**: determine if a character is a bracket, and find the corresponding bracket;
 - **`Data.Char.Braille`**: a module to render Braille characters with six or eight dot cells;
 - **`Data.Char.Card`**: a module to work with playing cards;
 - **`Data.Char.Chess`**: a module to render chess pieces (and some variants) in unicode;
 - **`Data.Char.Control`**: a module that visualizes *control characters*;
 - **`Data.Char.Combining`**: combine a character with a (sequence of) *combining* characters (diacritics, geometrical shapes, etc.);
 - **`Data.Char.Currency`**: translate currencies to Unicode characters and vice versa;
 - **`Data.Char.Dice`**: a module to render die unicode characters;
 - **`Data.Char.Domino`**: a module to render domino unicode characters;
 - **`Data.Char.Egyptian`**: a module that defines pattern synonyms for ancient Egyptian hieroglyphs;
 - **`Data.Char.Emoji.*`**: modules that provide functions to render *emoji* characters (flags, objects, persons, etc.);
 - **`Data.Char.Enclosed`**: functions to convert alphanumerical characters to characters where these are enclosed by circles, squares, parenthesis, etc.;
 - **`Data.Char.Frame`**: typesetting frame elements. Lines of the frames can be `Light` or `Heavy`, and there are additional options to use arcs for corners;
 - **`Data.Char.Math.*`**: a set of modules to render mathematical symbols and text, for example in *Fraktur*, *Blackboard bold*, etc.
 - **`Data.Char.Number.Duodecimal`**: a module to work with [duodecimal numbers](https://en.wikipedia.org/wiki/Duodecimal);
 - **`Data.Char.Number.Egyptian`**: a module to render [ancient Egyptian numerals](https://en.wikipedia.org/wiki/Egyptian_numerals);
 - **`Data.Char.Number.Mayan`**: a module to render [Mayan numerals](https://en.wikipedia.org/wiki/Maya_numerals);
 - **`Data.Char.Number.Roman`**: a module to render [Roman numerals](https://en.wikipedia.org/wiki/Roman_numerals);
 - **`Data.Char.Number.Segmented`**: a module to render numbers on a [seven-segment display](https://en.wikipedia.org/wiki/Seven-segment_display);
 - **`Data.Char.Number.VulgarFraction`**: a module to use *vulgar fractions* defined in the Unicode standard;
 - **`Data.Char.Private`**: characters belonging to the *private usage area*;
 - **`Data.Char.Private.Klingon`**: Klingon script characters;
 - **`Data.Char.Small`**: making use of subscript and superscript in unicode, and for example formatting `Ratio` objects; and
 - **`Data.Char.Tag`**: a unicode block that is used as emoji modifiers for regional flags.

## `unicode-tricks` is *safe* Haskell

The modules are marked with the `{-# LANGUAGE Safe #-}` pragma, it thus provides
guarantees, for example about not using [`unsafePerformIO`](https://begriffs.com/posts/2015-05-24-safe-haskell.html).

## Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/unicode-tricks).

You can contact the package maintainer by sending a mail to
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

Contributors:

 - [`@wismill`](https://github.com/wismill) found a bug and added tests for the
   `Data.Char.Math` as well as the `Data.Char.Number.Segmented` module.
