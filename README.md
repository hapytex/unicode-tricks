# unicode-tricks

[![Build Status of the package by Travis](https://travis-ci.com/hapytex/unicode-tricks.svg?branch=master)](https://travis-ci.com/hapytex/unicode-tricks)
[![Build Status of the package by Hackage](https://matrix.hackage.haskell.org/api/v2/packages/unicode-tricks/badge)](https://matrix.hackage.haskell.org/#/package/unicode-tricks)
[![Hackage version badge](https://img.shields.io/hackage/v/unicode-tricks.svg)](https://hackage.haskell.org/package/unicode-tricks)

Based on the [**`unicode`** package](https://hackage.haskell.org/package/unicode) by *Henning Thielemann*.

This library aims to provide functionality to make working with certain blocks of
unicode characters more effective.

Currently the package contains the following modules:

 - **`Data.Char.Core`**: a module that defines common data structures;
 - **`Data.Char.Block`**: rendering a 2-by-2 block by a matrix of `Bool`s;
 - **`Data.Char.Braille`**: a module to render Braille characters with six or
   eight dot cells;
 - **`Data.Char.Card`**: a module to work with playing cards;
 - **`Data.Char.Chess`**: a module to render chess pieces (and some variants) in
   unicode;
 - **`Data.Char.Dice`**: a module to render die unicode characters;
 - **`Data.Char.Domino`**: a module to render domino unicode characters;
 - **`Data.Char.Frame`**: typesetting frame elements. Lines of the frames can be
   `Light` or `Heavy`, and there are additional options to use arcs for
   corners;
 - **`Data.Char.Roman`**: a module to render Roman numerals; and
 - **`Data.Char.Small`**: making use of subscript and superscript in unicode, and
   for example formatting `Ratio` objects.

## `unicode-tricks` is *safe* Haskell

The modules are marked with the `{-# LANGUAGE Safe #-}` pragma, it thus provides
guarantees, for example about not using [`unsafePerformIO`](https://begriffs.com/posts/2015-05-24-safe-haskell.html).

## Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/unicode-tricks).

You can contact the package maintainer by sending a mail to
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

