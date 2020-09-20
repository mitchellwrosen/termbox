# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.3.0] - 2020-09-20

### Added
- Add `Cells` and `Cursor` types
- Export `Termbox.Internal` module that roughly corresponds to the C library

### Changed
- Add a few arguments to the action provided to `run`
- Make `run` throw `InitError`s as IO exceptions
- Reset output mode to "normal" on shutdown to work around a small bug in termbox.c that retains the output mode across
  separate invocations of init/shutdown
- Change type of `set` to construct a `Cells` rather than an `IO ()`
- Change a few keys into pattern synonyms because they overlap

### Removed
- Remove the alt modifier field from `KeyEvent`
- Remove `setCursor`, `hideCursor`, `clear`, `flush`, `getCells`, `getSize`, `poll`, `run_`
- Remove `InputMode`, `MouseMode`, and `OutputMode`, providing sane defaults instead
- Remove build dependency on `c2hs`
- Remove support for GHC < 8.2

## [0.2.0.1] - 2020-06-27

### Changed
- Bump `base` upper bound

## [0.2.0] - 2019-06-21

### Added
- Add `getCells` function
- Add `run` function

### Changed
- Rename `size` to `getSize`
- Rename `main` to `run_` and return errors as an `Either` instead of throwing.
- Make `Attr`'s `Semigroup` instance right-biased instead of left-biased.
- Make `Attr`'s `Num` instance total.

### Removed
- Add `buffer` function

## [0.1.0] - 2018-07-18

### Added
- Initial release
