# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added
- Add `defaultInputMode`, `defaultOutputMode`
- Export `Termbox.Internal` module that (roughly) contains 1:1 C bindings

### Changed
- Remove `HasCallStack` constraint from `getInputMode` and `getOutputMode`
- Add `InputMode` and `OutputMode` arguments to `run`
- Reset output mode to "normal" on shutdown to work around a small bug in termbox.c that retains the output mode across
  separate invocations of init/shutdown

### Removed
- Remove `getInputMode`, `setInputMode`, `getOutputMode`, `setOutputMode`
- Remove build dependency on `c2hs`

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
