# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added
- Added `defaultInputMode`, `defaultOutputMode`

### Changed
- Removed `HasCallStack` constraint from `getInputMode` and `getOutputMode`
- Added `InputMode` and `OutputMode` arguments to `run`
- Reset output mode to "normal" on shutdown to work around a small bug in termbox.c that retains the output mode across
  separate invocations of init/shutdown

### Removed
- Removed `getInputMode`, `setInputMode`, `getOutputMode`, `setOutputMode`

## [0.2.0.1] - 2020-06-27

### Changed
- Bumped `base` upper bound

## [0.2.0] - 2019-06-21

### Added
- `getCells` function
- `run` function

### Changed
- Renamed `size` to `getSize`
- Renamed `main` to `run_` and return errors as an `Either` instead of throwing.
- Made `Attr`'s `Semigroup` instance right-biased instead of left-biased.
- Made `Attr`'s `Num` instance total.

### Removed
- `buffer` function

## [0.1.0] - 2018-07-18

### Added
- Initial release
