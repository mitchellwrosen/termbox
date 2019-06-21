# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

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
