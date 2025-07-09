# AoC2015

Advent of Code 2015 - my solutions

http://adventofcode.com/2015

# How to run

## Set up compiler

I've had some issues with using OS-level `ghc`, so I've switched to using an indepenent install.
Set it up with:
- Install `ghcup`
- `ghcup install ghc recommended`
- `ghcup set ghc recommended`
- `ghcup install cabal latest`
- `ghcup set cabal latest`
- Make sure it's actually set up in `$PATH`

## Run the code

Run `cabal v2-run Day<X>-<Y>` where `<X>` is day number, and `<Y>` is task number.
