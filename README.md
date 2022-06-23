# README

A simple command-line program that spells numbers from 0 to 1000.

## Running the command line program

- Install the [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
  building tool. I chose it among other things because it supports LTS (Long-Term Support)
  releases of libraries known to be working together.
- Open a terminal and run `stack install`. This will install an exectable under `~/.local/bin`.
- Type `signal` to use the translator.

## Running tests

Type `stack test` on a terminal.

## Other considerations

All the code is in the `Translate.hs` module as it's small enough. Within that module there
are clearly identified sections that can be refactored into their own modules.
