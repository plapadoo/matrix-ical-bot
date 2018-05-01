# matrix-ical

This bot listens for changes in a directory containing ical files and outputs messages on standard output that can be fed to the [matrix-bot](http://github.com/plapadoo/matrix-bot).

## Installation

### Via Docker

Coming soon

### Manually

Assuming you have compiled the bot yourself, you’re left with a single executable file:  `matrix-ical-bot`. The application takes a single command line parameter `--directory` specifying the ical directory to watch.

## Compilation from source

### Using Nix

The easiest way to compile the bot or the docker image from source is to use the [nix package manager](https://nixos.org/nix/). With it, you can build the bot using

    nix-build

The resulting files will be located in the `result/` directory. To build the Docker image, use

    nix-build dockerimage.nix

This will, at the last line, output a path that you can feed into `docker load`.

### Using cabal

The bot can be compiled using [cabal-install](https://www.haskell.org/cabal/) by using `cabal install --only-dependencies` and then `cabal install`.
