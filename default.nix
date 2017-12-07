{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "matrix-ical-bot" ./. {}
