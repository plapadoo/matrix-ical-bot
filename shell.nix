{ pkgs ? import <nixpkgs> {} }:
let
  drv = pkgs.haskellPackages.callCabal2nix "matrix-ical" ./. {};
in
  drv.env
