{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackagesAug = haskellPackages.override {
    overrides = self: super: {
      iCalendarDevelop = self.callPackage (import ./icalendar.nix) {};
    };
  };

  f = import ./ical-bot.nix;

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackagesAug.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
