{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  iCalendarDevelop = (haskellPackages.callPackage (import ./icalendar.nix) {});

  f = { mkDerivation, base, bifunctors, containers, data-default
      , directory, filepath, fsnotify, lens, lucid
      , monad-loops
      , optparse-applicative, stdenv, text, text-format, thyme, time, tz
      , vector-space, test-framework-th, test-framework-hunit
      }:
      mkDerivation {
        pname = "matrix-ical";
        version = "0.3";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        testHaskellDepends = [ test-framework-th test-framework-hunit ];
        libraryHaskellDepends = [
          base bifunctors containers data-default directory filepath fsnotify monad-loops
          iCalendarDevelop lens lucid text text-format thyme time tz vector-space
        ];
        executableHaskellDepends = [
          base filepath fsnotify lens optparse-applicative text text-format
          thyme tz vector-space
        ];
        homepage = "https://github.com/plapadoo/matrix-ical#readme";
        description = "Bot that listens for ical file system changes and posts them to matrix rooms";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
