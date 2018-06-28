let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      iCalendarDevelop = self.callPackage (import ./icalendar.nix) {};
    };
  };
  thisPackage = haskellPackages.callPackage (import ./ical-bot.nix) {};
in
  pkgs.dockerTools.buildImage {
    name = "matrix-ical-bot";
    tag = "latest";
    contents = pkgs.haskell.lib.justStaticExecutables thisPackage;
  }
