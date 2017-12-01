{ mkDerivation, base, containers, data-default, dhall, directory
, fsnotify, iCalendar, lucid, matrix-bot-api, stdenv, text
, text-format, time
}:
mkDerivation {
  pname = "matrix-ical-bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers data-default dhall directory fsnotify iCalendar
    lucid matrix-bot-api text text-format time
  ];
  doHaddock = false;
  homepage = "https://github.com/plapadoo/matrix-ical-bot#readme";
  description = "Bot that listens for ical file system changes and posts them to matrix rooms";
  license = stdenv.lib.licenses.bsd3;
}
