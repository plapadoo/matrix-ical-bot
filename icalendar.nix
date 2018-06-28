{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, containers, criterion, data-default, deepseq
, heap, hspec, mime, mtl, network, network-uri, old-locale, parsec
, stdenv, text, time, fetchFromGitHub
}:
mkDerivation {
  pname = "iCalendar";
  version = "0.5.0.0";
  src = fetchFromGitHub {
      owner = "plapadoo";
      repo = "iCalendar";
      rev = "00c6ab510e7cb485068291be76273ae5dfc96fb";
      sha256 = "1r7zf851k23r3iffdgfsgqh3nnd6yz80xbqmg8lp7zcjjvhd5w0q";
    };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    containers data-default deepseq heap mime mtl network network-uri
    old-locale parsec text time
  ];
  testHaskellDepends = [
    base bytestring case-insensitive containers data-default hspec
    parsec text time
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion data-default deepseq mime
  ];
  homepage = "http://github.com/chrra/iCalendar";
  description = "iCalendar data types, parser, and printer";
  license = stdenv.lib.licenses.bsd3;
}
