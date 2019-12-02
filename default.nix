{ mkDerivation, base, hpack, stdenv, trifecta, vector }:
mkDerivation {
  pname = "aoc";
  version = "2019.0.2.0";
  src = ./.;
  libraryHaskellDepends = [ base trifecta vector ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  homepage = "https://github.com/yurrriq/aoc19#readme";
  description = "My Haskell solutions to Advent of Code problems";
  license = stdenv.lib.licenses.mit;
  maintainers = with stdenv.lib.maintainers; [ yurrriq ];
}
