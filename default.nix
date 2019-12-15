{ mkDerivation, base, conduit, data-ordlist, digits, hashable
, hpack, lens, mtl, stdenv, transformers, trifecta
, unordered-containers, vector
}:
mkDerivation {
  pname = "aoc";
  version = "2019.0.8.1";
  src = ./.;
  libraryHaskellDepends = [
    base conduit data-ordlist digits hashable lens mtl transformers
    trifecta unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/yurrriq/aoc19#readme";
  description = "My Haskell solutions to Advent of Code problems";
  license = stdenv.lib.licenses.mit;
  maintainers = with stdenv.lib.maintainers; [ yurrriq ];
}
