{ mkDerivation, base, conduit, data-ordlist, digits, hashable
, hpack, lens, mtl, stdenv, tasty, tasty-discover, tasty-hunit
, transformers, trifecta, unordered-containers, vector
}:
mkDerivation {
  pname = "aoc";
  version = "2019.0.9.0";
  src = ./.;
  libraryHaskellDepends = [
    base conduit data-ordlist digits hashable lens mtl transformers
    trifecta unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base conduit digits tasty tasty-discover tasty-hunit vector
  ];
  testToolDepends = [ tasty-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/yurrriq/aoc19#readme";
  description = "My Haskell solutions to Advent of Code problems";
  license = stdenv.lib.licenses.mit;
  maintainers = with stdenv.lib.maintainers; [ yurrriq ];
}
