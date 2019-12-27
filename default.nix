{ mkDerivation, base, composition, conduit, data-ordlist, digits
, hashable, hpack, lens, mtl, stdenv, tasty, tasty-discover
, tasty-hunit, transformers, trifecta, unordered-containers, vector
}:
mkDerivation {
  pname = "aoc";
  version = "2019.0.12.0";
  src = ./.;
  libraryHaskellDepends = [
    base composition conduit data-ordlist digits hashable lens mtl
    transformers trifecta unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base conduit digits tasty tasty-discover tasty-hunit vector
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/yurrriq/aoc19#readme";
  description = "My Haskell solutions to Advent of Code problems";
  license = stdenv.lib.licenses.mit;
  maintainers = with stdenv.lib.maintainers; [ yurrriq ];
}
