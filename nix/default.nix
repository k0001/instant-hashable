{ mkDerivation
, stdenv

  # haskell deps
, base
, hashable
, instant-generics
, tasty
, tasty-quickcheck
}:

mkDerivation {
  pname = "instant-hashable";
  version = "0.2";
  homepage = "https://github.com/k0001/instant-hashable";
  description = "Generic Hashable instances through instant-generics";
  license = stdenv.lib.licenses.bsd3;
  src = ../.;
  isLibrary = true;
  isExecutable = false;
  doHaddock = true;
  buildDepends = [base hashable instant-generics tasty tasty-quickcheck];
}
