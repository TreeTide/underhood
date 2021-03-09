{ mkDerivation, base, bytestring, HUnit, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, vector
}:
mkDerivation {
  pname = "text-offset";
  version = "0.1.0.0";
  sha256 = "002fd2d5926a1ba7b447f84871c42d9c25d8b7706cfba166c98a7c6b0b763456";
  libraryHaskellDepends = [ base text vector ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/google/haskell-indexer";
  description = "Library for converting between line/column and byte offset";
  license = stdenv.lib.licenses.asl20;
}
