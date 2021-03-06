{ mkDerivation, base, binary, bytestring, ghc-prim
, lens-family-core, pipes, pipes-bytestring, pipes-parse
, smallcheck, stdenv, tasty, tasty-hunit, tasty-smallcheck
, transformers
}:
mkDerivation {
  pname = "pipes-binary";
  version = "0.4.1";
  sha256 = "0dyng5pvyjw7bjflzfbrkwq99qzyc4a2rh0nlidjhy95ixs7jvhv";
  libraryHaskellDepends = [
    base binary bytestring ghc-prim pipes pipes-bytestring pipes-parse
    transformers
  ];
  testHaskellDepends = [
    base binary bytestring ghc-prim lens-family-core pipes pipes-parse
    smallcheck tasty tasty-hunit tasty-smallcheck transformers
  ];
  doCheck = false;
  homepage = "https://github.com/k0001/pipes-binary";
  description = "Encode and decode binary streams using the pipes and binary libraries";
  license = stdenv.lib.licenses.bsd3;
}
