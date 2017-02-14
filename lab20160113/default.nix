{ mkDerivation, base, binary, binary-bits, binary-parsers
, bytestring, optparse-applicative, pipes, pipes-binary
, pipes-bytestring, pipes-parse, stdenv
}:
mkDerivation {
  pname = "lab20160113";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary binary-bits binary-parsers bytestring
    optparse-applicative pipes pipes-binary pipes-bytestring
    pipes-parse
  ];
  license = stdenv.lib.licenses.bsd3;
}
