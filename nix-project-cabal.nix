{ mkDerivation, base, gitrev, optparse-applicative, process, stdenv
}:
mkDerivation {
  pname = "nix-project-cabal";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base gitrev optparse-applicative process
  ];
  license = stdenv.lib.licenses.bsd3;
}
