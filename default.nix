{ mkDerivation, base, conduit, conduit-extra, containers, lens
, lifted-base, machines, machines-io, pointed, pretty-show, stdenv
, stm, text, transformers
}:
mkDerivation {
  pname = "zurihac-machines";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base conduit conduit-extra containers lens lifted-base machines
    machines-io pointed pretty-show stm text transformers
  ];
  description = "Playing with Haskell Machines";
  license = stdenv.lib.licenses.bsd3;
}
