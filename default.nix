{ mkDerivation, base, directory, optparse-applicative, shelly
, stdenv, system-filepath, text, time
}:
mkDerivation {
  pname = "dhess-ssh-keygen";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base directory optparse-applicative shelly system-filepath text
    time
  ];
  homepage = "https://github.com/dhess/dhess-ssh-keygen";
  description = "A simple script to enforce good ssh-keygen hygiene";
  license = stdenv.lib.licenses.bsd3;
}
