{ mkDerivation, base, directory, optparse-applicative, stdenv
, system-filepath, text, time, turtle
}:
mkDerivation {
  pname = "dhess-ssh-keygen";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base directory optparse-applicative system-filepath text time
    turtle
  ];
  homepage = "https://github.com/dhess/dhess-ssh-keygen";
  description = "A simple script to enforce good ssh-keygen hygiene";
  license = stdenv.lib.licenses.bsd3;
}
