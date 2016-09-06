{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base64-bytestring, bytestring
      , directory, entropy, optparse-applicative, shelly, stdenv
      , system-filepath, text, time, unix
      }:
      mkDerivation {
        pname = "dhess-ssh-keygen";
        version = "1.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base base64-bytestring bytestring directory entropy
          optparse-applicative shelly system-filepath text time unix
        ];
        homepage = "https://github.com/dhess/dhess-ssh-keygen";
        description = "Enforce good ssh-keygen hygiene";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
