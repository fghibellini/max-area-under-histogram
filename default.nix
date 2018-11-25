{ 
  nixpkgs ? (import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev= "b568a7c673f3c4dc7b3264655c16bab788ff6193";
    sha256 = "00i10x72yc310srgsk91yd86c2g0yjsw4kidql8j1imdf7qasw0f";
  }) {}),
  compiler ? "default"
}:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, QuickCheck, hspec, filepath, cabal-install, MonadRandom, deepseq
      , stdenv
      }:
      mkDerivation {
        pname = "ghibes-blog";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers hspec QuickCheck cabal-install MonadRandom deepseq
        ];
        homepage = "http://fghibellini.com";
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
