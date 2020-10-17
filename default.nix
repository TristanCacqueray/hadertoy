{ nixpkgs ? import <nixpkgs> { } }:
let
  name = "hadertoy";
  drv = nixpkgs.haskellPackages.callCabal2nix name ./. { };
  shellDrv = nixpkgs.haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [ drv ];
    buildInputs = with nixpkgs.haskellPackages; [ hlint cabal-install ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drv
