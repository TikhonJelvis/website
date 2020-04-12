{ pkgs ? import <nixpkgs> {} }:

let
  haskellDevelopmentTools = with pkgs.haskellPackages;
    [ cabal-install stylish-haskell ];

  website = import ./. {};
in
pkgs.lib.overrideDerivation website.env (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ haskellDevelopmentTools;
})
