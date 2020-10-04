{ sources ? import nix/sources.nix {} }:

let
  pkgs = import sources.nixpkgs {};

  haskellDevelopmentTools = with pkgs.haskellPackages;
    [ cabal-install stylish-haskell ];

  website = import ./. { inherit sources; };
in
pkgs.lib.overrideDerivation website.env (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ haskellDevelopmentTools ++ [ pkgs.s3cmd ];

  # Nix's glibc has weird locale settings on Linux, which make Haskell
  # binaries unable to handle UTF-8 by default. This fixes that
  # problem.
  LOCALE_ARCHIVE =
    pkgs.lib.optionalString pkgs.buildPlatform.isLinux "${pkgs.buildPackages.glibcLocales}/lib/locale/locale-archive";
  LANG = "C.UTF-8";
})
