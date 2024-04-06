{
  config ? {},
  pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "057f9aecfb71c4437d2b27d3323df7f93c010b7e";
    sha256 = "MxCVrXY6v4QmfTwIysjjaX0XUhqBbxTWWB4HXtDYsdk=";
  }) config,
  shell ? false
}:
let
  hp = pkgs.haskell.packages.ghc963;
  lib = pkgs.haskell.lib;
in
  hp.developPackage {
    root = ./.;
    returnShellEnv = shell;
    withHoogle = false;
    modifier = drv:
      lib.overrideCabal drv (attrs: {
        buildTools = (attrs.buildTools or [ ]) ++ [
          hp.cabal-install
          hp.haskell-language-server
        ];
      });
  }
