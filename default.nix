{
  config ? {},
  nixpkgs ? import ((import <nixpkgs> config).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channel";
    rev = "502845c3e31ef3de0e424f3fcb09217df2ce6df6";
    sha256 = "0fcqpsy6y7dgn0y0wgpa56gsg0b0p8avlpjrd79fp4mp9bl18nda";
  }) config,
  compiler ? "ghc8102"
}:
let
  inherit (nixpkgs) pkgs;
  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};
  drv = haskellPackages.callCabal2nix "shrinkmusic" ./. {};
in
  drv
