{
  config ? {},
  nixpkgs ? import ((import <nixpkgs> config).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channel";
    rev = "6f3bd5db2f4d1f8bf21631ebe56f496114637c56";
    sha256 = "1izdsqiyxb1bn9a4xl06hh14xhcc2rhcqcqfxwpf4q5i1q32bw5d";
  }) config,
  compiler ? "default"
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
