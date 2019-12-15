{
  config ? {},
  nixpkgs ? import ((import <nixpkgs> config).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channel";
    rev = "54f385241e6649128ba963c10314942d73245479";
    sha256 = "0bd4v8v4xcdbaiaa59yqprnc6dkb9jv12mb0h5xz7b51687ygh9l";
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
