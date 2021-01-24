# default.nix
{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    denote = ./.;
  };

  shells = {
    ghc = ["denote"];
  };
})