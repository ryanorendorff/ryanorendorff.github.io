let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  site = import ./nix/site.nix;
in pkgs.mkShell { inputsFrom = [ site.env ]; }
