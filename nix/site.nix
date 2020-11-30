let
  sources = import ./sources.nix { };
  pkgs = import sources.nixpkgs { };
  gitignoreSource = import ./gitignore.nix;
  src = import ./src.nix;
in pkgs.haskellPackages.callCabal2nix "site" src { }
