let
  sources = import ./sources.nix { };
  pkgs = import sources.nixpkgs { };
  gitignoreSource = import ./gitignore.nix;
  site-src = import ./site-src.nix;
in pkgs.haskellPackages.callCabal2nix "site" site-src { }
