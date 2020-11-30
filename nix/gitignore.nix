let
  sources = import ./sources.nix { };
  pkgs = import sources.nixpkgs { };
in (import sources.gitignore { inherit (pkgs) lib; }).gitignoreSource
