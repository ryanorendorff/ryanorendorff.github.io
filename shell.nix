let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  niv = (import sources.niv { }).niv;
  site = import ./nix/site.nix;
in pkgs.mkShell {
  inputsFrom = [ site.env ];
  buildInputs = [ pkgs.nixfmt ];
}
