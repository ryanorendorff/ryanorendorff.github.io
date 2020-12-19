let
  sources = import ./sources.nix { };
  pkgs = import sources.nixpkgs { };
in pkgs.stdenv.mkDerivation {
  name = "site-src";
  src = [
    ../src/site.hs
    ../src/ryanorendorff-website.cabal
  ];

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir $out

    for srcFile in $src; do
      cp $srcFile $out/$(stripHash $srcFile)
    done
  '';
}
