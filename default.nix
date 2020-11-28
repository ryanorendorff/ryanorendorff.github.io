let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  src = import ./nix/src.nix;
  site = import ./nix/site.nix;
in pkgs.stdenv.mkDerivation {
  name = "ryanorendorff.github.io";
  version = "1.0";

  inherit src;

  # From https://github.com/rpearce/hakyll-nix-template/blob/main/default.nix
  # Haskell code often fails to compile when the encoding is not set to
  # UFT-8. 
  LOCALE_ARCHIVE = pkgs.lib.optionalString (pkgs.buildPlatform.libc == "glibc")
    "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LANG = "en_US.UTF-8";

  buildInputs = [ site ];

  buildPhase = ''
    site build
  '';

  doCheck = true;
  checkPhase = ''
    site check --internal-links
  '';

  installPhase = ''
    mkdir $out;
    cp -r _site/* $out/
    cp CNAME $out/
  '';
}
