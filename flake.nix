{
  description = "Ryan Orendorff's personal website";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, gitignore }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];

      imports = [ haskell-flake.flakeModule ];

      perSystem = { self', config, pkgs, lib, system, ... }:
        let
          gitignoreSource = gitignore.lib.gitignoreSource;
          websiteSrc = gitignoreSource ./src;
        in {
          # Haskell project configuration
          haskellProjects.default = {
            projectRoot = ./src;
            devShell = {
              tools = hp: {
                ghcid = hp.ghcid;
                cabal-install = hp.cabal-install;
                fourmolu = hp.fourmolu;
                hlint = hp.hlint;
              };
            };
            autoWire = [ "packages" "devShells" ];
          };

          # Website build derivation
          packages.default = pkgs.stdenv.mkDerivation {
            pname = "ryanorendorff.github.io";
            version = self.shortRev or self.dirtyShortRev or "dev";
            src = websiteSrc;

            LOCALE_ARCHIVE = lib.optionalString
              (pkgs.stdenv.buildPlatform.libc == "glibc")
              "${pkgs.glibcLocales}/lib/locale/locale-archive";
            LANG = "en_US.UTF-8";

            buildInputs = [ self'.packages.ryanorendorff-website ];

            buildPhase = ''
              site build
            '';

            doCheck = true;
            checkPhase = ''
              site check --internal-links
            '';

            installPhase = ''
              mkdir $out
              cp -r _site/* $out/
              cp CNAME $out/
            '';

            meta = {
              description = "Ryan Orendorff's personal website";
              homepage = "https://ryanorendorff.github.io";
              license = lib.licenses.mit;
              maintainers = [ pkgs.lib.maintainers.ryanorendorff ];
            };
          };

          # Convenience apps for local development
          # Run from anywhere in the repo: nix run .#watch
          apps =
            let
              # Helper to create a script that runs site commands from src/
              siteScript = name: commands: {
                type = "app";
                program = "${pkgs.writeShellScript name ''
                  set -e
                  cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)/src"
                  ${commands}
                ''}";
              };
              site = "${self'.packages.ryanorendorff-website}/bin/site";
            in {
              build = siteScript "build" ''${site} build "$@"'';
              watch = siteScript "watch" ''
                ${site} clean
                ${site} watch "$@"
              '';
              clean = siteScript "clean" ''${site} clean "$@"'';
            };
        };
    };
}
