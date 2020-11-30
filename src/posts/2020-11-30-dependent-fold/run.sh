#! /usr/bin/env nix-shell
#! nix-shell -E "let pkgs = import (fetchTarball { url = \"https://github.com/NixOS/nixpkgs/archive/3c72bb875e67ad1bce805fe26a8a5d3a0e8078ed.tar.gz\"; sha256 = \"111mrviz6ar72pjsrb2v26xsjy5wza4q0wprj4bd6yk9lcdrw41n\"; }) {}; in pkgs.mkShell { name = \"run\"; buildInputs = with pkgs; [ ghcid (pkgs.haskellPackages.ghcWithPackages (p: with p; [clash-lib clash-prelude]))];}" -i bash
ghcid -c 'ghci' -T main DependentFold.lhs
