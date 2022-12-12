let
  pkgs = import (builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/8d6da33644bbd082b8a13f73ea2c7338190d7429.tar.gz";
    sha256 = "sha256:0ynbn807jcrpwy9d0lbccvkbpgagincpwrr5qqkan8wi7ywap680";
  }) { };
in pkgs.mkShell {
  name = "agda-env";
  buildInputs = [ (pkgs.agda.withPackages (p: with p; [ standard-library ])) ];
}
