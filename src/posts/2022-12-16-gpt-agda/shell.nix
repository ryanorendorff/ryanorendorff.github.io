let
  # Pin to nixos-25.11 (specific commit for reproducibility)
  pkgs = import (fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/89dbf01df72eb5ebe3b24a86334b12c27d68016a.tar.gz";
    sha256 = "0flvkjsk87211wi8zxrs4rqapwc8sar9xlsjlzlawmf46l9jqdmp";
  }) { };
in pkgs.mkShell {
  name = "agda-env";
  buildInputs = [ (pkgs.agda.withPackages (p: with p; [ standard-library ])) ];
}
