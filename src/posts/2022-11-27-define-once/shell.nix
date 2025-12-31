let
  # Pin to nixos-25.11 (specific commit for reproducibility)
  pkgs = import (fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/89dbf01df72eb5ebe3b24a86334b12c27d68016a.tar.gz";
    sha256 = "0flvkjsk87211wi8zxrs4rqapwc8sar9xlsjlzlawmf46l9jqdmp";
  }) { };

  python = pkgs.python3.withPackages (p: with p; [ jupyterlab ]);

in python.env
