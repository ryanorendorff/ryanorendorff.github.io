# We do not use a flake here because really it is overkill for a python shell
let
  nixpkgs-source = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/695b3515251873e0a7e2021add4bba643c56cde3.tar.gz";
    sha256 = "sha256:0hhn6li42s8awkpnc375zlzh3zk8lfsvg50mb03iq88lywbaikjg";
  };

  pkgs = import nixpkgs-source {
    config.allowBroken = true;
    overlays = [
      (self: super: {
        python3 = super.python3.override {
          packageOverrides = python-self: python-super: {
            jupyter_server = python-super.jupyter_server.overrideAttrs
              (oldAttrs: {
                disabledTests = oldAttrs.disabledTests
                  ++ [ "test_authorized_requests" ];
              });
          };
        };
      })
    ];
  };

  python = pkgs.python3.withPackages (p: with p; [ jupyterlab ]);

in python.env
