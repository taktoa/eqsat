with rec {
  fetchFromGitHub = (
    (import <nixpkgs> { config = {}; overlays = []; }).fetchFromGitHub);
  nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "12512f25e9ecdf6e8fc97a80297a4d2fa89e2f99";
    sha256 = "1g0wn93szifqyvd6n131j12m9vjw6lbpy2hms18sv1imw2lxsza1";
  };
};

import nixpkgs {
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskellPackages = super.haskellPackages.override {
        overrides = import ./overrides.nix { pkgs = self; };
      };

      z3 = (super.z3.override { python2 = super.python; }).overrideAttrs (old: rec {
        name = "z3-${version}";
        version = "4.6.0";
        src = super.fetchFromGitHub {
          owner  = "Z3Prover";
          repo   = "z3";
          rev    = "b0aaa4c6d7a739eb5e8e56a73e0486df46483222";
          sha256 = "1cgwlmjdbf4rsv2rriqi2sdpz9qxihxrcpm6a4s37ijy437xg78l";
        };
        propagatedBuildInputs = [ super.python.pkgs.setuptools ];
      });
    };
  };
  overlays = [];
}
