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
        overrides = import ./overrides.nix { pkgs = super; };
      };
    };
  };
  overlays = [];
}
