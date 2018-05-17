with rec {
  fetchFromGitHub = (
    (import <nixpkgs> { config = {}; overlays = []; }).fetchFromGitHub);
  nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "06c576b0525da85f2de86b3c13bb796d6a0c20f6";
    sha256 = "01cra89drfjf3yhii5na0j5ivap2wcs0h8i0xcxrjs946nk4pp5j";
  };
};

import nixpkgs {
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskellPackages = super.haskellPackages.override {
        overrides = import ./overrides.nix { pkgs = self; };
      };
    };
  };
  overlays = [];
}
