with rec {
  fetchFromGitHub = (
    (import <nixpkgs> { config = {}; overlays = []; }).fetchFromGitHub);
  nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "a8c71037e041725d40fbf2f3047347b6833b1703";
    sha256 = "1z4cchcw7qgjhy0x6mnz7iqvpswc2nfjpdynxc54zpm66khfrjqw";
  };
};

import nixpkgs {
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskellPackages = super.haskell.packages.ghc843.override {
        overrides = import ./overrides.nix { pkgs = self; };
      };
    };
  };
  overlays = [];
}
