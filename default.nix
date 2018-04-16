with rec {
  pkgs = import ./nix/nixpkgs.nix;
  drv = pkgs.haskellPackages.eqsat;
};

if pkgs.lib.inNixShell then drv.env else drv
