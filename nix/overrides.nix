{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  smallcheck-lens = doJailbreak super.smallcheck-lens;
  tasty-lens = doJailbreak super.tasty-lens;
  loc = doJailbreak super.loc;
  loc-test = doJailbreak super.loc-test;

  judy = super.judy.override { Judy = pkgs.judy; };

  disjoint-containers = dontCheck super.disjoint-containers;

  crackNum = super.crackNum_2_0;

  impure-containers = (
    appendPatch super.impure-containers ./patches/impure-containers.patch);

  sbv = (
    with rec {
      sbvSource = pkgs.fetchFromGitHub {
        owner  = "LeventErkok";
        repo   = "sbv";
        rev    = "4576ba8b91c945c7b9c82f5f6c9c171a85b178b9";
        sha256 = "1s1bag1zg3fg2agfjmniy1hhvxwigmgmbzyzqpnbprvlm4cr3i1p";
      };
      unpatchedSBV = self.callCabal2nix "sbv" sbvSource {};
    };
    dontCheck (appendPatch unpatchedSBV ./patches/sbv.patch));

  eqsat = (
    with rec {
      eqsatSource = pkgs.lib.cleanSource ../.;
      eqsatBasic = self.callCabal2nix "eqsat" eqsatSource {};
    };
    overrideCabal eqsatBasic (old: {
      preConfigure = (old.preConfigure or "") + ''
        echo "#define SMT_Z3_PATH \"${pkgs.z3}/bin/z3\"" \
             > library/EqSat/Internal/SBVDefines.hs
      '';
    }));
}
