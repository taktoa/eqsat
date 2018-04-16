{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  smallcheck-lens = doJailbreak super.smallcheck-lens;
  tasty-lens = doJailbreak super.tasty-lens;

  judy = (
    with { unpatchedJudy = super.judy.override { Judy = pkgs.judy; }; };
    appendPatch unpatchedJudy ./patches/judy.patch);

  disjoint-containers = dontCheck super.disjoint-containers;

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
    with { eqsatSource = pkgs.lib.cleanSource ../.; };
    self.callCabal2nix "eqsat" eqsatSource {});
}
