# `eqsat`

An implementation of [equality saturation][] in Haskell.

[equality saturation]: http://www.cs.cornell.edu/~ross/publications/eqsat/

## Compiling

If you have [Nix](https://nixos.org/nix/), you can easily build this project by
running `nix-build ./default.nix --no-out-link -Q`. This will eventually print
out a store path that looks like `/nix/store/<hash>-eqsat-0.1.0`, which contains
the libraries and executables.

Note that this will only work with Z3 version 4.6.0 or higher.
