{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

let R = pkgs.R.override { enableStrictBarrier = true; };
in
haskell.lib.buildStackProject {
  name = "eventbot";
  buildInputs = [ zlib.dev zlib.out ];
  inherit ghc;
}
