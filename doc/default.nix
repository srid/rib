{ pkgs ? import <nixpkgs> {}
# Cabal project root
, root ? ./. 
# Rib library source to use
, rib ? ../.
, ...
}:

import rib { inherit pkgs root; }
