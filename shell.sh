#!/bin/sh

nix-shell -I nixpkgs=channel:nixos-22.11 -p "haskellPackages.ghcWithPackages(p: with p; [cabal-install haskell-language-server aeson megaparsec time extra regex-tdfa raw-strings-qq text-show universum composition cond containers data-foldapp edit-distance extra generic-lens keys lens matrices megaparsec parsec parsec-numbers parser-combinators profunctors recursion-schemes split search-algorithms text TypeCompose universum vector data-dword list-predicate primes arithmoi])"
