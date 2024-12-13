let
  pkgs = import <nixpkgs> {};
  haskell = pkgs.haskellPackages.ghcWithPackages
    (ps: with ps; [ containers random ]);
in pkgs.mkShell {
  packages = [ haskell ];
}
