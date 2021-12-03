{ pkgs ? import <nixpkgs> {} }: pkgs.mkShell {
  buildInputs = with pkgs; let
    haskell = haskellPackages.ghcWithPackages (ps: with ps; [
      sbv
    ]);
  in [ haskell z3 ];
}
