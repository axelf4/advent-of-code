{ pkgs ? import <nixpkgs> {} }: pkgs.mkShell {
  buildInputs = with pkgs; let
    haskell = haskellPackages.ghcWithPackages
      (ps: [ ps.regex-pcre ]);
  in [ haskell ];
}
