{
  inputs = {
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
  };

  outputs = { self, nixpkgs, purescript-overlay }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ purescript-overlay.overlays.default ];
    };
  in {
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [
        pkgs.nodejs
        pkgs.purs
        pkgs.spago-unstable
        pkgs.purs-tidy-bin.purs-tidy-0_10_0
        pkgs.purescript-language-server
        pkgs.graphviz
      ];
    };
  };
}
