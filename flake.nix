{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    inherit (pkgs) lib;
  in {
    devShells.${system}.default = pkgs.mkShell {
      packages = lib.attrValues {
        inherit (pkgs)
          cargo rustc
          clippy cargo-edit cargo-tarpaulin
          cargo-criterion gnuplot;
      };
    };
  };
}
