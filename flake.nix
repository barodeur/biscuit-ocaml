{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        inherit (pkgs.darwin.apple_sdk) frameworks;
        inherit (pkgs) lib;
      in
      {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = [ pkgs.opam ] ++ lib.optionals pkgs.stdenv.isDarwin [
              frameworks.CoreServices
              frameworks.Foundation
            ];

            nativeBuildInputs = [ pkgs.pkg-config ];

            shellHook = ''
              eval $(opam env)
            '';
          };
        };
      });
}

