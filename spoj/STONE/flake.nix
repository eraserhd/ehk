{
  description = "TODO: fill me in";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        testapp = pkgs.callPackage ./derivation.nix {};
      in {
        packages = {
          default = testapp;
          inherit testapp;
        };
        checks = {
          test = pkgs.runCommandNoCC "testapp-test" {} ''
            mkdir -p $out
            : ${testapp}
          '';
        };
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            elixir
            inotify-tools
          ];

        };
    })) // {
      overlays.default = final: prev: {
        testapp = prev.callPackage ./derivation.nix {};
      };
    };
}
