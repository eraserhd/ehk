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
        dotnetPkgs =
          (with pkgs.dotnetCorePackages; combinePackages [
            sdk_8_0
          ]);

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
            zlib
            zlib.dev
            openssl
            dotnetPkgs
            dotnet-sdk
          ];
        };
    })) // {
      overlays.default = final: prev: {
        testapp = prev.callPackage ./derivation.nix {};
      };
    };
}
