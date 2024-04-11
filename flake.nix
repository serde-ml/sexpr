{
  description = "A little TUI framework for OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    minttea = {
      url = "github:leostera/minttea";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    serde = {
      url = "github:serde-ml/serde";
      inputs.minttea.follows = "minttea";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          inherit (pkgs) ocamlPackages mkShell;
          inherit (ocamlPackages) buildDunePackage;
          version = "0.0.1+dev";
        in
          {
            devShells = {
              default = mkShell {
                buildInputs = with ocamlPackages; [
                  dune_3
                  ocaml
                  utop
                  ocamlformat
                ];
                inputsFrom = [
                  self'.packages.default
                ];
                packages = builtins.attrValues {
                  inherit (ocamlPackages) ocaml-lsp ocamlformat-rpc-lib;
                };
              };
            };

            packages = {
              default = buildDunePackage {
                inherit version;
                pname = "serde_sexpr";
                propagatedBuildInputs = with ocamlPackages; [
                  qcheck
                  inputs'.serde.packages.default
                  inputs'.serde.packages.serde_derive
                  inputs'.minttea.packages.spices
                  ppx_inline_test
                  sedlex
                ];
                src = ./.;
              };
            };
          };
    };
}
