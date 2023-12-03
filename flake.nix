{
  inputs = {
    systems.url = "github:nix-systems/default";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };
 
  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = import inputs.systems;
    imports = [
      inputs.haskell-flake.flakeModule
    ];
    perSystem = { self', system, lib, config, pkgs, ... }: {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.rust-overlay.overlays.default
        ];
      };
      haskellProjects.default = {
        devShell.hlsCheck.enable = false;
        autoWire = [];
      };
      packages.default = config.haskellProjects.default.outputs.packages.techtonica.package;
      devShells.default = pkgs.mkShell {
        buildInputs = [
          (pkgs.rust-bin.stable.latest.default.override {
            extensions = ["rust-analyzer-preview"];
          })
        ];
        inputsFrom = [
          config.haskellProjects.default.outputs.devShell
        ];
      };
    };
  };
}

