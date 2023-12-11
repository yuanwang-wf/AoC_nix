{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";
    check-flake.url = "github:srid/check-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
        inputs.check-flake.flakeModule
      ];
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          packages = {
            # You can add more than one local package here.
            aoc.root = ./.; # Assumes ./my-package.cabal
          };
          buildTools = hp: {
            inherit (pkgs)
              treefmt;
            inherit (hp)
              implicit-hie;
          } // config.treefmt.formatters;
          # overrides = self: super: { };
          hlintCheck.enable = true;
          hlsCheck.enable = true;
        };
        treefmt.formatters = {
          inherit (pkgs)
            nixpkgs-fmt;
          inherit (pkgs.haskellPackages)
            cabal-fmt
            fourmolu;
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.aoc;
      };
    };
}
