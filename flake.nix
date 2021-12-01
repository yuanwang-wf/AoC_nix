{
  description = "virtual environments";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    # Use the same nixpkgs
    inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, devshell, flake-utils, gitignore }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            devshell.overlay
            (final: prev: {
              haskellPackages = prev.haskellPackages.override {
                overrides = hself: hsuper: {
                  aoc = hself.callCabal2nix "aoc"
                    (gitignore.lib.gitignoreSource ./.) { };
                };
              };
              aoc = final.haskell.lib.justStaticExecutables
                final.haskellPackages.aoc;
            })
          ];
        };
        myHaskellEnv = (pkgs.haskellPackages.ghcWithHoogle (p:
          with p; [
            aoc
            cabal-install
            hlint
            hpack
            brittany
          ]));

      in {
        packages = { haskell-hello = pkgs.haskell-hello; };
        defaultPackage = pkgs.aoc;
        checks = self.packages;
        devShell = pkgs.devshell.mkShell {
          name = "AdventofCode";
          imports = [ (pkgs.devshell.extraModulesDir + "/git/hooks.nix") ];
          git.hooks.enable = true;
          git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
          packages = [ myHaskellEnv

            pkgs.ormolu
                       pkgs.treefmt pkgs.nixfmt ];
        };
      });
}
