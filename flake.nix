{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.treefmt-nix.flakeModule
      ];

      perSystem =
        {
          config,
          lib,
          pkgs,
          ...
        }:
        let
          sliip = pkgs.haskellPackages.developPackage {
            root = ./.;
            modifier = drv: pkgs.haskell.lib.addBuildTools drv nativeBuildInputs;
          };

          buildInputs = [ ];
          nativeBuildInputs = [
            pkgs.haskellPackages.cabal-install # Cabal build tool for Haskell
            pkgs.haskellPackages.haskell-language-server # Haskell LSP
            pkgs.nil # Nix LSP

            config.treefmt.build.wrapper # Treefmt CLI
          ];
        in
        {
          treefmt = {
            projectRootFile = ".git/config";

            # Nix
            programs.nixfmt.enable = true;

            # Haskell
            programs.ormolu.enable = true;
            programs.cabal-gild.enable = true;
            programs.hlint.enable = true;

            # GitHub Actions
            programs.actionlint.enable = true;

            # Markdown
            programs.mdformat.enable = true;

            # ShellScript
            programs.shellcheck.enable = true;
            programs.shfmt.enable = true;
          };

          packages = {
            inherit sliip;
            default = sliip;
          };

          devShells.default = pkgs.haskellPackages.shellFor {
            packages = hpkgs: [
              (hpkgs.callCabal2nix "sliip" ./. { })
            ];

            inherit nativeBuildInputs buildInputs;
          };
        };
    };
}
