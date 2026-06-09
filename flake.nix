{
  description = "FrogOS IR — Intermediate Representation types";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    systems.url = "github:nix-systems/default";

    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    systems,
    treefmt-nix,
    ...
  }:
    flake-utils.lib.eachSystem (import systems) (system: let
      pkgs = import nixpkgs {inherit system;};

      packageName = "frogos-ir";

      haskellPkgs = pkgs.haskellPackages;

      treefmtEval = treefmt-nix.lib.evalModule pkgs {
        projectRootFile = "flake.nix";

        programs.alejandra.enable = true;
        programs.fourmolu.enable = true;
        settings.formatter.fourmolu.options = ["--indentation=4"];
      };

      pkg = haskellPkgs.callCabal2nix packageName ./. {};
    in {
      packages.default = pkg;
      packages.${packageName} = pkg;

      checks = {
        ${packageName} = pkg;

        formatting = treefmtEval.config.build.check self;
      };

      formatter = treefmtEval.config.build.wrapper;

      devShells.default = haskellPkgs.shellFor {
        packages = p: [pkg];

        withHoogle = true;

        buildInputs = [
          haskellPkgs.haskell-language-server
          pkgs.cabal-install
          pkgs.hlint
          treefmtEval.config.build.wrapper
        ];
      };
    });
}
