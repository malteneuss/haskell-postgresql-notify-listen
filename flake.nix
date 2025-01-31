{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ]; # import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.fourmolu-nix.flakeModule
      ];
      flake = {
        nixosModules.homefinder = import ./flake-nixosmodule.nix;
      };
      perSystem = { self', system, lib, config, pkgs, ... }: {
        # Our only Haskell project. You can have multiple projects, but this template
        # has only one.
        # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
        haskellProjects.default = {

          # Packages to add on top of `basePackages`
          packages = {
            # Add source or Hackage overrides here
            # (Local packages are added automatically)
            # aeson.source = "1.5.0.0" # Hackage version
            # shower.source = inputs.shower; # Flake input
            # broken in nixpkgs, force to use openai-hs


            # openai-hs = {
            #   source = "0.3.0.1";
            #   doCheck = false;
            # };
            tls = {
              source = "2.0.6";
              # doCheck = false;
            };
            tls-session-manager = {
              source = "0.0.6";
              # doCheck = false;
            };


          };

          # Add your package overrides here
          settings = {
            /*
            haskell-template = {
              haddock = false;
            };
            aeson = {
              check = false;
            };
            */
            openai-hs = {
              # doCheck = false;
              broken = false;

            };
          };

          # Development shell configuration
          devShell = {
            hlsCheck.enable = false;
          };

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          #programs.hlint.enable = true;
        };

        # Default shell.
        devShells.default = pkgs.mkShell rec {
          name = "haskell-template";
          meta.description = "Haskell development environment";
          # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
          ];
          nativeBuildInputs = with pkgs; [
            just
            # needed for command line tools like pg_config and psql
            postgresql_15
            # manage database schema and migrations
            dbmate
            zlib
          ];
          # Ensure that libz.so and other libraries are available to TH
          # splices, cabal repl, etc.
          LD_LIBRARY_PATH = lib.makeLibraryPath nativeBuildInputs;
        };
      };
    };
}
