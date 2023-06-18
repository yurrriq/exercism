{
  description = "Solutions to Exercism exercises";

  inputs = {
    emacs-overlay = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:nix-community/emacs-overlay";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/release-23.05";
    pre-commit-hooks = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:cachix/pre-commit-hooks.nix";
    };
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
  };

  outputs = { self, emacs-overlay, flake-utils, nixpkgs, pre-commit-hooks, treefmt-nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          overlays = [
            emacs-overlay.overlay
          ];
          inherit system;
        };
      in
      {
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              revive.enable = true;
              treefmt.enable = true;
            };
            settings = {
              treefmt.package = self.formatter.${system};
            };
          };
        };

        devShells = {
          default = with pkgs; mkShell {
            FONTCONFIG_FILE = makeFontsConf {
              fontDirectories = [
                (nerdfonts.override { fonts = [ "Iosevka" ]; })
              ];
            };
            buildInputs = [
              exercism
              rnix-lsp
            ];
            inherit (self.checks.${system}.pre-commit-check) shellHook;
          };

          go = self.devShells.${system}.default.overrideAttrs (super: {
            buildInputs = super.buildInputs ++ (with pkgs; [
              (
                emacsWithPackagesFromUsePackage {
                  alwaysEnsure = true;
                  config = ./go/emacs.el;
                }
              )
              go
              gopls
              gotools
              revive
            ]);
          });
        };

        formatter = treefmt-nix.lib.mkWrapper pkgs {
          projectRootFile = "flake.nix";
          programs = {
            black.enable = true;
            clang-format.enable = true;
            deadnix.enable = true;
            erlfmt = {
              enable = true;
              print-width = 80;
            };
            gofmt.enable = true;
            hlint.enable = true;
            nixpkgs-fmt.enable = true;
            # TODO: ocamlformat.enable = true;
            ormolu = {
              enable = true;
              ghcOpts = [
                "OverloadedStrings"
                "TemplateHaskell"
              ];
            };
            rufo.enable = true;
            rustfmt.enable = true;
          };
          settings.formatter = {
            clang-format = {
              excludes = [
                "**/test**"
              ];
            };
          };
        };
      }
    );
}
