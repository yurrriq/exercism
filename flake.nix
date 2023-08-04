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
    {
      overlays.haskell = _final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = _hfinal: hprev: {
            digits = hprev.callCabal2nix "digits"
              (prev.fetchFromGitHub {
                owner = "yurrriq";
                repo = "digits";
                rev = "c3a2c2bacc4a2e2c51beefa2fdb90da9a5bddf6b";
                hash = "sha256-/n2gf33zShj6LexHRplp975teCZyLAsg0rmXK9AHoK0=";
              })
              { };
          };
        };
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          overlays = [
            emacs-overlay.overlay
            self.overlays.haskell
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
          c = pkgs.mkShell {
            inputsFrom = [
              self.devShells.${system}.default
            ];
            nativeBuildInputs = with pkgs; [
              ccls
              doxygen
              (
                emacsWithPackagesFromUsePackage {
                  alwaysEnsure = true;
                  config = ./c/emacs.el;
                }
              )
              gcc
              indent
            ];
          };

          default = with pkgs; mkShell {
            FONTCONFIG_FILE = makeFontsConf {
              fontDirectories = [
                (nerdfonts.override { fonts = [ "Iosevka" ]; })
              ];
            };
            nativeBuildInputs = [
              exercism
              rnix-lsp
            ];
            inherit (self.checks.${system}.pre-commit-check) shellHook;
          };

          go = pkgs.mkShell {
            inputsFrom = [
              self.devShells.${system}.default
            ];
            nativeBuildInputs = with pkgs; [
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
            ];
          };

          haskell = pkgs.mkShell {
            inputsFrom = [
              self.devShells.${system}.default
            ];
            nativeBuildInputs = with pkgs; [
              (
                emacsWithPackagesFromUsePackage {
                  alwaysEnsure = true;
                  config = ./haskell/emacs.el;
                }
              )
              cabal-install
              ghc
              ghcid
              haskell-language-server
            ] ++ (with haskellPackages; [
              apply-refact
              cabal-plan
              hpack
              hlint
              ormolu
              pointfree
            ]);
          };

          jq = pkgs.mkShell {
            inputsFrom = [
              self.devShells.${system}.default
            ];
            nativeBuildInputs = with pkgs; [
              bats
              (
                emacsWithPackagesFromUsePackage {
                  alwaysEnsure = true;
                  config = ./jq/emacs.el;
                }
              )
              jq
            ];
          };

          purescript = pkgs.mkShell {
            inputsFrom = [
              self.devShells.${system}.default
            ];

            nativeBuildInputs = with pkgs; [
              dhall
              dhall-lsp-server
              (
                emacsWithPackagesFromUsePackage {
                  alwaysEnsure = true;
                  config = ./purescript/emacs.el;
                }
              )
              nodePackages.purescript-language-server
              nodejs_latest
              purescript
              spago
            ];
          };
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
            purs-tidy.enable = true;
            rufo.enable = true;
            rustfmt.enable = true;
          };
          settings.formatter = {
            clang-format = {
              excludes = [
                "**/test**"
              ];
            };
            purs-tidy = {
              includes = [
                "purescript/*/src/**/*.purs"
                "purescript/*/test/**/*.purs"
              ];
            };
          };
        };
      }
    );
}
