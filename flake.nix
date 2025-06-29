{
  description = "Solutions to Exercism exercises";

  inputs = {
    emacs-overlay = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:nix-community/emacs-overlay";
    };
    fenix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/fenix";
    };
    flake-compat.url = "github:edolstra/flake-compat";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    gleam-mode = {
      flake = false;
      submodules = true;
      type = "git";
      url = "https://github.com/gleam-lang/gleam-mode";
    };
    naersk = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nmattia/naersk";
    };
    nix-vscode-extensions = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:nix-community/nix-vscode-extensions";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/release-24.05";
    pre-commit-hooks-nix = {
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:cachix/git-hooks.nix";
    };
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
  };

  outputs = inputs@{ flake-parts, nixpkgs, self, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports =
        let
          inherit (builtins) readDir;
          inherit (nixpkgs.lib) foldlAttrs pathIsRegularFile;
          collectFlakeModules = acc: name: type:
            let flakeModule = ./. + "/${name}/flake-module.nix"; in
            if type == "directory" && pathIsRegularFile flakeModule
            then
              [ flakeModule ] ++ acc
            else
              acc;
        in
        [
          inputs.pre-commit-hooks-nix.flakeModule
          inputs.treefmt-nix.flakeModule
        ] ++ foldlAttrs collectFlakeModules [ ] (builtins.readDir ./.);

      systems = [
        "x86_64-linux"
      ];

      flake = {
        overlays = {
          default =
            let
              inherit (nixpkgs) lib;
            in
            lib.composeManyExtensions
              (lib.attrValues
                (lib.filterAttrs (name: _: name != "default") self.overlays));

          iosevka-custom = _final: prev: {
            # https://typeof.net/Iosevka/customizer
            iosevka-custom = prev.iosevka.override {
              privateBuildPlan = ''
                [buildPlans.Iosevkacustom]
                family = "Iosevka Custom"
                spacing = "normal"
                serifs = "sans"
                exportGlyphNames = true
                [buildPlans.Iosevkacustom.weights.Regular]
                shape = 400
                menu = 400
                css = 400
                [buildPlans.Iosevkacustom.weights.Bold]
                shape = 700
                menu = 700
                css = 700
                [buildPlans.Iosevkacustom.slopes.Upright]
                angle = 0
                shape = "upright"
                menu = "upright"
                css = "normal"
                [buildPlans.Iosevkacustom.slopes.Italic]
                angle = 9.4
                shape = "italic"
                menu = "italic"
                css = "italic"
                [buildPlans.Iosevkacustom.ligations]
                inherits = "dlig"
                [buildPlans.Iosevkacustom.variants.design]
                lower-lambda = "curly-tailed-turn"
              '';
              set = "custom";
            };
          };
        };
      };

      perSystem = { config, pkgs, self', system, ... }: {
        _module.args.pkgs = import nixpkgs {
          overlays = [
            inputs.emacs-overlay.overlay
            inputs.fenix.overlays.default
            self.overlays.default
          ];
          inherit system;
        };

        devShells = {
          awk = pkgs.mkShell {
            inputsFrom = [
              self'.devShells.default
            ];
            nativeBuildInputs = with pkgs; [
              bats
              gawk
            ];
          };

          default = with pkgs; mkShell {
            FONTCONFIG_FILE = makeFontsConf {
              fontDirectories = [ iosevka-custom ];
            };
            inputsFrom = [
              config.pre-commit.devShell
            ];
            nativeBuildInputs = [
              exercism
              nixd
            ];
          };

          jq = pkgs.mkShell {
            inputsFrom = [
              self'.devShells.default
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
              self'.devShells.default
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
              # FIXME
              # spago
            ];
          };
        };

        pre-commit.settings.hooks = {
          treefmt.enable = true;
        };

        treefmt = {
          programs = {
            black.enable = true;
            clang-format.enable = true;
            deadnix.enable = true;
            nixpkgs-fmt.enable = true;
            # TODO: ocamlformat.enable = true;
            # FIXME: purs-tidy.enable = true;
            prettier.enable = true;
            rufo.enable = true;
            rustfmt.enable = true;
          };
          settings.formatter = {
            clang-format = {
              excludes = [
                "**/test**"
              ];
            };
            # purs-tidy = {
            #   includes = [
            #     "purescript/*/src/**/*.purs"
            #     # "purescript/*/test/**/*.purs"
            #   ];
            # };
          };
        };
      };
    };
}
