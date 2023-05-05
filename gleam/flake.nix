{
  description = "exercism exercises in Gleam";

  inputs = {
    emacs-overlay = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:nix-community/emacs-overlay";
    };
    flake-utils.url = "github:numtide/flake-utils";
    gleam-mode = {
      flake = false;
      submodules = true;
      type = "git";
      url = "https://github.com/gleam-lang/gleam-mode";
    };
    nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
  };

  outputs = { self, ... }@inputs:
    let
      inherit (inputs.nixpkgs) lib;
    in
    {
      overlays = {
        default = lib.composeManyExtensions
          (lib.attrValues
            (lib.filterAttrs (n: _: n != "default") self.overlays));

        myEmacs = _final: prev: {
          myEmacs = prev.emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
            extraEmacsPackages = epkgs: [
              epkgs.tree-sitter
              epkgs.tree-sitter-indent
              self.packages.${prev.system}.gleam-mode
            ];
          };
        };
      };
    } // inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import inputs.nixpkgs {
          overlays = [
            inputs.emacs-overlay.overlay
            self.overlays.default
          ];
          inherit system;
        };
      in
      {
        devShells.default = with pkgs; mkShell {
          FONTCONFIG_FILE = pkgs.makeFontsConf {
            fontDirectories = [ pkgs.iosevka ];
          };
          buildInputs = with pkgs; [
            exercism
            erlang
            gleam
            myEmacs
            nixpkgs-fmt
            rebar3
            rnix-lsp
            tree-sitter
          ];
        };

        packages = {
          gleam-mode = pkgs.runCommand "gleam-mode" { } ''
            mkdir -p $out/share/emacs/site-lisp
            cp -r ${inputs.gleam-mode} $out/share/emacs/site-lisp/gleam-mode
          '';
        };
      });
}
