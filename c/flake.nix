{
  description = "Solutions to Exercism exercises in C";

  inputs = {
    emacs-overlay = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:nix-community/emacs-overlay";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, emacs-overlay, flake-utils, nixpkgs }: {
    overlays = {
      myEmacs = final: prev: {
        myEmacs = prev.emacsWithPackagesFromUsePackage {
          alwaysEnsure = true;
          config = ./emacs.el;
        };
      };
    };
  } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        overlays = [
          emacs-overlay.overlay
          self.overlays.myEmacs
        ];
        inherit system;
      };
    in
    {
      devShell = with pkgs; mkShell {
        buildInputs = [
          ccls
          exercism
          gcc
          indent
          myEmacs
          nixpkgs-fmt
          rnix-lsp
        ];
      };
    }
  );
}
