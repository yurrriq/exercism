{
  description = "Solutions to Prolog exercises from exercism.io";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, emacs-overlay, nixpkgs }:
    let
      pkgs = import nixpkgs {
        overlays = [
          emacs-overlay.overlay
          self.overlay
        ];
        system = "x86_64-linux";
      };
    in
    {
      overlay = final: prev: {
        myEmacs = prev.emacsWithPackagesFromUsePackage {
          alwaysEnsure = true;
          config = ./emacs.el;
        };
      };

      devShell.x86_64-linux = with pkgs; mkShell {
        FONTCONFIG_FILE = makeFontsConf {
          fontDirectories = [
            (nerdfonts.override { fonts = [ "Iosevka" ]; })
          ];
        };
        buildInputs = [
          exercism
          swiProlog
          myEmacs
          rnix-lsp
        ];
      };
    };
}
