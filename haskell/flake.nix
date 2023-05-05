{
  description = "Solutions to Haskell exercises from exercism.io";

  inputs = {
    emacs-overlay = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/emacs-overlay";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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
      overlay = _final: prev: {
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
          cabal-install
          direnv
          exercism
          ghc
          ghcid
          haskell-language-server
          haskellPackages.cabal-plan
          haskellPackages.hpack
          haskellPackages.hlint
          haskellPackages.ormolu
          haskellPackages.pointfree
          myEmacs
          nix-direnv
          rnix-lsp
        ];
      };
    };
}
