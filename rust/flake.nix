{
  description = "exercism exercises in Rust";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
  };

  outputs = { self, emacs-overlay, flake-utils, nixpkgs }: {
    overlay = final: prev: {
      myEmacs = prev.emacsWithPackagesFromUsePackage {
        alwaysEnsure = true;
        config = ./emacs.el;
      };
    };
  } // flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      pkgs = import nixpkgs {
        overlays = [
          emacs-overlay.overlay
          self.overlay
        ];
        inherit system;
      };
    in
    {
      devShell = with pkgs; mkShell {
        FONTCONFIG_FILE = pkgs.makeFontsConf {
          fontDirectories = [ pkgs.iosevka ];
        };
        RUST_BACKTRACE = 1;
        buildInputs = with pkgs; [
          cargo
          clippy
          direnv
          exercism
          myEmacs
          nix-direnv
          nixpkgs-fmt
          pandoc
          rust-analyzer
          rustc
          rustfmt
        ];
      };
    }
  );
}
