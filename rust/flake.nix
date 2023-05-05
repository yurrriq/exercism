{
  description = "exercism exercises in Rust";

  inputs = {
    emacs-overlay = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:nix-community/emacs-overlay";
    };
    fenix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/fenix";
    };
    flake-utils.url = "github:numtide/flake-utils";
    naersk = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nmattia/naersk";
    };
    nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
  };

  outputs = { self, ... }@inputs:
    let
      inherit (inputs.nixpkgs) lib;
    in
    {
      overlay = lib.composeManyExtensions (lib.attrValues self.overlays);
      overlays = {
        myEmacs = _final: prev: {
          myEmacs = prev.emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          };
        };
      };
    } // inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import inputs.nixpkgs {
          overlays = [
            inputs.emacs-overlay.overlay
            inputs.fenix.overlays.default
            self.overlay
          ];
          inherit system;
        };
      in
      {
        devShells.default = with pkgs; mkShell {
          FONTCONFIG_FILE = pkgs.makeFontsConf {
            fontDirectories = [ pkgs.iosevka ];
          };
          RUST_BACKTRACE = 1;
          buildInputs = with pkgs; [
            exercism
            (
              fenix.complete.withComponents [
                "cargo"
                "clippy"
                "rust-src"
                "rustc"
                "rustfmt"
              ]
            )
            myEmacs
            nixpkgs-fmt
            rnix-lsp
            rust-analyzer-nightly
          ];
        };

        packages =
          {
            default = pkgs.symlinkJoin {
              name = "exercism-rust";
              paths =
                builtins.attrValues
                  (pkgs.lib.filterAttrs
                    (n: _: n != "default")
                    self.packages.${system});
            };
          } // (
            let
              inherit (builtins) attrNames filter listToAttrs pathExists readDir;
            in
            listToAttrs (
              map
                (pname: lib.nameValuePair pname (
                  inputs.naersk.lib.${system}.buildPackage {
                    inherit pname;
                    root = ./. + "/${pname}";
                    doCheck = true;
                  }
                )
                )
                (filter (name: pathExists (./. + ("/" + name + "/Cargo.toml")))
                  (attrNames (lib.filterAttrs (_: type: type == "directory") (readDir ./.))))
            )
          );
      }
    );
}
