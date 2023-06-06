{
  description = "exercism exercises in Elixir";

  inputs = {
    emacs-overlay = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/emacs-overlay";
    };

    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
  };

  outputs = { self, ... }@inputs:
    let
      inherit (inputs.nixpkgs) lib;
    in
    {
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
            self.overlays.myEmacs
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
            elixir
            elixir_ls
            exercism
            myEmacs
            nixpkgs-fmt
            # rebar3
            rnix-lsp
          ];
        };

        packages =
          {
            default = pkgs.symlinkJoin {
              name = "exercism-elixir";
              paths =
                builtins.attrValues
                  (pkgs.lib.filterAttrs
                    (n: _: n != "default")
                    self.packages.${system});
            };
          };
      }
    );
}
