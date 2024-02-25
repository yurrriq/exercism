{ inputs, ... }:

{
  perSystem = { pkgs, self', ... }: {
    devShells.sml = pkgs.mkShell {
      FONTCONFIG_FILE = pkgs.makeFontsConf {
        fontDirectories = [
          (
            pkgs.nerdfonts.override {
              fonts = [ "FiraCode" ];
            }
          )
        ];
      };

      inputsFrom = [
        self'.devShells.default
      ];

      nativeBuildInputs = with pkgs; [
        (
          vscode-with-extensions.override {
            vscode = vscodium;
            vscodeExtensions = (with vscode-extensions; [
              bbenoist.nix
            ]) ++ (with inputs.nix-vscode-extensions.extensions.${system}; [
              vscode-marketplace.editorconfig.editorconfig
              vscode-marketplace.azdavis.millet
              vscode-marketplace.mkhl.direnv
              vscode-marketplace.tuttieee.emacs-mcx
            ]);
          }
        )
        millet
        polyml
        smlfmt
      ];
    };
  };
}
