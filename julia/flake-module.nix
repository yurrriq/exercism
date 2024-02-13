{ inputs, ... }:

{
  perSystem = { pkgs, self', system, ... }: {
    devShells.julia = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];

      nativeBuildInputs = with pkgs; [
        julia
        (
          vscode-with-extensions.override {
            vscode = vscodium;
            vscodeExtensions = (with vscode-extensions; [
              bbenoist.nix
            ]) ++ (with inputs.nix-vscode-extensions.extensions.${system}; [
              vscode-marketplace.editorconfig.editorconfig
              vscode-marketplace.julialang.language-julia
              vscode-marketplace.mkhl.direnv
              vscode-marketplace.tuttieee.emacs-mcx
            ]);
          }
        )
      ];
    };
  };
}
