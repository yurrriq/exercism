{ inputs, ... }:

{
  perSystem = { pkgs, self', system, ... }: {
    devShells.julia = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];

      nativeBuildInputs = with pkgs; [
        julia
        nixpkgs-fmt
        (
          vscode-with-extensions.override {
            vscode = vscodium;
            vscodeExtensions = with inputs.nix-vscode-extensions.extensions.${system}.vscode-marketplace; [
              editorconfig.editorconfig
              jnoortheen.nix-ide
              julialang.language-julia
              mkhl.direnv
              tuttieee.emacs-mcx
            ];
          }
        )
      ];
    };

    treefmt.programs.prettier.excludes = [
      "julia/*/docs/build/**"
    ];
  };
}
