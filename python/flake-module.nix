{ ... }:

{
  perSystem = { config, pkgs, self', ... }: {
    devShells.python = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        (
          emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          }
        )
        (
          python3.withPackages (ps: with ps; [
            pytest
            python-lsp-server
          ])
        )
      ];
    };

    treefmt = {
      programs = {
        black.enable = true;
      };
    };
  };
}
