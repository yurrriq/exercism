{ ... }:

{
  perSystem = { config, pkgs, self', ... }: {
    devShells.prolog = pkgs.mkShell {
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
        swi-prolog
        (
          texlive.combine {
            inherit (texlive) scheme-small
              # datetime
              dirtytalk
              latexmk
              plweb
              ;
          }
        )
      ];
    };
  };
}
