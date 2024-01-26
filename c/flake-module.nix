{ ... }:

{
  perSystem = { pkgs, self', ... }: {
    devShells.c = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        ccls
        doxygen
        (
          emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          }
        )
        gcc
        indent
        noweb
        # python3Packages.pygments
        (
          texlive.combine {
            inherit noweb;
            inherit (texlive) scheme-small
              # catchfile
              # datetime
              # dirtytalk
              # fmtcount
              # framed
              # fvextra
              # hardwrap
              # ifplatform
              latexmk
              # minted
              # titlesec
              # todonotes
              tufte-latex
              xetex
              # xstring
              ;
          }
        )
        # which
      ];
    };

    treefmt = {
      programs = {
        clang-format.enable = true;
      };
    };
  };
}
