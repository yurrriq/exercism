{ ... }:

{
  perSystem = { config, pkgs, self', system, ... }: {
    devShells.ocaml = pkgs.mkShell {
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
        ocamlformat
        # pkg-config
      ] ++ (with ocamlPackages; [
        base
        core
        dune_3
        findlib
        ocaml
        ocaml-lsp
        ounit
        utop
      ]);
    };

    treefmt = {
      programs = {
        ocamlformat.enable = true;
      };
    };
  };
}