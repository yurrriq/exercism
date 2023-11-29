{ ... }:

{
  perSystem = { config, pkgs, self', ... }: {
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
      ] ++ (with ocamlPackages; [
        base
        core
        dune_3
        findlib
        ocaml
        ocaml-lsp
        ocamlformat
        odoc
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
