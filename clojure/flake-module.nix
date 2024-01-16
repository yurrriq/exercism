{ ... }:

{
  perSystem = { pkgs, self', ... }: {
    devShells.clojure = pkgs.mkShell {
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
        clojure
        clojure-lsp
        leiningen
      ];
    };
  };
}
